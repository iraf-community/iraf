include	<mach.h>
include	<ctype.h>
include	<error.h>
include	<imhdr.h>
include	<imset.h>
include	<pmset.h>
include	<syserr.h>


# DS_PMMAP -- Open a pixel mask READ_ONLY.
#
# Open the pixel mask.  If a regular image is specified convert it to
# a pixel mask.  Match the mask to the reference image based on the
# physical coordinates.  A null filename is allowed and returns NULL.

pointer procedure ds_pmmap (pmname, refim)

char	pmname[ARB]		#I Pixel mask name
pointer	refim			#I Reference image pointer

pointer	im
char	fname[SZ_FNAME]
int	nowhite(), errcode()
bool	streq()
pointer	im_pmmap(), ds_pmimmap()
errchk	ds_pmimmap, ds_match

begin
	if (nowhite (pmname, fname, SZ_FNAME) == 0)
	    return (NULL)
	if (streq (fname, "EMPTY"))
	    return (NULL)
	if (fname[1] == '!') {
	    iferr (call imgstr (refim, fname[2], fname, SZ_FNAME))
		fname[1] = EOS
	} else if (streq (fname, "BPM")) {
	    iferr (call imgstr (refim, "BPM", fname, SZ_FNAME))
		return (NULL)
	}

	iferr (im = im_pmmap (fname, READ_ONLY, NULL)) {
	    switch (errcode()) {
	    case SYS_FOPNNEXFIL, SYS_PLBADSAVEF:
		im = ds_pmimmap (fname, refim)
	    default:
		call erract (EA_ERROR)
	    }
	}

	iferr (call ds_match (im, refim))
	    call erract (EA_WARN)

	return (im)
end


# DS_PMIMMAP -- Open a pixel mask from a non-pixel list image.
# Return error if the image cannot be opened.

pointer procedure ds_pmimmap (pmname, refim)

char	pmname[ARB]		#I Image name
pointer	refim			#I Reference image pointer

int	i, ndim, npix, val
pointer	sp, v1, v2, im_in, im_out, pm, mw, data

int	imgnli()
pointer immap(), pm_newmask(), im_pmmapo(), imgl1i(), mw_openim()
errchk	immap, mw_openim

begin
	call smark (sp)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)

	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)

	im_in = immap (pmname, READ_ONLY, 0)
	pm = pm_newmask (im_in, 27)

	ndim = IM_NDIM(im_in)
	npix = IM_LEN(im_in,1)

	while (imgnli (im_in, data, Meml[v1]) != EOF) {
	    do i = 0, npix-1 {
		val = Memi[data+i]
		if (val < 0)
		   Memi[data+i] = 0
	    }
	    call pmplpi (pm, Meml[v2], Memi[data], 0, npix, PIX_SRC)
	    call amovl (Meml[v1], Meml[v2], ndim)
	}

	im_out = im_pmmapo (pm, im_in)
	data = imgl1i (im_out)		# Force I/O to set header
	mw = mw_openim (im_in)		# Set WCS
	call mw_saveim (mw, im_out)
	call mw_close (mw)

	call imunmap (im_in)
	call sfree (sp)
	return (im_out)
end


# DS_MATCH -- Set the pixel mask to match the reference image.
# This matches sizes and physical coordinates and allows the
# original mask to be smaller or larger than the reference image.
# Subsequent use of the pixel mask can then work in the logical
# coordinates of the reference image.  The mask values are the maximum
# of the mask values which overlap each reference image pixel.
# A null input returns a null output.

procedure ds_match (im, refim)

pointer	im			#U Pixel mask image pointer
pointer	refim			#I Reference image pointer

int	i, j, k, l, i1, i2, j1, j2, nc, nl, ncpm, nlpm, nx, val
double	x1, x2, y1, y2, lt[6], lt1[6], lt2[6]
long	vold[IM_MAXDIM], vnew[IM_MAXDIM]
pointer	pm, pmnew, imnew, mw, ctx, cty, bufref, bufpm

int	imstati()
pointer	pm_open(), mw_openim(), im_pmmapo(), imgl1i(), mw_sctran()
bool	pm_empty(), pm_linenotempty()
errchk	pm_open, mw_openim

begin
	if (im == NULL)
	    return

	# Set sizes.
	nc = IM_LEN(refim,1)
	nl = IM_LEN(refim,2)
	ncpm = IM_LEN(im,1)
	nlpm = IM_LEN(im,2)

	# If the mask is empty and the sizes are the same then it does not
	# matter if the two are actually matched in physical coordinates.
	pm = imstati (im, IM_PMDES)
	if (pm_empty(pm) && nc == ncpm && nl == nlpm)
	    return

	# Compute transformation between reference (logical) coordinates
	# and mask (physical) coordinates.

	mw = mw_openim (im)
	call mw_gltermd (mw, lt, lt[5], 2)
	call mw_close (mw)

	mw = mw_openim (refim)
	call mw_gltermd (mw, lt2, lt2[5], 2)
	call mw_close (mw)

	# Combine lterms.
	call mw_invertd (lt, lt1, 2)
	call mw_mmuld (lt1, lt2, lt, 2)
	call mw_vmuld (lt, lt[5], lt[5], 2)
	lt[5] = lt2[5] - lt[5]
	lt[6] = lt2[6] - lt[6]
	do i = 1, 6
	    lt[i] = nint (1D6 * (lt[i]-int(lt[i]))) / 1D6 + int(lt[i])

	# Check for a rotation.  For now don't allow any rotation.
	if (lt[2] != 0. || lt[3] != 0.)
	    call error (1, "Image and mask have a relative rotation")
	
	# Check for an exact match.
	if (lt[1] == 1D0 && lt[4] == 1D0 && lt[5] == 0D0 && lt[6] == 0D0)
	    return

	# Set reference to mask coordinates.
	mw = mw_openim (im)
	call mw_sltermd (mw, lt, lt[5], 2)
	ctx = mw_sctran (mw, "logical", "physical", 1)
	cty = mw_sctran (mw, "logical", "physical", 2)

	# Create a new pixel mask of the required size and offset.
	# Do dummy image I/O to set the header.
	pmnew = pm_open (NULL)
	call pm_ssize (pmnew, 2, IM_LEN(refim,1), 27)
	imnew = im_pmmapo (pmnew, NULL)
	bufref = imgl1i (imnew)

	# Compute region of mask overlapping the reference image.
	call mw_ctrand (ctx, 1-0.5D0, x1, 1)
	call mw_ctrand (ctx, nc+0.5D0, x2, 1)
	i1 = max (1, nint(min(x1,x2)+1D-5))
	i2 = min (ncpm, nint(max(x1,x2)-1D-5))
	call mw_ctrand (cty, 1-0.5D0, y1, 1)
	call mw_ctrand (cty, nl+0.5D0, y2, 1)
	j1 = max (1, nint(min(y1,y2)+1D-5))
	j2 = min (nlpm, nint(max(y1,y2)-1D-5))

	# Set the new mask values to the maximum of all mask values falling
	# within each reference pixel in the overlap region.
	if (i1 <= i2 && j1 <= j2) {
	    nx = i2 - i1 + 1
	    call malloc (bufpm, nx, TY_INT)
	    call malloc (bufref, nc, TY_INT)
	    vold[1] = i1
	    vnew[1] = 1
	    do j = 1, nl {
		call mw_ctrand (cty, j-0.5D0, y1, 1)
		call mw_ctrand (cty, j+0.5D0, y2, 1)
		j1 = max (1, nint(min(y1,y2)+1D-5))
		j2 = min (nlpm, nint(max(y1,y2)-1D-5))
		if (j2 < j1)
		    next

		vnew[2] = j
		call aclri (Memi[bufref], nc)
		do l = j1, j2 {
		    vold[2] = l
		    if (!pm_linenotempty (pm, vold))
			next
		    call pmglpi (pm, vold, Memi[bufpm], 0, nx, 0)
		    do i = 1, nc {
			call mw_ctrand (ctx, i-0.5D0, x1, 1)
			call mw_ctrand (ctx, i+0.5D0, x2, 1)
			i1 = max (1, nint(min(x1,x2)+1D-5))
			i2 = min (ncpm, nint(max(x1,x2)-1D-5))
			if (i2 < i1)
			    next
			val = Memi[bufref+i-1]
			do k = i1-vold[1], i2-vold[1]
			    val = max (val, Memi[bufpm+k])
			Memi[bufref+i-1] = val
		    }
		}
		call pmplpi (pmnew, vnew, Memi[bufref], 0, nc, PIX_SRC)
	    }
	    call mfree (bufref, TY_INT)
	    call mfree (bufpm, TY_INT)
	}

	call mw_close (mw)
	call imunmap (im)
	im = imnew
	call imseti (im, IM_PMDES, pmnew)
end
