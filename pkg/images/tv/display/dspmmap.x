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
	if (streq (fname, "BPM")) {
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

short	val
int	i, ndim, npix
pointer	sp, v1, v2, im_in, im_out, pm, mw, data

int	imgnls()
pointer immap(), pm_newmask(), im_pmmapo(), imgl1i(), mw_openim()
errchk	immap, mw_openim

begin
	call smark (sp)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)

	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)

	im_in = immap (pmname, READ_ONLY, 0)
	pm = pm_newmask (im_in, 16)

	ndim = IM_NDIM(im_in)
	npix = IM_LEN(im_in,1)

	while (imgnls (im_in, data, Meml[v1]) != EOF) {
	    do i = 0, npix-1 {
		val = Mems[data+i]
		if (val < 0)
		   Mems[data+i] = 0
	    }
	    call pmplps (pm, Meml[v2], Mems[data], 0, npix, PIX_SRC)
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
# coordinates of the reference image.  A null input returns a null output.

procedure ds_match (im, refim)

pointer	im			#U Pixel mask image pointer
pointer	refim			#I Reference image pointer

int	i, j, k, nc, nl, ncpm, nlpm, c1, c2, l1, l2, nref, npm
int	steptype, xoffset, xstep, yoffset, ystep
real	x1, x2, y1, y2
long	vold[IM_MAXDIM], vnew[IM_MAXDIM]
pointer	mwref, mwpm, ctref, ctpm, pm, pmnew, imnew, bufref, bufpm

int	imstati()
pointer	pm_open(), mw_openim(), im_pmmapo(), imgl1i(), mw_sctran()
bool	pm_empty(), pm_linenotempty()
errchk	pm_open, mw_openim

begin
	if (im == NULL)
	    return

	# Set sizes.
	pm = imstati (im, IM_PMDES)
	nc = IM_LEN(refim,1)
	nl = IM_LEN(refim,2)
	ncpm = IM_LEN(im,1)
	nlpm = IM_LEN(im,2)

	# Check if the two are the same logical size and the mask is empty.
	if (nc == ncpm && nl == nlpm && pm_empty (pm))
	    return

	# Check coordinate transformations.
	mwref = mw_openim (refim)
	mwpm = mw_openim (im)

	steptype = 1
	ctref = mw_sctran (mwref, "logical", "physical", 3)
	ctpm = mw_sctran (mwpm, "physical", "logical", 3)
	call mw_c2tranr (ctref, 1., 1., x1, y1) 
	call mw_c2tranr (ctpm, x1, y1, x1, y1) 
	call mw_c2tranr (ctref, 2., 1., x2, y2) 
	call mw_c2tranr (ctpm, x2, y2, x2, y2) 
	if (abs(x2-x1) < 1.) {
	    steptype = 2
	    call mw_ctfree (ctref)
	    call mw_ctfree (ctpm)
	    ctref = mw_sctran (mwref, "physical", "logical", 3)
	    ctpm = mw_sctran (mwpm, "logical", "physical", 3)
	    call mw_c2tranr (ctpm, 1., 1., x1, y1) 
	    call mw_c2tranr (ctref, x1, y1, x1, y1) 
	    call mw_c2tranr (ctpm, 2., 1., x2, y2) 
	    call mw_c2tranr (ctref, x2, y2, x2, y2) 
	}
	x2 = x2 - x1
	if (abs(y1-y2) > EPSILONR)
	    call error (0, "Image and mask have a relative rotation")
	if (abs(x1-nint(x1)) > EPSILONR  && abs(x1-nint(x1))-0.5 > EPSILONR)
	    call error (0, "Image and mask have non-integer relative offsets")
	if (abs(x2-nint(x2)) > EPSILONR)
	    call error (0, "Image and mask have non-integer relative steps")
	xoffset = nint (x1 - 1.)
	xstep = nint (x2)

	if (steptype == 1) {
	    call mw_c2tranr (ctref, 1., 1., x1, y1) 
	    call mw_c2tranr (ctpm, x1, y1, x1, y1) 
	    call mw_c2tranr (ctref, 1., 2., x2, y2) 
	    call mw_c2tranr (ctpm, x2, y2, x2, y2) 
	} else {
	    call mw_c2tranr (ctpm, 1., 1., x1, y1) 
	    call mw_c2tranr (ctref, x1, y1, x1, y1) 
	    call mw_c2tranr (ctpm, 1., 2., x2, y2) 
	    call mw_c2tranr (ctref, x2, y2, x2, y2) 
	}
	y2 = y2 - y1
	if (abs(x1-x2) > EPSILONR)
	    call error (0, "Image and mask have a relative rotation")
	if (abs(y1-nint(y1)) > EPSILONR  && abs(y1-nint(y1))-0.5 > EPSILONR)
	    call error (0, "Image and mask have non-integer relative offsets")
	if (abs(y2-nint(y2)) > EPSILONR)
	    call error (0, "Image and mask have non-integer relative steps")
	yoffset = nint (y1 - 1.)
	ystep = nint (y2)

	call mw_ctfree (ctref)
	call mw_ctfree (ctpm)
	call mw_close (mwref)
	call mw_close (mwpm)

	# Check if the two have the same coordinate system.
	if (nc==ncpm && nl==nlpm && xoffset==0 && yoffset==0 && xstep==ystep)
	    return

	# Create a new pixel mask of the required size and offset.
	pmnew = pm_open (NULL)
	call pm_ssize (pmnew, 2, IM_LEN(refim,1), 16)
	imnew = im_pmmapo (pmnew, NULL)
	bufref = imgl1i (imnew)

	if (steptype == 1) {
	    c1 = 1 + xoffset + max (0, (xstep - 1 - xoffset) / xstep) * xstep
	    c2 = 1 + xoffset + min (nc-1, (ncpm - 1 - xoffset) / xstep) * xstep
	    l1 = 1 + yoffset + max (0, (ystep - 1 - yoffset) / ystep) * ystep
	    l2 = 1 + yoffset + min (nl-1, (nlpm - 1 - yoffset) / ystep) * ystep
	    npm = c2 - c1 + 1
	    nref = npm / xstep
	    if (nref > 0) {
		call malloc (bufpm, npm, TY_SHORT)
		call malloc (bufref, nref, TY_SHORT)
		call amovkl (long(1), vold, IM_MAXDIM)
		call amovkl (long(1), vnew, IM_MAXDIM)
		vold[1] = c1
		vnew[1] = c1 - xoffset
		do i = l1, l2, ystep {
		    vold[2] = i
		    if (!pm_linenotempty (pm, vold))
			next
		    call pmglps (pm, vold, Mems[bufpm], 0, npm, 0)
		    vnew[2] = l1 - yoffset + (i - l1) / ystep
		    j = 0
		    do k = 0, npm-1, xstep {
			Mems[bufref+j] = Mems[bufpm+k]
			j = j + 1
		    }
		    call pmplps (pmnew, vnew, Mems[bufref], 0, nref, PIX_SRC)
		}
	    }
	} else {
	    c1 = max (1, 1 - xoffset)
	    c2 = min (ncpm, nc / xstep - xoffset)
	    l1 = max (1, 1 - yoffset)
	    l2 = min (nlpm, nl / ystep - yoffset)
	    npm = c2 - c1 + 1
	    nref = npm * xstep
	    if (nref > 0) {
		call malloc (bufpm, npm, TY_SHORT)
		call malloc (bufref, nref, TY_SHORT)
		call amovkl (long(1), vold, IM_MAXDIM)
		call amovkl (long(1), vnew, IM_MAXDIM)
		vold[1] = c1
		vnew[1] = c1 + xoffset
		do i = l1, l2 {
		    vold[2] = i
		    if (!pm_linenotempty (pm, vold))
			next
		    call pmglps (pm, vold, Mems[bufpm], 0, npm, 0)
		    call aclrs (Mems[bufref], nref)
		    do j = 0, npm-1 {
			k = j * xstep
			Mems[bufref+k] = Mems[bufpm+j]
		    }
		    vnew[2] = l1 + yoffset + (i - l1) * ystep
		    call pmplps (pmnew, vnew, Mems[bufref], 0, nref, PIX_SRC)
		}
	    }
	    call mfree (bufpm, TY_SHORT)
	    call mfree (bufref, TY_SHORT)
	}

	# Update the IMIO descriptor.
	call imunmap (im)
	im = imnew
	call imseti (im, IM_PMDES, pmnew)
end
