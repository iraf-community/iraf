include	<mach.h>
include	<ctype.h>
include	<error.h>
include	<imhdr.h>
include	<imset.h>
include	<pmset.h>
include	<mwset.h>
include	<syserr.h>
include	<math/iminterp.h>

# Pixel mask matching options.
define	PM_MATCH	"|logical|physical|world|offset|"
define	PM_LOGICAL	1		# Match in logical coordinates
define	PM_PHYSICAL	2		# Match in physical coordinates
define	PM_WORLD	3		# Match in world coordinates
define	PM_OFFSET	4		# Match in physical with WCS offset


# XT_PMMAP/XT_MAPPM -- Open a pixel mask READ_ONLY.
#
# This routine maps multiple types of mask files and designations.
# It may match the mask coordinates to the reference image based on the
# physical coordinate system so the mask may be of a different size.
# The mask name is returned so that the task has the name pointed to by "BPM".
# A null filename is allowed and returns NULL.
#
# Modified to use xt_maskname with the reference image extension name.
# Minor bug fixes in xt_match.

pointer procedure yt_pmmap (pmname, refim, mname, sz_mname)

char	pmname[ARB]		#I Pixel mask name
pointer	refim			#I Reference image pointer
char	mname[ARB]		#O Expanded mask name
int	sz_mname		#O Size of expanded mask name

pointer	yt_mappm()
errchk	yt_mappm

begin
	return (yt_mappm (pmname, refim, "physical", mname, sz_mname))
end

pointer procedure yt_mappm (pmname, refim, match, mname, sz_mname)

char	pmname[ARB]		#I Pixel mask name
pointer	refim			#I Reference image pointer
char	match[ARB]		#I Match by physical coordinates?
char	mname[ARB]		#O Expanded mask name
int	sz_mname		#O Size of expanded mask name

int	i, j, flag, nowhite()
pointer	sp, fname, extname, im, ref, yt_pmmap1()
bool	streq()
errchk	yt_pmmap1

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (extname, SZ_FNAME, TY_CHAR)

	im = NULL
	i = nowhite (pmname, Memc[fname], SZ_FNAME)

	# Process invert flags.  These occur more than once.
	j = 0; flag = 0
	for (i=0; Memc[fname+i]!=EOS; i=i+1) {
	    if (Memc[fname+i] == '^')
	        flag = flag + 1
	    else {
	        Memc[fname+j] = Memc[fname+i]
		j = j + 1
	    }
	}
	Memc[fname+j] = EOS
	if (mod (flag, 2) == 0)
	    flag = 0
	else
	    flag = INVERT_MASK


	# Resolve keyword references.
	if (Memc[fname] == '!') {
	    iferr (call imgstr (refim, Memc[fname+1], Memc[fname], SZ_FNAME))
		Memc[fname] = EOS
	} else if (streq (Memc[fname], "BPM")) {
	    iferr (call imgstr (refim, "BPM", Memc[fname], SZ_FNAME))
		Memc[fname] = EOS
	}

	# Resolve other special names.
	if (streq (Memc[fname], "EMPTY"))
	    ref = refim
	else
	    ref = NULL

	# Create the mask.
	if (Memc[fname] != EOS) {
	    iferr (im = yt_pmmap1 (Memc[fname], ref, refim, flag, match)) {
	        ifnoerr (call imgstr (refim, "extname", Memc[extname],
		    SZ_FNAME)) {
		    call xt_maskname (Memc[fname], Memc[extname], READ_ONLY,
		        Memc[fname], SZ_FNAME)
		    im = yt_pmmap1 (Memc[fname], ref, refim, flag, match)
		} else
		    im = yt_pmmap1 (Memc[fname], ref, refim, flag, match)
	    }
	}
	call strcpy (Memc[fname], mname, sz_mname)

	call sfree (sp)
	return (im)
end


# XT_PMUNMAP -- Unmap a mask image.
# Note that the imio pointer may be purely an internal pointer opened
# with im_pmmapo so we need to free the pl pointer explicitly.

procedure yt_pmunmap (im)

pointer	im			#I IMIO pointer for mask

pointer	pm
int	imstati()

begin
	pm = imstati (im, IM_PMDES)
	call pm_close (pm)
	call imseti (im, IM_PMDES, NULL)
	call imunmap (im)
end


# XT_PMMAP1 -- Open a pixel mask READ_ONLY.  The input mask may be
# a pixel list image, a non-pixel list image, or a text file.
# Return error if the pixel mask cannot be opened.  For pixel masks
# or image masks possibly match the WCS.

pointer procedure yt_pmmap1 (pmname, ref, refim, flag, match)

char	pmname[ARB]		#I Pixel mask name
pointer	ref			#I Reference image for pixel mask
pointer	refim			#I Reference image for image or text
int	flag			#I Mask flag
char	match[ARB]		#I Match by physical coordinates?

int	imstati(),  errcode()
pointer	im, pm
pointer	im_pmmap(), yt_pmimmap(), yt_pmtext(), yt_pmsection()
bool	streq()
errchk	yt_match

begin
	im = NULL

	if (streq (pmname, "STDIN"))
	    im = yt_pmtext (pmname, refim, flag)

	else if (pmname[1] == '[')
	    im = yt_pmsection (pmname, refim, flag)

	else {
	    ifnoerr (im = im_pmmap (pmname, READ_ONLY, ref)) {
		call yt_match (im, refim, match)
		if (flag == INVERT_MASK) {
		    pm = imstati (im, IM_PMDES)
		    call yt_pminvert (pm)
		    call imseti (im, IM_PMDES, pm)
		}
	    } else {
		switch (errcode()) {
		case SYS_IKIOPEN, SYS_FOPNNEXFIL, SYS_PLBADSAVEF, SYS_FOPEN:
		    ifnoerr (im = yt_pmimmap (pmname, refim, flag))
			call yt_match (im, refim, match)
		    else {
			switch (errcode()) {
			case SYS_IKIOPEN:
			    im = yt_pmtext (pmname, refim, flag)
			default:
			    call erract (EA_ERROR)
			}
		    }
		default:
		    call erract (EA_ERROR)
		}
	    }
	}

	return (im)
end


# XT_PMIMMAP -- Open a pixel mask from a non-pixel list image.
# Return error if the image cannot be opened.

pointer procedure yt_pmimmap (pmname, refim, flag)

char	pmname[ARB]		#I Image name
pointer	refim			#I Reference image pointer
int	flag			#I Mask flag

int	i, ndim, npix, rop, val
pointer	sp, v1, v2, im_in, im_out, pm, mw, data

int	imstati(), imgnli()
pointer immap(), pm_newmask(), im_pmmapo(), imgl1i(), mw_openim()
errchk	immap, mw_openim, im_pmmapo

begin
	call smark (sp)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)

	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)

	im_in = immap (pmname, READ_ONLY, 0)
	pm = imstati (im_in, IM_PMDES)
	if (pm != NULL)
	    return (im_in)
	pm = pm_newmask (im_in, 16)

	ndim = IM_NDIM(im_in)
	npix = IM_LEN(im_in,1)

	if (flag == INVERT_MASK)
	    rop = PIX_NOT(PIX_SRC)
	else
	    rop = PIX_SRC

	while (imgnli (im_in, data, Meml[v1]) != EOF) {
	    if (flag == INVERT_MASK) {
		do i = 0, npix-1 {
		    val = Memi[data+i]
		    if (val <= 0)
		       Memi[data+i] = 1
		    else
		       Memi[data+i] = 0
		}
	    } else {
		do i = 0, npix-1 {
		    val = Memi[data+i]
		    if (val < 0)
		       Memi[data+i] = 0
		}
	    }
	    call pmplpi (pm, Meml[v2], Memi[data], 0, npix, rop)
	    call amovl (Meml[v1], Meml[v2], ndim)
	}

	im_out = im_pmmapo (pm, im_in)
	data = imgl1i (im_out)		# Force I/O to set header
	mw = mw_openim (im_in)		# Set WCS
	call mw_saveim (mw, im_out)
	call mw_close (mw)

	#call imunmap (im_in)
	call yt_pmunmap (im_in)
	call sfree (sp)
	return (im_out)
end


# XT_PMTEXT -- Create a pixel mask from a text file of rectangles.
# Return error if the file cannot be opened.
# This routine only applies to the first 2D plane.

pointer procedure yt_pmtext (pmname, refim, flag)

char	pmname[ARB]		#I Image name
pointer	refim			#I Reference image pointer
int	flag			#I Mask flag

int	fd, nc, nl, c1, c2, l1, l2, nc1, nl1, rop
pointer	pm, im, mw, dummy

int	open(), fscan(), nscan()
pointer	pm_newmask(), im_pmmapo(), imgl1i(), mw_openim()
errchk	open,im_pmmapo

begin
	fd = open (pmname, READ_ONLY, TEXT_FILE)
	pm = pm_newmask (refim, 16)

	nc = IM_LEN(refim,1)
	nl = IM_LEN(refim,2)

	if (flag == INVERT_MASK)
	    call pl_box (pm, 1, 1, nc, nl, PIX_SET+PIX_VALUE(1))

	while (fscan (fd) != EOF) {
	    call gargi (c1)
	    call gargi (c2)
	    call gargi (l1)
	    call gargi (l2)
	    if (nscan() != 4) {
		if (nscan() == 2) {
		    l1 = c2
		    c2 = c1
		    l2 = l1
		} else
		    next
	    }

	    c1 = max (1, c1)
	    c2 = min (nc, c2)
	    l1 = max (1, l1)
	    l2 = min (nl, l2)
	    nc1 = c2 - c1 + 1
	    nl1 = l2 - l1 + 1
	    if (nc1 < 1 || nl1 < 1)
		next

	    # Select mask value based on shape of rectangle.
	    if (flag == INVERT_MASK)
		rop = PIX_CLR
	    else if (nc1 <= nl1)
		rop = PIX_SET+PIX_VALUE(2)
	    else
		rop = PIX_SET+PIX_VALUE(3)

	    # Set mask rectangle.
	    call pm_box (pm, c1, l1, c2, l2, rop)
	}

	call close (fd)
	im = im_pmmapo (pm, refim)
	dummy = imgl1i (im)		# Force I/O to set header
	mw = mw_openim (refim)		# Set WCS
	call mw_saveim (mw, im)
	call mw_close (mw)

	return (im)
end


# XT_PMSECTION -- Create a pixel mask from an image section.
# This only applies the mask to the first plane of the image.

pointer procedure yt_pmsection (section, refim, flag)

char	section[ARB]		#I Image section
pointer	refim			#I Reference image pointer
int	flag			#I Mask flag

int	i, j, ip, temp, a[2], b[2], c[2], rop, ctoi()
pointer	pm, im, mw, dummy, pm_newmask(), im_pmmapo(), imgl1i(), mw_openim()
errchk	im_pmmapo
define  error_  99

begin
	# This is currently only for 1D and 2D images.
	if (IM_NDIM(refim) > 2)
	    call error (1, "Image sections only allowed for 1D and 2D images")

        # Decode the section string.
	call amovki (1, a, 2)
	call amovki (1, b, 2)
	call amovki (1, c, 2)
	do i = 1, IM_NDIM(refim)
	    b[i] = IM_LEN(refim,i)

        ip = 1
        while (IS_WHITE(section[ip]))
            ip = ip + 1
        if (section[ip] == '[') {
            ip = ip + 1

	    do i = 1, IM_NDIM(refim) {
		while (IS_WHITE(section[ip]))
		    ip = ip + 1

		# Get a:b:c.  Allow notation such as "-*:c"
		# (or even "-:c") where the step is obviously negative.

		if (ctoi (section, ip, temp) > 0) {                 # a
		    a[i] = temp
		    if (section[ip] == ':') {
			ip = ip + 1
			if (ctoi (section, ip, b[i]) == 0)             # a:b
			    goto error_
		    } else
			b[i] = a[i]
		} else if (section[ip] == '-') {                    # -*
		    temp = a[i]
		    a[i] = b[i]
		    b[i] = temp
		    ip = ip + 1
		    if (section[ip] == '*')
			ip = ip + 1
		} else if (section[ip] == '*')                      # *
		    ip = ip + 1
		if (section[ip] == ':') {                           # ..:step
		    ip = ip + 1
		    if (ctoi (section, ip, c[i]) == 0)
			goto error_
		    else if (c[i] == 0)
			goto error_
		}
		if (a[i] > b[i] && c[i] > 0)
		    c[i] = -c[i]

		while (IS_WHITE(section[ip]))
		    ip = ip + 1
		if (i < IM_NDIM(refim)) {
		    if (section[ip] != ',')
			goto error_
		} else {
		    if (section[ip] != ']')
			goto error_
		}
		ip = ip + 1
	    }
	}

	# In this case make the values be increasing only.
	do i = 1, IM_NDIM(refim)
	    if (c[i] < 0) {
		temp = a[i]
		a[i] = b[i]
		b[i] = temp
		c[i] = -c[i]
	    }

	# Make the mask.
	pm = pm_newmask (refim, 16)

	if (flag == INVERT_MASK) {
	    rop = PIX_SET+PIX_VALUE(1)
	    call pm_box (pm, 1, 1, IM_LEN(refim,1), IM_LEN(refim,2), rop)
	    rop = PIX_CLR
	} else
	    rop = PIX_SET+PIX_VALUE(1)

	if (c[1] == 1 && c[2] == 1)
	    call pm_box (pm, a[1], a[2], b[1], b[2], rop)

	else if (c[1] == 1)
	    for (i=a[2]; i<=b[2]; i=i+c[2])
		call pm_box (pm, a[1], i, b[1], i, rop)

	else
	    for (i=a[2]; i<=b[2]; i=i+c[2])
		for (j=a[1]; j<=b[1]; j=j+c[1])
		    call pm_point (pm, j, i, rop)

	im = im_pmmapo (pm, refim)
	dummy = imgl1i (im)		# Force I/O to set header
	mw = mw_openim (refim)		# Set WCS
	call mw_saveim (mw, im)
	call mw_close (mw)

	return (im)

error_
        call error (1, "Error in image section specification")
end


# XT_PMINVERT -- Invert a pixel mask by changing 0 to 1 and non-zero to zero.

procedure yt_pminvert (pm)

pointer	pm		#I Pixel mask to be inverted

int	i, naxes, axlen[IM_MAXDIM], depth, npix, val
pointer	sp, v, buf, one
bool	pm_linenotempty()

begin
	call pm_gsize (pm, naxes, axlen, depth)

	call smark (sp)
	call salloc (v, IM_MAXDIM, TY_LONG)
	call salloc (buf, axlen[1], TY_INT)
	call salloc (one, 6, TY_INT)

	npix = axlen[1]
	RLI_LEN(one) = 2
	RLI_AXLEN(one) = npix
	Memi[one+3] = 1
	Memi[one+4] = npix
	Memi[one+5] = 1

	call amovkl (long(1), Meml[v], IM_MAXDIM)
	repeat {
	    if (pm_linenotempty (pm, Meml[v])) {
		call pmglpi (pm, Meml[v], Memi[buf], 0, npix, 0)
		do i = 0, npix-1 {
		    val = Memi[buf+i]
		    if (val == 0)
			Memi[buf+i] = 1
		    else
			Memi[buf+i] = 0
		}
		call pmplpi (pm, Meml[v], Memi[buf], 0, npix, PIX_SRC)
	    } else
		call pmplri (pm, Meml[v], Memi[one], 0, npix, PIX_SRC)
	    
	    do i = 2, naxes {
		Meml[v+i-1] = Meml[v+i-1] + 1
		if (Meml[v+i-1] <= axlen[i])
		    break
		else if (i < naxes)
		    Meml[v+i-1] = 1
	    }
	} until (Meml[v+naxes-1] > axlen[naxes])

	call sfree (sp)
end


# XT_MATCH -- Set the pixel mask to match the reference image.
# This matches sizes and possibly the physical coordinates and allows the
# original mask to be smaller or larger than the reference image.
# Subsequent use of the pixel mask can then work in the logical
# coordinates of the reference image.  The mask values are the maximum
# of the mask values which overlap each reference image pixel.
# A null input returns a null output.

procedure yt_match (im, refim, match)

pointer	im			#U Pixel mask image pointer
pointer	refim			#I Reference image pointer
char	match[ARB]		#I Match by physical coordinates?

int	i, j, k, l, i1, i2, j1, j2, nc, nl, ncpm, nlpm, nx, val
int	pmmatch, maxmaskval
double	x1, x2, y1, y2, lt[6], lt1[6], lt2[6]
long	vold[IM_MAXDIM], vnew[IM_MAXDIM]
pointer	str, pm, pmnew, imnew, mw, ctx, cty, bufref, bufpm

int	imstati(), strdic(), envfind(), nscan()
pointer	pm_open(), mw_openim(), im_pmmapo(), imgl1i(), mw_sctran()
bool	pm_empty(), pm_linenotempty()
errchk	yt_match_world, pm_open, mw_openim, im_pmmapo

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

	# Set match type.
	call malloc (str, SZ_FNAME, TY_CHAR)
	call sscan (match)
	call gargwrd (Memc[str], SZ_FNAME); call gargi (maxmaskval)
	if (nscan() == 1)
	    maxmaskval = 1
	pmmatch = strdic (Memc[str], Memc[str], SZ_FNAME, PM_MATCH)
	if (pmmatch == 0 && match[1] != EOS) {
	    if (envfind (match, Memc[str], SZ_FNAME) > 0) {
		call sscan (Memc[str])
		call gargwrd (Memc[str], SZ_FNAME); call gargi (maxmaskval)
		if (nscan() == 1)
		    maxmaskval = 1
		pmmatch = strdic (Memc[str], Memc[str], SZ_FNAME, PM_MATCH)
	    } else
	        pmmatch = PM_LOGICAL
	} else {
	    if (envfind ("pmmatch", Memc[str], SZ_FNAME) > 0) {
		call sscan (Memc[str])
		call gargwrd (Memc[str], SZ_FNAME); call gargi (maxmaskval)
		if (nscan() == 1)
		    maxmaskval = 1
		pmmatch = strdic (Memc[str], Memc[str], SZ_FNAME, PM_MATCH)
	    }
	}
	call mfree (str, TY_CHAR)
	if (pmmatch == 0)
	    call error (1, "Unknown or invalid pixel mask matching option")

	if (pmmatch == PM_WORLD) {
	    call yt_match_world (im, refim, maxmaskval)
	    return
	}

	# Compute transformation between reference (logical) coordinates
	# and mask (physical) coordinates.  Apply a world coordinate
	# offset if desired.

	mw = mw_openim (im)
	if (pmmatch == PM_OFFSET) {
	    call mw_gwtermd (mw, lt[5], lt1, lt, 2)
	    ctx = mw_sctran (mw, "world", "physical", 0)
	    call mw_ctrand (ctx, lt1, lt1[5], 2)
	} else
	    call aclrd (lt1[5], 2)
	call mw_gltermd (mw, lt, lt[5], 2)
	call mw_close (mw)

	if (pmmatch == PM_LOGICAL)
	    call amovd (lt, lt2, 6)
	else {
	    mw = mw_openim (refim)
	    if (pmmatch == PM_OFFSET) {
		ctx = mw_sctran (mw, "world", "physical", 0)
		call mw_ctrand (ctx, lt1, lt1[3], 2)
		lt1[5] = nint (lt1[5] - lt1[3])
		lt1[6] = nint (lt1[6] - lt1[4])
	    }
	    call mw_gltermd (mw, lt2, lt2[5], 2)
	    lt2[5] = lt2[5] - lt1[5]
	    lt2[6] = lt2[6] - lt1[6]
	    call mw_close (mw)
	}

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
	if (lt[1] == 1D0 && lt[4] == 1D0 && lt[5] == 0D0 && lt[6] == 0D0 &&
	    nc == ncpm && nl == nlpm)
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
	    vold[1] = i1
	    vnew[1] = 1

	    # If the scales are the same then it is just a problem of
	    # padding.  In this case use range lists for speed.
	    if (lt[1] == 1D0 && lt[4] == 1D0) {
		call malloc (bufpm, 3+3*nc, TY_INT)
		k = nint (lt[5])
		l = nint (lt[6])
		do j = max(1-l,j1), min(nl-l,j2) {
		    vold[2] = j
		    call plglri (pm, vold, Memi[bufpm], 0, nc, PIX_SRC)
		    if (k != 0) {
			bufref = bufpm
			do i = 2, Memi[bufpm] {
			    bufref = bufref + 3
			    Memi[bufref] = Memi[bufref] + k
			}
		    }
		    vnew[2] = j + l
		    call pmplri (pmnew, vnew, Memi[bufpm], 0, nc, PIX_SRC)
		}
		bufref = NULL

	    # Do all the geometry and pixel size matching.  This can
	    # be slow.
	    } else {
		call malloc (bufpm, nx, TY_INT)
		call malloc (bufref, nc, TY_INT)
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
	    }
	    call mfree (bufref, TY_INT)
	    call mfree (bufpm, TY_INT)
	}

	call mw_close (mw)
	call yt_pmunmap (im)
	im = imnew
	call imseti (im, IM_PMDES, pmnew)
end


# XT_MATCH_WORLD -- Set the pixel mask to match the reference image in
# world coordinates.  The algorithm can fail in various ways, especially
# when higher order WCS are used.  This ideally works with images and masks
# that are not greatly skewed in RA/DEC space.

procedure yt_match_world (im, refim, maxmaskval)

pointer	im			#U Pixel mask image pointer
pointer	refim			#I Reference image pointer
int	maxmaskval		#I Maximum mask value

int	i, j, k, l, nc, nl, ncpm, nlpm, cstep, lstep, buf, nxmsi, nymsi
int	c_im, l_im, c_ref, l_ref, c1_ref, c2_ref, l1_ref, l2_ref
int	xmin, xmax, ymin, ymax
double	pix_im[2], pix_ref[2], pix_tmp[2], w1[2], w2[2]
real	x, y, icstep, ilstep, d[2], der[2,2]
long	v[2]
pointer	sp, bits, rl
pointer	ba, mw_im, mw_ref, ct1, ct2, pm, xmsi, ymsi, xvec, yvec, ptr

int	imstati()
real	msieval()
pointer	xt_baopen(), pm_open(), im_pmmapo(), imgl1i()
pointer	mw_openim(), mw_sctran()
bool	pm_empty()
errchk	xt_baopen, pm_open, mw_openim, im_pmmapo, msiinit, msifit

begin
	if (im == NULL)
	    return

	# Set sizes.
	ncpm = IM_LEN(im,1)
	nlpm = IM_LEN(im,2)
	nc = IM_LEN(refim,1)
	nl = IM_LEN(refim,2)

	# If the mask is empty and the sizes are the same then it does not
	# matter if the two are actually matched in world coordinates.
	pm = imstati (im, IM_PMDES)
	if (pm_empty(pm) && nc == ncpm && nl == nlpm)
	    return

	# Allocate working lines.
	call smark (sp)
	call salloc (bits, nc, TY_INT)
	call salloc (rl, 3+3*ncpm, TY_INT)

	# Use a bit array to hold the output in memory compactly.
	ba = xt_baopen (nc, nl, maxmaskval)

	# Set logical to logical transformation through the world coordinate
	# systems.  Use a surface fit to speed up the WCS calculations.

	mw_im = mw_openim (im)
	mw_ref = mw_openim (refim)

	# First bound the reference image in world coordinates.
	# The image is sampled and a small amount of buffer is used.

	ct1 = mw_sctran (mw_ref, "logical", "world", 3)
	cstep = 20; lstep = 20
	icstep = (nc - 1.) / (cstep - 1.); ilstep = (nl - 1.) / (lstep - 1.)
	w1[1] = MAX_DOUBLE; w1[2] = -MAX_DOUBLE
	w2[1] = MAX_DOUBLE; w2[2] = -MAX_DOUBLE
	for (pix_im[2]=1-ilstep; pix_im[2]<=nl+1+ilstep;
	    pix_im[2]=pix_im[2]+ilstep) {
	    for (pix_im[1]=1-icstep; pix_im[1]<=nc+1+icstep;
		pix_im[1]=pix_im[1]+icstep) {
		call mw_ctrand (ct1, pix_im, pix_ref, 2)
		w1[1] = min (w1[1], pix_ref[1])
		w1[2] = max (w1[2], pix_ref[1])
		w2[1] = min (w2[1], pix_ref[2])
		w2[2] = max (w2[2], pix_ref[2])
	    }
	}
	call mw_ctfree (ct1)

	# Fit coordinate surfaces for the mapping from the mask to the
	# the reference image.  This is done because the WCS evaluations
	# can be slow.  This is done on a subsample and then linear
	# interpolation will be done.  Provide a buffer to avoid edge
	# effects from the subsampling.  Bound the mask to what overlaps
	# the reference image.

	cstep = 10; lstep = 10; buf = 1

	ct1 = mw_sctran (mw_im, "logical", "world", 3)
	ct2 = mw_sctran (mw_ref, "world", "logical", 3)

	call msiinit (xmsi, II_BILINEAR)
	call msiinit (ymsi, II_BILINEAR)
	nxmsi = nint ((ncpm - 1.) / cstep + 2*buf + 1)
	nymsi = nint ((nlpm - 1.) / lstep + 2*buf + 1)
	icstep = (nxmsi - (2.*buf + 1)) / (ncpm - 1.)
	ilstep = (nymsi - (2.*buf + 1)) / (nlpm - 1.)
	call malloc (xvec, nxmsi*nymsi, TY_REAL)
	call malloc (yvec, nxmsi*nymsi, TY_REAL)
	xmin=ncpm+1; xmax=0; ymin=nlpm+1; ymax=0
	k = -1
	do j = 1, nymsi {
	    pix_im[2] = (j - (2*buf))  / ilstep + 1
	    do i = 1, nxmsi {
		k = k + 1
		pix_im[1] = (i - (2*buf)) / icstep + 1
		call mw_ctrand (ct1, pix_im, pix_tmp, 2)
		if (pix_tmp[1] < w1[1] || pix_tmp[1] > w1[2] ||
		    pix_tmp[2] < w2[1] || pix_tmp[2] > w2[2]) {
		    Memr[xvec+k] = 0
		    Memr[yvec+k] = 0
		    next
		}

		call mw_ctrand (ct2, pix_tmp, pix_ref, 2)
		x = pix_ref[1]
		y = pix_ref[2]
		if (x > 0.5 && x < nc+0.5 && y > 0.5 && y < nl+0.5) {
		    l = max (1, min (ncpm, nint (pix_im[1])))
		    xmin = min (xmin, l)
		    xmax = max (xmax, l)
		    l = max (1, min (nlpm, nint (pix_im[2])))
		    ymin = min (ymin, l)
		    ymax = max (ymax, l)
		}

		Memr[xvec+k] = x
		Memr[yvec+k] = y
	    }
	}
	call msifit (xmsi, Memr[xvec], nxmsi, nymsi, nxmsi) 
	call msifit (ymsi, Memr[yvec], nxmsi, nymsi, nxmsi) 
	call mfree (xvec, TY_REAL)
	call mfree (yvec, TY_REAL)
	call mw_close (mw_im)
	call mw_close (mw_ref)

	# Expand the mask bound to avoid missing the edge.
	i = (xmin - 1) * icstep + (2*buf) - 1
	xmin = (i - (2*buf)) / icstep + 1
	xmin = max (1, min (ncpm, nint(xmin)))
	i = (xmax - 1) * icstep + (2*buf) + 1.99
	xmax = (i - (2*buf)) / icstep + 1
	xmax = max (1, min (ncpm, nint(xmax)))
	j = (ymin - 1) * ilstep + (2*buf) - 1
	ymin = (j - (2*buf)) / ilstep + 1
	ymin = max (1, min (nlpm, nint(ymin)))
	j = (ymax - 1) * ilstep + (2*buf) + 1.99
	ymax = (j - (2*buf)) / ilstep + 1
	ymax = max (1, min (nlpm, nint(ymax)))

	# Determine size of mask pixel in reference system.
	# This is approximate because we don't take into account the
	# shape of the transformed square mask pixels.

	x = (xmax+xmin)/2; y = (ymax+ymin)/2
	x = (x - 1) * icstep + (2*buf); y = (y - 1) * ilstep + (2*buf)
	call msider (xmsi, x, y, der, 2, 2, 2)
	d[1] = max (abs(der[2,1]), abs(der[1,2])) * icstep
	call msider (ymsi, x, y, der, 2, 2, 2)
	d[2] = max (abs(der[2,1]), abs(der[1,2])) * ilstep

	# Go through each mask pixel and add to the new mask.
	# This uses range lists to quickly skip good pixels.

	v[1] = 1
	do l_im = ymin, ymax {
	    v[2] = l_im
	    call plglri (pm, v, Memi[rl], 0, ncpm, PIX_SRC)
	    y = (l_im - 1) * ilstep + (2*buf)
	    ptr = rl
	    do k = RL_FIRST, RLI_LEN(rl) {
		ptr = ptr + RL_LENELEM
		c_im = Memi[ptr+RL_XOFF]
		if (c_im > xmax)
		    next
		if (c_im+Memi[ptr+RL_NOFF]-1 < xmin)
		    next
		x = (c_im - 1) * icstep + (2*buf) - icstep
		do c_im = 1, Memi[ptr+RL_NOFF] {
		    x = x + icstep
		    pix_ref[1] = msieval (xmsi, x, y)
		    pix_ref[2] = msieval (ymsi, x, y)
		    pix_tmp[1] = max (1D0, pix_ref[1] - 0.45 * d[1])
		    pix_tmp[2] = min (double(nc), pix_ref[1] + 0.45 * d[1])
		    if (pix_tmp[2] < 1 || pix_tmp[1] > nc)
		        next
		    c1_ref = nint (pix_tmp[1])
		    c2_ref = nint (pix_tmp[2])
		    pix_tmp[1] = max (1D0, pix_ref[2] - 0.45 * d[2])
		    pix_tmp[2] = min (double(nl), pix_ref[2] + 0.45 * d[2])
		    if (pix_tmp[2] < 1 || pix_tmp[1] > nl)
		        next
		    l1_ref = nint (pix_tmp[1])
		    l2_ref = nint (pix_tmp[2])
		    do l_ref = l1_ref, l2_ref {
			do c_ref = c1_ref, c2_ref {
			    call xt_bapi (ba, c_ref, l_ref,
			        Memi[ptr+RL_VOFF], 1)
			}
		    }
		}
	    }
	}

	call msifree (xmsi)
	call msifree (ymsi)
	call yt_pmunmap (im)

	# Create a new pixel mask of the required size and populate.
	# Do dummy image I/O to set the header.

	pm = pm_open (NULL)
	call pm_ssize (pm, 2, IM_LEN(refim,1), 27)
	im = im_pmmapo (pm, NULL)
	ptr = imgl1i (im)

	do j = 1, nl {
	    call xt_bagi (ba, 1, j, Memi[bits], nc)
	    v[2] = j
	    call pmplpi (pm, v, Memi[bits], 0, nc, PIX_SRC)
	}

	call imseti (im, IM_PMDES, pm)

	call xt_baclose (ba)
	call sfree (sp)
end
