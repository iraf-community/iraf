include	<mach.h>
include	<ctype.h>
include	<error.h>
include	<imhdr.h>
include	<imset.h>
include	<pmset.h>
include	<mwset.h>
include	<syserr.h>


# XT_PMMAP -- Open a pixel mask READ_ONLY.
#
# This routine maps multiple types of mask files and designations.
# It matches the mask coordinates to the reference image based on the
# physical coordinate system so the mask may be of a different size.
# The mask name is returned so that the task has the name pointed to by "BPM".
# A null filename is allowed and returns NULL.

pointer procedure xt_pmmap (pmname, refim, mname, sz_mname)

char	pmname[ARB]		#I Pixel mask name
pointer	refim			#I Reference image pointer
char	mname[ARB]		#O Expanded mask name
int	sz_mname		#O Size of expanded mask name

int	i, flag, nowhite()
pointer	sp, fname, im, ref, xt_pmmap1()
bool	streq()
errchk	xt_pmmap1

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	i = nowhite (pmname, Memc[fname], SZ_FNAME)

	if (Memc[fname] == '!') {
	    flag = INVERT_MASK
	    fname = fname + 1
	} else
	    flag = 0

	im = NULL
	if (streq (pmname, "BPM")) {
	    iferr (call imgstr (refim, "BPM", Memc[fname], SZ_FNAME))
		#call strcpy ("EMPTY", Memc[fname], SZ_FNAME)
		Memc[fname] = EOS
	}

	if (streq (Memc[fname], "EMPTY"))
	    ref = refim
	else
	    ref = NULL

	if (Memc[fname] != EOS)
	    im = xt_pmmap1 (Memc[fname], ref, refim, flag)
	call strcpy (Memc[fname], mname, sz_mname)

	call sfree (sp)
	return (im)
end


# XT_PMMAP1 -- Open a pixel mask READ_ONLY.  The input mask may be
# a pixel list image, a non-pixel list image, or a text file.
# Return error if the pixel mask cannot be opened.  For pixel masks
# or image masks match the WCS.

pointer procedure xt_pmmap1 (pmname, ref, refim, flag)

char	pmname[ARB]		#I Pixel mask name
pointer	ref			#I Reference image for pixel mask
pointer	refim			#I Reference image for image or text
int	flag			#I Mask flag

int	imstati(),  errcode()
pointer	im, pm
pointer	im_pmmap(), xt_pmimmap(), xt_pmtext(), xt_pmsection()
bool	streq()
errchk	xt_match

begin
	im = NULL

	if (streq (pmname, "STDIN"))
	    im = xt_pmtext (pmname, refim, flag)

	else if (pmname[1] == '[')
	    im = xt_pmsection (pmname, refim, flag)

	else {
	    ifnoerr (im = im_pmmap (pmname, READ_ONLY, ref)) {
		call xt_match (im, refim)
		if (flag == INVERT_MASK) {
		    pm = imstati (im, IM_PMDES)
		    call xt_pminvert (pm)
		    call imseti (im, IM_PMDES, pm)
		}
	    } else {
		switch (errcode()) {
		case SYS_FOPNNEXFIL, SYS_PLBADSAVEF:
		    ifnoerr (im = xt_pmimmap (pmname, refim, flag))
			call xt_match (im, refim)
		    else {
			switch (errcode()) {
			case SYS_IKIOPEN:
			    im = xt_pmtext (pmname, refim, flag)
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

pointer procedure xt_pmimmap (pmname, refim, flag)

char	pmname[ARB]		#I Image name
pointer	refim			#I Reference image pointer
int	flag			#I Mask flag

short	val
int	i, ndim, npix, rop
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

	if (flag == INVERT_MASK)
	    rop = PIX_NOT(PIX_SRC)
	else
	    rop = PIX_SRC

	while (imgnls (im_in, data, Meml[v1]) != EOF) {
	    if (flag == INVERT_MASK) {
		do i = 0, npix-1 {
		    val = Mems[data+i]
		    if (val <= 0)
		       Mems[data+i] = 1
		    else
		       Mems[data+i] = 0
		}
	    } else {
		do i = 0, npix-1 {
		    val = Mems[data+i]
		    if (val < 0)
		       Mems[data+i] = 0
		}
	    }
	    call pmplps (pm, Meml[v2], Mems[data], 0, npix, rop)
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


# XT_PMTEXT -- Create a pixel mask from a text file of rectangles.
# Return error if the file cannot be opened.
# This routine only applies to the first 2D plane.

pointer procedure xt_pmtext (pmname, refim, flag)

char	pmname[ARB]		#I Image name
pointer	refim			#I Reference image pointer
int	flag			#I Mask flag

int	fd, nc, nl, c1, c2, l1, l2, nc1, nl1, rop
pointer	pm, im, mw, dummy

int	open(), fscan(), nscan()
pointer	pm_newmask(), im_pmmapo(), imgl1i(), mw_openim()
errchk	open

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

pointer procedure xt_pmsection (section, refim, flag)

char	section[ARB]		#I Image section
pointer	refim			#I Reference image pointer
int	flag			#I Mask flag

int	i, j, ip, temp, a[2], b[2], c[2], rop, ctoi()
pointer	pm, im, mw, dummy, pm_newmask(), im_pmmapo(), imgl1i(), mw_openim()
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

procedure xt_pminvert (pm)

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
# This matches sizes and physical coordinates and allows the
# original mask to be smaller or larger than the reference image.
# Subsequent use of the pixel mask can then work in the logical
# coordinates of the reference image.  A null input returns a null output.

procedure xt_match (im, refim)

pointer	im			#U Pixel mask image pointer
pointer	refim			#I Reference image pointer

int	i, j, k, nc, nl, ncpm, nlpm, c1, c2, l1, l2, nref, npm
int	steptype, xoffset, xstep, yoffset, ystep
double	x1, x2, y1, y2
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
	call mw_c2trand (ctref, 1D0, 1D0, x1, y1) 
	call mw_c2trand (ctpm, x1, y1, x1, y1) 
	call mw_c2trand (ctref, 2D0, 1D0, x2, y2) 
	call mw_c2trand (ctpm, x2, y2, x2, y2) 
	if (abs(x2-x1) < 1D0) {
	    steptype = 2
	    call mw_ctfree (ctref)
	    call mw_ctfree (ctpm)
	    ctref = mw_sctran (mwref, "physical", "logical", 3)
	    ctpm = mw_sctran (mwpm, "logical", "physical", 3)
	    call mw_c2trand (ctpm, 1D0, 1D0, x1, y1) 
	    call mw_c2trand (ctref, x1, y1, x1, y1) 
	    call mw_c2trand (ctpm, 2D0, 1D0, x2, y2) 
	    call mw_c2trand (ctref, x2, y2, x2, y2) 
	}
	x2 = x2 - x1
	if (abs(y1-y2) > 10*EPSILONR)
	    call error (0, "Image and mask have a relative rotation")
#	if (abs(x1-nint(x1)) > 10*EPSILONR  &&
#	    abs(x1-nint(x1))-0.5 > 10*EPSILONR)
	if (abs(x1-nint(x1)) > 10*EPSILONR)
	    call error (0, "Image and mask have non-integer relative offsets")
	if (abs(x2-nint(x2)) > 10*EPSILONR)
	    call error (0, "Image and mask have non-integer relative steps")
	xoffset = nint (x1 - 1D0)
	xstep = nint (x2)

	if (steptype == 1) {
	    call mw_c2trand (ctref, 1D0, 1D0, x1, y1) 
	    call mw_c2trand (ctpm, x1, y1, x1, y1) 
	    call mw_c2trand (ctref, 1D0, 2D0, x2, y2) 
	    call mw_c2trand (ctpm, x2, y2, x2, y2) 
	} else {
	    call mw_c2trand (ctpm, 1D0, 1D0, x1, y1) 
	    call mw_c2trand (ctref, x1, y1, x1, y1) 
	    call mw_c2trand (ctpm, 1D0, 2D0, x2, y2) 
	    call mw_c2trand (ctref, x2, y2, x2, y2) 
	}
	y2 = y2 - y1
	if (abs(x1-x2) > 10*EPSILONR)
	    call error (0, "Image and mask have a relative rotation")
#	if (abs(y1-nint(y1)) > 10*EPSILONR  &&
#	    abs(y1-nint(y1))-0.5 > 10*EPSILONR)
	if (abs(y1-nint(y1)) > 10*EPSILONR)
	    call error (0, "Image and mask have non-integer relative offsets")
	if (abs(y2-nint(y2)) > 10*EPSILONR)
	    call error (0, "Image and mask have non-integer relative steps")
	yoffset = nint (y1 - 1D0)
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
