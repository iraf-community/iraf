# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<mach.h>
include	<imhdr.h>
include	"src/icombine.h"


# T_IMCOMBINE - This task combines a list of images into an output image
# and an optional sigma image.  There are many combining options from
# which to choose.

procedure t_imcombine ()

pointer	sp, fname, output, headers, bmask, rmask, sigma, nrmask, emask, logfile
pointer	scales, zeros, wts, im
int	n, input, ilist, olist, hlist, blist, rlist, slist, nrlist, elist

bool	clgetb()
real	clgetr()
int	clgwrd(), clgeti(), imtopenp(), imtopen(), imtgetim(), imtlen()
pointer	immap()
errchk	immap, icombine

include	"src/icombine.com"

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (headers, SZ_FNAME, TY_CHAR)
	call salloc (bmask, SZ_FNAME, TY_CHAR)
	call salloc (rmask, SZ_FNAME, TY_CHAR)
	call salloc (nrmask, SZ_FNAME, TY_CHAR)
	call salloc (emask, SZ_FNAME, TY_CHAR)
	call salloc (sigma, SZ_FNAME, TY_CHAR)
	call salloc (expkeyword, SZ_FNAME, TY_CHAR)
	call salloc (statsec, SZ_FNAME, TY_CHAR)
	call salloc (gain, SZ_FNAME, TY_CHAR)
	call salloc (rdnoise, SZ_FNAME, TY_CHAR)
	call salloc (snoise, SZ_FNAME, TY_CHAR)
	call salloc (logfile, SZ_FNAME, TY_CHAR)

	# Get task parameters.  Some additional parameters are obtained later.
	ilist = imtopenp ("input")
	olist = imtopenp ("output")
	hlist = imtopenp ("headers")
	blist = imtopenp ("bpmasks")
	rlist = imtopenp ("rejmasks")
	nrlist = imtopenp ("nrejmasks")
	elist = imtopenp ("expmasks")
	slist = imtopenp ("sigmas")
	call clgstr ("logfile", Memc[logfile], SZ_FNAME)

	project = clgetb ("project")
	combine = clgwrd ("combine", Memc[fname], SZ_FNAME, COMBINE)
	if (combine == MEDIAN || combine == LMEDIAN) {
	    if (combine == MEDIAN)
		medtype = MEDAVG
	    else {
		medtype = MEDLOW
		combine = MEDIAN
	    }
	}
	reject = clgwrd ("reject", Memc[fname], SZ_FNAME, REJECT)
	blank = clgetr ("blank")
	call clgstr ("expname", Memc[expkeyword], SZ_FNAME)
	call clgstr ("statsec", Memc[statsec], SZ_FNAME)
	call clgstr ("gain", Memc[gain], SZ_FNAME)
	call clgstr ("rdnoise", Memc[rdnoise], SZ_FNAME)
	call clgstr ("snoise", Memc[snoise], SZ_FNAME)
	lthresh = clgetr ("lthreshold")
	hthresh = clgetr ("hthreshold")
	lsigma = clgetr ("lsigma")
	hsigma = clgetr ("hsigma")
	pclip = clgetr ("pclip")
	flow = clgetr ("nlow")
	fhigh = clgetr ("nhigh")
	nkeep = clgeti ("nkeep")
	grow = clgetr ("grow")
	mclip = clgetb ("mclip")
	sigscale = clgetr ("sigscale")
	verbose = false

	# Check lists.
	n = imtlen (ilist)
	if (n == 0)
	    call error (1, "No input images to combine")

	if (project) {
	    if (imtlen (olist) != n)
		call error (1, "Wrong number of output images")
	    if (imtlen (hlist) != 0 && imtlen (hlist) != n)
		call error (1, "Wrong number of header files")
	    if (imtlen (blist) != 0 && imtlen (blist) != n)
		call error (1, "Wrong number of bad pixel masks")
	    if (imtlen (rlist) != 0 && imtlen (rlist) != n)
		call error (1, "Wrong number of rejection masks")
	    if (imtlen (nrlist) > 0 && imtlen (nrlist) != n)
		call error (1, "Wrong number of number rejected masks")
	    if (imtlen (elist) > 0 && imtlen (elist) != n)
		call error (1, "Wrong number of exposure masks")
	    if (imtlen (slist) > 0 && imtlen (slist) != n)
		call error (1, "Wrong number of sigma images")
	} else {
	    if (imtlen (olist) != 1)
		call error (1, "Wrong number of output images")
	    if (imtlen (hlist) > 1)
		call error (1, "Wrong number of header files")
	    if (imtlen (blist) > 1)
		call error (1, "Wrong number of bad pixel masks")
	    if (imtlen (rlist) > 1)
		call error (1, "Wrong number of rejection masks")
	    if (imtlen (nrlist) > 1)
		call error (1, "Wrong number of number rejected masks")
	    if (imtlen (elist) > 1)
		call error (1, "Wrong number of exposure masks")
	    if (imtlen (slist) > 1)
		call error (1, "Wrong number of sigma images")
	}

	# Check parameters, map INDEFs, and set threshold flag
	if (pclip == 0. && reject == PCLIP)
	    call error (1, "Pclip parameter may not be zero")
	if (IS_INDEFR (blank))
	    blank = 0.
	if (IS_INDEFR (lsigma))
	    lsigma = MAX_REAL
	if (IS_INDEFR (hsigma))
	    hsigma = MAX_REAL
	if (IS_INDEFR (pclip))
	    pclip = -0.5
	if (IS_INDEFR (flow))
	    flow = 0
	if (IS_INDEFR (fhigh))
	    fhigh = 0
	if (IS_INDEFR (grow))
	    grow = 0.
	if (IS_INDEF (sigscale))
	    sigscale = 0.

	if (IS_INDEF(lthresh) && IS_INDEF(hthresh))
	    dothresh = false
	else {
	    dothresh = true
	    if (IS_INDEF(lthresh))
		lthresh = -MAX_REAL
	    if (IS_INDEF(hthresh))
		hthresh = MAX_REAL
	}

	# Loop through image lists.
	while (imtgetim (ilist, Memc[fname], SZ_FNAME) != EOF) {
	    iferr {
		scales = NULL; input = ilist

		if (imtgetim (olist, Memc[output], SZ_FNAME) == EOF) {
		    if (project) {
			call sprintf (Memc[output], SZ_FNAME,
			    "IMCOMBINE: No output image for %s")
			    call pargstr (Memc[fname])
			call error (1, Memc[output])
		    } else
			call error (1, "IMCOMBINE: No output image")
		}
		if (imtgetim (hlist, Memc[headers], SZ_FNAME) == EOF)
		    Memc[headers] = EOS
		if (imtgetim (blist, Memc[bmask], SZ_FNAME) == EOF)
		    Memc[bmask] = EOS
		if (imtgetim (rlist, Memc[rmask], SZ_FNAME) == EOF)
		    Memc[rmask] = EOS
		if (imtgetim (nrlist, Memc[nrmask], SZ_FNAME) == EOF)
		    Memc[nrmask] = EOS
		if (imtgetim (elist, Memc[emask], SZ_FNAME) == EOF)
		    Memc[emask] = EOS
		if (imtgetim (slist, Memc[sigma], SZ_FNAME) == EOF)
		    Memc[sigma] = EOS

		# Set the input list and initialize the scaling factors.
		if (project) {
		    im = immap (Memc[fname], READ_ONLY, 0)
		    if (IM_NDIM(im) == 1)
			n = 0
		    else
			n = IM_LEN(im,IM_NDIM(im))
		    call imunmap (im)
		    if (n == 0) {
			call sprintf (Memc[output], SZ_FNAME,
			    "IMCOMBINE: Can't project one dimensional image %s")
			    call pargstr (Memc[fname])
			call error (1, Memc[output])
		    }
		    input = imtopen (Memc[fname])
		} else {
		    call imtrew (ilist)
		    n = imtlen (ilist)
		    input = ilist
		}

		# Allocate and initialize scaling factors.
		call malloc (scales, 3*n, TY_REAL)
		zeros = scales + n
		wts = scales + 2 * n
		call amovkr (INDEFR, Memr[scales], 3*n)

		call icombine (input, Memc[output], Memc[headers], Memc[bmask],
		    Memc[rmask], Memc[nrmask], Memc[emask], Memc[sigma],
		    Memc[logfile], Memr[scales], Memr[zeros], Memr[wts],
		    NO, NO, NO)

	    } then
		call erract (EA_WARN)

	    if (input != ilist)
		call imtclose (input)
	    call mfree (scales, TY_REAL)
	    if (!project)
		break
	}

	call imtclose (ilist)
	call imtclose (olist)
	call imtclose (hlist)
	call imtclose (blist)
	call imtclose (rlist)
	call imtclose (nrlist)
	call imtclose (elist)
	call imtclose (slist)
	call sfree (sp)
end
