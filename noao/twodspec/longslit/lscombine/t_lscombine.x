# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<mach.h>
include	<imhdr.h>
include	"src/icombine.h"


# T_LSCOMBINE - This task combines a list of images into an output image
# and optional associated images and mask.  There are many combining options
# from which to choose.
#
# This is a variant of IMCOMBINE that combines longslit spectra matched in
# world coordinates.  The spectral images are first resampled to a common
# grid of pixels in temporary images and then combined, after which the
# temporary images are deleted.

procedure t_lscombine ()

pointer	sp, fname, output, headers, bmask, rmask, sigma, nrmask, emask, logfile
pointer	scales, zeros, wts, im
int	n, input, ilist, olist, hlist, blist, rlist, slist, nrlist, elist
int	input1, mask1, delete

bool	clgetb()
real	clgetr()
int	clgwrd(), clgeti(), imtopenp(), imtopen(), imtgetim(), imtlen()
pointer	immap()
errchk	immap, icombine, lsc_transform

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
	call salloc (ictask, SZ_FNAME, TY_CHAR)
	call salloc (expkeyword, SZ_FNAME, TY_CHAR)
	call salloc (statsec, SZ_FNAME, TY_CHAR)
	call salloc (gain, SZ_FNAME, TY_CHAR)
	call salloc (rdnoise, SZ_FNAME, TY_CHAR)
	call salloc (snoise, SZ_FNAME, TY_CHAR)
	call salloc (logfile, SZ_FNAME, TY_CHAR)

	# Get task parameters.  Some additional parameters are obtained later.
	call strcpy ("LSCOMBINE", Memc[ictask], SZ_FNAME)
	ilist = imtopenp ("input")
	olist = imtopenp ("output")
	hlist = imtopenp ("headers")
	blist = imtopenp ("bpmasks")
	rlist = imtopenp ("rejmasks")
	nrlist = imtopenp ("nrejmasks")
	elist = imtopenp ("expmasks")
	slist = imtopenp ("sigmas")
	call clgstr ("logfile", Memc[logfile], SZ_FNAME)

	#project = clgetb ("project")
	project = false
	combine = clgwrd ("combine", Memc[fname], SZ_FNAME, COMBINE)
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
		scales = NULL; input = ilist; input1 = NULL; mask1 = NULL

		if (imtgetim (olist, Memc[output], SZ_FNAME) == EOF) {
		    if (project) {
			call sprintf (Memc[output], SZ_FNAME,
			    "LSCOMBINE: No output image for %s")
			    call pargstr (Memc[fname])
			call error (1, Memc[output])
		    } else
			call error (1, "LSCOMBINE: No output image")
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
			    "LSCOMBINE: Can't project one dimensional image %s")
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

		# Register the images.
		call lsc_transform (input, input1, mask1)

		# Set special values for LSCOMBINE application. 
		dothresh = true
		if (IS_INDEF(lthresh))
		    lthresh = -MAX_REAL
		if (IS_INDEF(hthresh))
		    hthresh = MAX_REAL
		lthresh = max (-MAX_REAL * 0.999, lthresh)

		# Combine and then delete the temporary transformed images.
		call icombine (input1, Memc[output], Memc[headers], Memc[bmask],
		    Memc[rmask], Memc[nrmask], Memc[emask], Memc[sigma],
		    Memc[logfile], Memr[scales], Memr[zeros], Memr[wts], NO,
		    delete)

		# Delete temporary files.
		if (input1 != input) {
		    call imtrew (input1)
		    while (imtgetim (input1, Memc[fname], SZ_FNAME) != EOF)
		        iferr (call imdelete (Memc[fname]))
			    ;
		    while (imtgetim (mask1, Memc[fname], SZ_FNAME) != EOF)
		        iferr (call imdelete (Memc[fname]))
			    ;
		}

	    } then
		call erract (EA_WARN)

	    if (input1 != NULL && input1 != input)
		call imtclose (input1)
	    if (mask1 != NULL)
		call imtclose (mask1)
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


include	<math/iminterp.h>


# LSC_TRANSFORM -- Transform list of spectra to a matching coordinate system.
# The routine uses additional task parameters to specify the desired
# coordinate system.

procedure lsc_transform (input, output, masks)

pointer	input			#I List of input spectra
pointer	output			#O List of transformed spectra
pointer	masks			#O List of masks

bool	dotransform
int	i, j, n, err, nwa[2], nw[2], nusf, nvsf, mtype
real	w1a[2], w2a[2], dwa[2], w1[2], w2[2], dw[2], aux
pointer	sp, inname, outname, minname, moutname, tmp
pointer	w1s[2], w2s[2], dws[2], nws[2], linear[2]
pointer	in, out, pmin, pmout, mw, ct, ptr
pointer un[2], usf, vsf, xmsi, ymsi, jmsi, xout, yout, dxout, dyout

bool	streq()
int	clgeti(), clgwrd(), errget()
int	imtopen(), imtgetim(), imtrgetim(), imtlen()
real	clgetr()
real	mw_c1tranr()
pointer	immap(), mw_openim(), mw_sctran(), yt_mappm()
errchk	immap, mw_openim, mw_sctran, yt_mappm

include "../transform/transform.com"

begin

	n = imtlen (input)

	call smark (sp)
	call salloc (inname, SZ_FNAME, TY_CHAR)
	call salloc (outname, SZ_FNAME, TY_CHAR)
	call salloc (minname, SZ_FNAME, TY_CHAR)
	call salloc (moutname, SZ_FNAME, TY_CHAR)
	call salloc (tmp, SZ_FNAME, TY_CHAR)
	do j = 1, 2 {
	    call salloc (w1s[j], n, TY_REAL)
	    call salloc (w2s[j], n, TY_REAL)
	    call salloc (dws[j], n, TY_REAL)
	    call salloc (nws[j], n, TY_INT)
	    call salloc (linear[j], n, TY_INT)
	}

	# Get/set parameters.  These are similar to TRANSFORM.
	itype = clgwrd ("interptype", Memc[inname], SZ_FNAME, II_BFUNCTIONS)
	u1 = clgetr ("x1"); u2 = clgetr ("x2");
	du = clgetr ("dx"); nu = clgeti ("nx")
	v1 = clgetr ("y1"); v2 = clgetr ("y2")
	dv = clgetr ("dy"); nv = clgeti ("ny")
	ulog = false; vlog = false
	flux = true
	blank = -MAX_REAL
	usewcs = true

	# The mask is only generated if the COMBINE parameter masktype is set.
	mtype = clgwrd ("masktype", Memc[tmp], SZ_FNAME, "|none|goodvalue|")

	err = 0; dotransform = false
	iferr {
	    in = NULL; pmin = NULL; out = NULL; pmout = NULL;  mw= NULL

	    # Get the linear WCS (or approximation) for each input.
	    # We get them all first since we need to compute a global
	    # WCS for the final combined spectrm.

	    do i = 0, n-1 {
		if (imtrgetim (input, i+1, Memc[inname], SZ_FNAME) == EOF)
		    call error (1, "Premature end of input list")
		ptr = immap (Memc[inname], READ_ONLY, 0); in = ptr
		ptr = mw_openim (in); mw = ptr
		do j = 1, 2 {
		    ct = mw_sctran (mw, "logical", "world", j)
		    Memi[nws[j]+i] = IM_LEN(in,j)
		    Memr[w1s[j]+i] =  mw_c1tranr (ct, 1.)
		    Memr[w2s[j]+i] =  mw_c1tranr (ct, real(Memi[nws[j]+i]))
		    Memr[dws[j]+i] = (Memr[w2s[j]+i] - Memr[w1s[j]+i]) /
		        (Memi[nws[j]+i] - 1)
		    call mw_ctfree (ct)
		    call mw_gwattrs (mw, j, "wtype", Memc[outname], SZ_FNAME)
		    if (streq (Memc[outname], "linear"))
		        Memi[linear[j]+i] = YES
		    else
		        Memi[linear[j]+i] = NO
		}
		call mw_close (mw)
		call imunmap (in)
	    }

	    # Set the linear WCS for each axis.  The follow sets values for
	    # those elements specified by the users as INDEF.

	    w1a[1] = u1; w2a[1] = u2; dwa[1] = du; nwa[1] = nu
	    w1a[2] = v1; w2a[2] = v2; dwa[2] = dv; nwa[2] = nv
	    do j = 1, 2 {
		w1[j] = w1a[j]; w2[j] = w2a[j]; dw[j] = dwa[j]; nw[j] = nwa[j]

		# Starting value.
		if (IS_INDEFR(w1[j])) {
		    if (IS_INDEFR(dw[j]) || dw[j] > 0.) {
			w1[j] = MAX_REAL
			do i = 0, n-1 {
			    if (Memr[dws[j]+i] > 0.)
			        aux = Memr[w1s[j]+i]
			    else
			        aux = Memr[w2s[j]+i]
			    if (aux < w1[j])
			        w1[j] = aux
			}
		    } else {
			w1[j] = -MAX_REAL
			do i = 0, n-1 {
			    if (Memr[dws[j]+i] > 0.)
			        aux = Memr[w2s[j]+i]
			    else
			        aux = Memr[w1s[j]+i]
			    if (aux > w1[j])
			        w1[j] = aux
			}
		    }
		}

		# Ending value.
		if (IS_INDEFR(w2[j])) {
		    if (IS_INDEFR(dw[j]) || dw[j] > 0.) {
			w2[j] = -MAX_REAL
			do i = 0, n-1 {
			    if (Memr[dws[j]+i] > 0.)
			        aux = Memr[w2s[j]+i]
			    else
			        aux = Memr[w1s[j]+i]
			    if (aux > w2[j])
			        w2[j] = aux
			}
		    } else {
			w2[j] = MAX_REAL
			do i = 0, n-1 {
			    if (Memr[dws[j]+i] > 0.)
			        aux = Memr[w1s[j]+i]
			    else
			        aux = Memr[w2s[j]+i]
			    if (aux < w2[j])
			        w2[j] = aux
			}
		    }
		}

		# Increment.
		if (IS_INDEFR(dw[j])) {
		    dw[j] = MAX_REAL
		    do i = 0, n-1 {
		        aux = abs (Memr[dws[j]+i])
			if (aux < dw[j])
			    dw[j] = aux
		    }
		}
		if ((w2[j] - w1[j]) / dw[j] < 0.)
		    dw[j] = -dw[j]

		# Number of pixels.
		if (IS_INDEFI(nw[j]))
		    nw[j] = int ((w2[j] - w1[j]) / dw[j] + 0.5) + 1

		# Adjust the values.
		if (IS_INDEFR(dwa[j]))
		    dw[j] = (w2[j] - w1[j]) / (nw[j] - 1)
		else if (IS_INDEFR(w2a[j]))
		    w2[j] = w1[j] + (nw[j] - 1) * dw[j]
		else if (IS_INDEFR(w1a[j]))
		    w1[j] = w2[j] - (nw[j] - 1) * dw[j]
		else {
		    nw[j] = int ((w2[j] - w1[j]) / dw[j] + 0.5) + 1
		    w2[j] = w1[j] + (nw[j] - 1) * dw[j]
		}
	    }

	    # Check if the images need to be transformed.  If all the
	    # input are already in the desired system then we don't need
	    # to need to transform.  But if even one needs to be transformed
	    # we transform all of them.  This is not ideal but it simplifies
	    # the code for now.

	    do i = 0, n-1 {
	        do j = 1, 2 {
		    if (Memi[linear[j]+i] != YES)
		        dotransform = true
		    if (Memr[w1s[j]+i] != w1[j])
		        dotransform = true
		    if (Memr[w2s[j]+i] != w2[j])
		        dotransform = true
		    if (Memr[dws[j]+i] != dw[j])
		        dotransform = true
		    if (dotransform)
		        break
		}
		if (dotransform)
		    break
	    }

	    # Transform the images if needed.
	    if (dotransform) {
	        u1 = w1[1]; u2 = w2[1]; du = dw[1]; nu = nw[1]
	        v1 = w1[2]; v2 = w2[2]; dv = dw[2]; nv = nw[2]
		call mktemp ("lsc", Memc[tmp], SZ_FNAME)
		do i = 0, n-1 {
		    # Get the input name.
		    if (imtrgetim (input, i+1, Memc[inname], SZ_FNAME) == EOF)
			call error (1, "Premature end of input list")

		    # Map the input, output, and WCS.
		    ptr = immap (Memc[inname], READ_ONLY, 0); in = ptr
		    ptr = mw_openim (in); mw = ptr
		    call sprintf (Memc[outname], SZ_FNAME, "%s%d")
			call pargstr (Memc[tmp])
			call pargi (i)
		    ptr = immap (Memc[outname], NEW_COPY, in); out = ptr
		    call imastr (out, "ICFNAME", Memc[inname])

		    # Set masks.
		    if (mtype > 1) {
			ptr = yt_mappm ("BPM", in,"logical", Memc[minname],
			    SZ_FNAME)
		        pmin = ptr
			if (pmin != NULL) {
			    call sprintf (Memc[moutname], SZ_FNAME, "m%s%d.pl")
				call pargstr (Memc[tmp])
				call pargi (i)
			    call xt_maskname (Memc[moutname], "", NEW_IMAGE,
				Memc[moutname], SZ_FNAME)
			    ptr = immap (Memc[moutname], NEW_COPY, in)
			    pmout = ptr
			    call imastr (out, "BPM", Memc[moutname])
			    call imastr (pmout, "ICBPM", Memc[minname])
			}
		    }

		    # Use the TRANSFORM routines.
		    call tr_gwcs (mw, un, IM_LEN(in,1), IM_LEN(in,2), ct,
		        usf, nusf, vsf, nvsf)
		    call tr_setup (ct, usf, nusf, vsf, nvsf, un, xmsi, ymsi,
		        jmsi, xout, yout, dxout, dyout)

		    call tr_transform (in, out, pmin, pmout, un, xmsi, ymsi,
		        jmsi, Memr[xout], Memr[yout], Memr[dxout], Memr[dyout])

		    # Finish up.
		    call mw_close (mw)
		    if (pmout != NULL)
			call imunmap (pmout)
		    if (pmin != NULL)
			call xt_pmunmap (pmin)
		    call imunmap (out)
		    call imunmap (in)
		    call mfree (xout, TY_REAL)
		    call mfree (yout, TY_REAL)
		    call mfree (dxout, TY_REAL)
		    call mfree (dyout, TY_REAL)
		    call msifree (xmsi)
		    call msifree (ymsi)
		    if (jmsi != NULL)
			call msifree (jmsi)
		    if (un[1] != NULL)
			call un_close (un[1])
		    if (un[2] != NULL)
			call un_close (un[2])
		}
	    }

	} then {
	    # Save error for later reporting after cleaning up.
	    err = errget (Memc[inname], SZ_FNAME)

	    if (mw != NULL)
	        call mw_close (mw)
	    if (pmout != NULL)
		call imunmap (pmout)
	    if (pmin != NULL)
		call xt_pmunmap (pmin)
	    if (out != NULL)
	        call imunmap (out)
	    if (in != NULL)
	        call imunmap (in)
	    call mfree (xout, TY_REAL)
	    call mfree (yout, TY_REAL)
	    call mfree (dxout, TY_REAL)
	    call mfree (dyout, TY_REAL)
	    if (xmsi != NULL)
		call msifree (xmsi)
	    if (ymsi != NULL)
		call msifree (ymsi)
	    if (jmsi != NULL)
		call msifree (jmsi)
	    if (un[1] != NULL)
		call un_close (un[1])
	    if (un[2] != NULL)
		call un_close (un[2])

	    # Open the temporary list, delete any found, and report err.
	    call sprintf (Memc[outname], SZ_FNAME, "%s*,m%s*.pl")
	        call pargstr (Memc[tmp])
	        call pargstr (Memc[tmp])
	    output = imtopen (Memc[outname])
	    while (imtgetim (output, Memc[outname], SZ_FNAME) != EOF)
		iferr (call imdelete (Memc[outname]))
		    ;
	    call imtclose (output)
	    masks = NULL

	    call error (err, Memc[inname])
	}

	# Set the list to combine.  If the input did not need to be
	# transformed return the input pointer as the output pointer.
	# The calling program can check for equality to decided whether
	# to delete the temporary image.

	if (dotransform) {
	    call sprintf (Memc[outname], SZ_FNAME, "%s*")
	        call pargstr (Memc[tmp])
	    output = imtopen (Memc[outname])
	    call sprintf (Memc[outname], SZ_FNAME, "m%s*.pl")
	        call pargstr (Memc[tmp])
	    masks = imtopen (Memc[outname])
	} else
	    output = input

	call sfree (sp)
end
