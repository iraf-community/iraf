include	<error.h>
include	<mach.h>
include	<gset.h>

define	NSUB	3	# Number of pixel subsamples
define	MC_N	50	# Monte-Carlo samples
define	MC_P	10	# Percent done interval (percent)
define	MC_SIG	68	# Sigma sample point (percent)

# GFIT -- Fit Gaussian

procedure gfit (sh, gfd, wx1, wy1, wcs, pix, n, fd1, fd2, xg, yg, sg, lg, pg,ng)

pointer	sh			# SHDR pointer
pointer	gfd			# GIO file descriptor
real	wx1, wy1		# Cursor position
real	wcs[n]			# Spectrum data
real	pix[n]			# Spectrum data
int	n			# Number of points
int	fd1, fd2		# Output file descriptors
pointer	xg, yg, sg, lg, pg	# Pointers to fit parameters
int	ng			# Number of components

int	fit[5], nsub, mc_p, mc_sig, mc_n
int	i, j, i1, npts, nlines, wc, key
long	seed
real	w, dw, wyc, wx, wy, wx2, wy2, v, u
real	slope, peak, flux, cont, gfwhm, lfwhm, eqw, scale, sscale, chisq
real	sigma0, invgain, wyc1, slope1, flux1, cont1, eqw1
bool	fitit
pointer	xg1, yg1, sg1, lg1
pointer	sp, cmd, x, y, s, z, ym, conte, xge, yge, sge, lge, fluxe, eqwe

int	clgeti(), clgcur()
real	clgetr(), model(), gasdev(), asumr()
errchk	dofit, dorefit

define	done_	99

begin
	call smark (sp)
	call salloc (cmd, SZ_FNAME, TY_CHAR)

	# Input cursor is first continuum point now get second continuum point.
	call printf ("k again:")
	if (clgcur ("cursor", wx2, wy2, wc, key, Memc[cmd], SZ_FNAME) == EOF) {
	    call sfree (sp)
	    return
	}

	# Set pixel indices and determine number of points to fit.
	call fixx (sh, wx1, wx2, wy1, wy2, i1, j)
	npts = j - i1 + 1
	if (npts < 3) {
	    call eprintf ("At least 3 points are required\n")
	    call sfree (sp)
	    return
	}

	# Allocate space for the points to be fit.
	call salloc (x, npts, TY_REAL)
	call salloc (y, npts, TY_REAL)
	call salloc (s, npts, TY_REAL)
	call salloc (z, npts, TY_REAL)

	# Scale the data.
	mc_n = clgeti ("nerrsample")
	sigma0 = clgetr ("sigma0")
	invgain = clgetr ("invgain")
	if (IS_INDEF(sigma0) || IS_INDEF(invgain) || sigma0<0. ||
	    invgain<0. || (sigma0 == 0. && invgain == 0.)) {
	    sigma0 = INDEF
	    invgain = INDEF
	}
	scale = 0.
	do i = 1, npts {
	    Memr[x+i-1] = wcs[i1+i-1]
	    Memr[y+i-1] = pix[i1+i-1]
	    if (Memr[y+i-1] <= 0.)
		if (!IS_INDEF(invgain) && invgain != 0.) {
		    sigma0 = INDEF
		    invgain = INDEF
		    call eprintf (
			"WARNING: Cannot compute errors with non-zero gain")
		    call eprintf (
			" and negative pixel values.\n")
		}
	    scale = max (scale, abs (Memr[y+i-1]))
	}
	if (IS_INDEF(sigma0)) {
	    call amovkr (1., Memr[s], npts)
	    sscale = 1.
	} else {
	    do i = 1, npts
		Memr[s+i-1] = sqrt (sigma0 ** 2 + invgain * Memr[y+i-1])
	    sscale = asumr (Memr[s], npts) / npts
	}
	call adivkr (Memr[y], scale, Memr[y], npts)
	call adivkr (Memr[s], sscale, Memr[s], npts)

	# Allocate memory.
	nlines = 1
	if (ng == 0) {
	    call malloc (xg, nlines, TY_REAL)
	    call malloc (yg, nlines, TY_REAL)
	    call malloc (sg, nlines, TY_REAL)
	    call malloc (lg, nlines, TY_REAL)
	    call malloc (pg, nlines, TY_INT)
	} else if (ng != nlines) {
	    call realloc (xg, nlines, TY_REAL)
	    call realloc (yg, nlines, TY_REAL)
	    call realloc (sg, nlines, TY_REAL)
	    call realloc (lg, nlines, TY_REAL)
	    call realloc (pg, nlines, TY_INT)
	}
	ng = nlines

	# Do fit.
	fit[1] = 1
	fit[2] = 2
	fit[3] = 2
	fit[4] = 2
	fit[5] = 2

	# Setup initial estimates.
	slope = (wy2-wy1) / (wx2-wx1) / scale
	wyc = wy1 / scale - slope * wx1
	wx = 0
	do i = 0, npts-1 {
	    w = Memr[x+i]
	    wy = Memr[y+i] - wyc - slope * w
	    if (abs (wy) > wx) {
		wx = abs (wy)
		j = i
		Memr[xg] = w
		Memr[yg] = wy
	    }
	}

	if (j > 0 && j < npts-1) {
	    w = Memr[x+j-1]
	    wy = min (0.99, max (0.01, abs (Memr[y+j-1] - wyc - slope*w) / wx))
	    gfwhm = 2.355 * sqrt (-0.5 * (w-Memr[xg])**2 / log (wy))
	    w = Memr[x+j+1]
	    wy = min (0.99, max (0.01, abs (Memr[y+j+1] - wyc - slope*w) / wx))
	    gfwhm = (gfwhm + 2.355 * sqrt (-0.5*(w-Memr[xg])**2/log (wy))) / 2
	} else
	    gfwhm = 0.3 * abs (Memr[x+npts-1] - Memr[x])

	switch (key) {
	case 'l':
	    Memr[sg] = 0.
	    Memr[lg] = gfwhm
	    Memi[pg] = 2
	case 'v':
	    Memr[sg] = 0.5 * gfwhm
	    Memr[lg] = 0.5 * gfwhm
	    Memi[pg] = 3
	default:
	    Memr[sg] = gfwhm
	    Memr[lg] = 0.
	    Memi[pg] = 1
	}

	nsub = NSUB
	dw = (wcs[n] - wcs[1]) / (n - 1)
	iferr (call dofit (fit, Memr[x], Memr[y], Memr[s], npts, dw, nsub,
	    wyc, slope, Memr[xg], Memr[yg], Memr[sg], Memr[lg], Memi[pg],
	    ng, chisq)) {
	    fitit = false
	    goto done_
	}

	# Compute Monte-Carlo errors.
	if (mc_n > 9 && !IS_INDEF(sigma0)) {
	    mc_p = nint (mc_n * MC_P / 100.)
	    mc_sig = nint (mc_n * MC_SIG / 100.)

	    call salloc (ym, npts, TY_REAL)
	    call salloc (xg1, ng, TY_REAL)
	    call salloc (yg1, ng, TY_REAL)
	    call salloc (sg1, ng, TY_REAL)
	    call salloc (lg1, ng, TY_REAL)
	    call salloc (conte, mc_n*ng, TY_REAL)
	    call salloc (xge, mc_n*ng, TY_REAL)
	    call salloc (yge, mc_n*ng, TY_REAL)
	    call salloc (sge, mc_n*ng, TY_REAL)
	    call salloc (lge, mc_n*ng, TY_REAL)
	    call salloc (fluxe, mc_n*ng, TY_REAL)
	    call salloc (eqwe, mc_n*ng, TY_REAL)
	    do i = 1, npts {
		w = Memr[x+i-1]
		Memr[ym+i-1] = model (w, dw, nsub, Memr[xg], Memr[yg],
		    Memr[sg], Memr[lg], Memi[pg], ng) + wyc + slope * w
	    }
	    seed = 1
	    do i = 0, mc_n-1 {
		if (i > 0 && mod (i, mc_p) == 0) {
		    call printf ("%2d ")
			call pargi (100 * i / mc_n)
		    call flush (STDOUT)
		}
		do j = 1, npts
		    Memr[y+j-1] = Memr[ym+j-1] +
			sscale / scale * Memr[s+j-1] * gasdev (seed)
		wyc1 = wyc
		slope1 = slope
		call amovr (Memr[xg], Memr[xg1], ng)
		call amovr (Memr[yg], Memr[yg1], ng)
		call amovr (Memr[sg], Memr[sg1], ng)
		call amovr (Memr[lg], Memr[lg1], ng)
		call dorefit (fit, Memr[x], Memr[y], Memr[s], npts,
		    dw, nsub, wyc1, slope1,
		    Memr[xg1], Memr[yg1], Memr[sg1], Memr[lg1], Memi[pg], ng,
		    chisq)

		do j = 0, ng-1 {
		    cont = wyc + slope * Memr[xg+j]
		    cont1 = wyc1 + slope1 * Memr[xg+j]
		    switch (Memi[pg+j]) {
		    case 1:
			flux = 1.064467 * Memr[yg+j] * Memr[sg+j]
			flux1 = 1.064467 * Memr[yg1+j] * Memr[sg1+j]
		    case 2:
			flux = 1.570795 * Memr[yg+j] * Memr[lg+j]
			flux1 = 1.570795 * Memr[yg1+j] * Memr[lg1+j]
		    case 3:
			call voigt (0., 0.832555*Memr[lg+j]/Memr[sg+j], v, u)
			flux = 1.064467 * Memr[yg+j] * Memr[sg+j] / v
			call voigt (0., 0.832555*Memr[lg1+j]/Memr[sg1+j], v, u)
			flux1 = 1.064467 * Memr[yg1+j] * Memr[sg1+j] / v
		    }
		    if (cont > 0. && cont1 > 0.) {
			eqw = -flux / cont
			eqw1 = -flux1 / cont1
		    } else {
			eqw = 0.
			eqw1 = 0.
		    }
		    Memr[conte+j*mc_n+i] = abs (cont1 - cont)
		    Memr[xge+j*mc_n+i] = abs (Memr[xg1+j] - Memr[xg+j])
		    Memr[yge+j*mc_n+i] = abs (Memr[yg1+j] - Memr[yg+j])
		    Memr[sge+j*mc_n+i] = abs (Memr[sg1+j] - Memr[sg+j])
		    Memr[lge+j*mc_n+i] = abs (Memr[lg1+j] - Memr[lg+j])
		    Memr[fluxe+j*mc_n+i] = abs (flux1 - flux)
		    Memr[eqwe+j*mc_n+i] = abs (eqw1 - eqw)
		}
	    }
	    do j = 0, ng-1 {
		call asrtr (Memr[conte+j*mc_n], Memr[conte+j*mc_n], mc_n)
		call asrtr (Memr[xge+j*mc_n], Memr[xge+j*mc_n], mc_n)
		call asrtr (Memr[yge+j*mc_n], Memr[yge+j*mc_n], mc_n)
		call asrtr (Memr[sge+j*mc_n], Memr[sge+j*mc_n], mc_n)
		call asrtr (Memr[lge+j*mc_n], Memr[lge+j*mc_n], mc_n)
		call asrtr (Memr[fluxe+j*mc_n], Memr[fluxe+j*mc_n], mc_n)
		call asrtr (Memr[eqwe+j*mc_n], Memr[eqwe+j*mc_n], mc_n)
	    }
	    call amulkr (Memr[conte], scale, Memr[conte], mc_n*ng)
	    call amulkr (Memr[yge], scale, Memr[yge], mc_n*ng)
	    call amulkr (Memr[fluxe], scale, Memr[fluxe], mc_n*ng)
	}

	call amulkr (Memr[yg], scale, Memr[yg], ng)
	wyc = (wyc + slope * wx1) * scale
	slope = slope * scale

	# Compute model spectrum with continuum and plot.
	fitit = true
	do i = 1, npts {
	    w = wcs[i1+i-1]
	    Memr[z+i-1] = model (w, dw, nsub, Memr[xg], Memr[yg],
		Memr[sg], Memr[lg], Memi[pg], ng) + wyc + slope * (w - wx1)
	}

	call gseti (gfd, G_PLTYPE, 2)
	call gseti (gfd, G_PLCOLOR, 2)
	call gpline (gfd, wcs[i1], Memr[z], npts)
	call gseti (gfd, G_PLTYPE, 3)
	call gseti (gfd, G_PLCOLOR, 3)
	call gline (gfd, wx1, wyc, wx2, wyc + slope * (wx2 - wx1))
	call gseti (gfd, G_PLTYPE, 1)
	call gseti (gfd, G_PLCOLOR, 1)
	call gflush (gfd)

done_
	# Log computed values
	if (fitit) {
	    do i = 1, nlines {
		w = Memr[xg+i-1]
		cont = wyc + slope * (w - wx1)
		peak = Memr[yg+i-1]
		gfwhm = Memr[sg+i-1]
		lfwhm = Memr[lg+i-1]
		switch (Memi[pg+i-1]) {
		case 1:
		    flux = 1.064467 * peak * gfwhm
		    if (cont > 0.)
			eqw = -flux / cont
		    else
			eqw = INDEF
		    call printf (
	"\n%d: center = %8.6g, flux = %8.4g, eqw = %6.4g, gfwhm = %6.4g")
			call pargi (i)
			call pargr (w)
			call pargr (flux)
			call pargr (eqw)
			call pargr (gfwhm)
		case 2:
		    flux = 1.570795 * peak * lfwhm
		    if (cont > 0.)
			eqw = -flux / cont
		    else
			eqw = INDEF
		    call printf (
	"\n%d: center = %8.6g, flux = %8.4g, eqw = %6.4g, lfwhm = %6.4g")
			call pargi (i)
			call pargr (w)
			call pargr (flux)
			call pargr (eqw)
			call pargr (lfwhm)
		case 3:
		    call voigt (0., 0.832555*lfwhm/gfwhm, v, u)
		    flux = 1.064467 * peak * gfwhm / v
		    if (cont > 0.)
			eqw = -flux / cont
		    else
			eqw = INDEF
		    call printf (
	"\n%d: center = %8.6g, eqw = %6.4g, gfwhm = %6.4g, lfwhm = %6.4g")
			call pargi (i)
			call pargr (w)
			call pargr (eqw)
			call pargr (gfwhm)
			call pargr (lfwhm)
		}
		if (fd1 != NULL) {
		    call fprintf (fd1,
			" %9.7g %9.7g %9.6g %9.4g %9.6g %9.4g %9.4g\n")
			call pargr (w)
			call pargr (cont)
			call pargr (flux)
			call pargr (eqw)
			call pargr (peak)
			call pargr (gfwhm)
			call pargr (lfwhm)
		}
		if (fd2 != NULL) {
		    call fprintf (fd2,
			" %9.7g %9.7g %9.6g %9.4g %9.6g %9.4g %9.4g\n")
			call pargr (w)
			call pargr (cont)
			call pargr (flux)
			call pargr (eqw)
			call pargr (peak)
			call pargr (gfwhm)
			call pargr (lfwhm)
		}
		if (mc_n > 9 && !IS_INDEF(sigma0)) {
		    if (fd1 != NULL) {
			call fprintf (fd1,
		"  (%7.5g) (%7w) (%7.4g) (%7.4g) (%7.4g) (%7.4g) (%7.4g)\n")
			    call pargr (Memr[xge+(i-1)*mc_n+mc_sig])
			    call pargr (Memr[fluxe+(i-1)*mc_n+mc_sig])
			    call pargr (Memr[eqwe+(i-1)*mc_n+mc_sig])
			    call pargr (Memr[yge+(i-1)*mc_n+mc_sig])
			    call pargr (Memr[sge+(i-1)*mc_n+mc_sig])
			    call pargr (Memr[lge+(i-1)*mc_n+mc_sig])
		    }
		    if (fd2 != NULL) {
			call fprintf (fd2,
		"  (%7.5g) (%7w) (%7.4g) (%7.4g) (%7.4g) (%7.4g) (%7.4g)\n")
			    call pargr (Memr[xge+(i-1)*mc_n+mc_sig])
			    call pargr (Memr[fluxe+(i-1)*mc_n+mc_sig])
			    call pargr (Memr[eqwe+(i-1)*mc_n+mc_sig])
			    call pargr (Memr[yge+(i-1)*mc_n+mc_sig])
			    call pargr (Memr[sge+(i-1)*mc_n+mc_sig])
			    call pargr (Memr[lge+(i-1)*mc_n+mc_sig])
		    }
		}
	    }
	} else {
	    call mfree (xg, TY_REAL)
	    call mfree (yg, TY_REAL)
	    call mfree (sg, TY_REAL)
	    call mfree (lg, TY_REAL)
	    call mfree (pg, TY_INT)
	    ng = 0
	}

	call sfree (sp)
end
