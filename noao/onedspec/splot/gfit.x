include	<error.h>
include	<mach.h>
include	<gset.h>

define	SQ2PI	2.5066283
define	MC_N	50	# Monte-Carlo samples
define	MC_SIG	34	# Sigma sample point

# GFIT -- Fit Gaussian

procedure gfit (sh, gfd, wx1, wy1, wcs, pix, n, fd1, fd2, xg, yg, sg, ng)

pointer	sh			# SHDR pointer
pointer	gfd			# GIO file descriptor
real	wx1, wy1		# Cursor position
real	wcs[n]			# Spectrum data
real	pix[n]			# Spectrum data
int	n			# Number of points
int	fd1, fd2		# Output file descriptors
pointer	xg, yg, sg		# Pointers to fit parameters
int	ng			# Number of components

int	bkgfit, posfit, sigfit, nsub
int	i, j, i1, npts, nlines, wc, key
long	seed
real	w, dw, wyc, wx, wy, wx2, wy2
real	slope, height, flux, cont, sigma, eqw, scale, sscale, chisq
real	sigma0, invgain, wyc1, slope1, flux1, cont1, eqw1
bool	fit
pointer	xg1, yg1, sg1
pointer	sp, cmd, x, y, s, z, ym, conte, xge, yge, sge, fluxe, eqwe

int	clgcur()
real	clgetr(), model(), gasdev(), asumr()
errchk	dofit

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
		sigma0 = INDEF
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
	} else if (ng != nlines) {
	    call realloc (xg, nlines, TY_REAL)
	    call realloc (yg, nlines, TY_REAL)
	    call realloc (sg, nlines, TY_REAL)
	}
	ng = nlines

	# Do fit.
	posfit = 2
	sigfit = 2
	bkgfit = 0

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
	    wy = min (0.99, max (0.01, abs (Memr[y+j-1] - wyc-slope*w) / wx))
	    sigma = sqrt (-0.5 * (w-Memr[xg])**2 / log (wy))
	    w = Memr[x+j+1]
	    wy = min (0.99, max (0.01, abs (Memr[y+j+1] - wyc-slope*w) / wx))
	    sigma = sigma + sqrt (-0.5 * (w-Memr[xg])**2 / log (wy))
	    Memr[sg] = sigma / 2
	} else {
	    Memr[sg] = 0.2 * abs (Memr[x+npts-1] - Memr[x])
	}

	nsub = 3
	dw = (wcs[n] - wcs[1]) / (n - 1)
	iferr (call dofit (bkgfit, posfit, 3, sigfit, Memr[x], Memr[y],
	    Memr[s], npts, dw, nsub, wyc, slope, Memr[xg], Memr[yg],
	    Memr[sg], ng, chisq)) {
	    fit = false
	    goto done_
	}

	# Compute Monte-Carlo errors.
	if (!IS_INDEF(sigma0)) {
	    call salloc (ym, npts, TY_REAL)
	    call salloc (xg1, ng, TY_REAL)
	    call salloc (yg1, ng, TY_REAL)
	    call salloc (sg1, ng, TY_REAL)
	    call salloc (conte, MC_N*ng, TY_REAL)
	    call salloc (xge, MC_N*ng, TY_REAL)
	    call salloc (yge, MC_N*ng, TY_REAL)
	    call salloc (sge, MC_N*ng, TY_REAL)
	    call salloc (fluxe, MC_N*ng, TY_REAL)
	    call salloc (eqwe, MC_N*ng, TY_REAL)
	    do i = 1, npts {
		w = Memr[x+i-1]
		Memr[ym+i-1] = model (w, dw, nsub, Memr[xg], Memr[yg],
		    Memr[sg], ng) + wyc + slope * w
	    }
	    seed = 1
	    do i = 0, MC_N-1 {
		do j = 1, npts
		    Memr[y+j-1] = Memr[ym+j-1] +
			sscale / scale * Memr[s+j-1] * gasdev (seed)
		wyc1 = wyc
		slope1 = slope
		call amovr (Memr[xg], Memr[xg1], ng)
		call amovr (Memr[yg], Memr[yg1], ng)
		call amovr (Memr[sg], Memr[sg1], ng)
		call dofit (bkgfit, posfit, 3, sigfit, Memr[x],
		    Memr[y], Memr[s], npts, dw, nsub, wyc1, slope1,
		    Memr[xg1], Memr[yg1], Memr[sg1], ng, chisq)

		do j = 0, ng-1 {
		    cont = wyc + slope * Memr[xg+j]
		    cont1 = wyc1 + slope1 * Memr[xg+j]
		    flux = Memr[sg+j] * Memr[yg+j] * SQ2PI
		    flux1 = Memr[sg1+j] * Memr[yg1+j] * SQ2PI
		    if (cont > 0. && cont1 > 0.) {
			eqw = -flux / cont
			eqw1 = -flux1 / cont1
		    } else {
			eqw = 0.
			eqw1 = 0.
		    }
		    Memr[conte+j*MC_N+i] = abs (cont1 - cont)
		    Memr[xge+j*MC_N+i] = abs (Memr[xg1+j] - Memr[xg+j])
		    Memr[yge+j*MC_N+i] = abs (Memr[yg1+j] - Memr[yg+j])
		    Memr[sge+j*MC_N+i] = abs (Memr[sg1+j] - Memr[sg+j])
		    Memr[fluxe+j*MC_N+i] = abs (flux1 - flux)
		    Memr[eqwe+j*MC_N+i] = abs (eqw1 - eqw)
		}
	    }
	    do j = 0, ng-1 {
		call asrtr (Memr[conte+j*MC_N], Memr[conte+j*MC_N], MC_N)
		call asrtr (Memr[xge+j*MC_N], Memr[xge+j*MC_N], MC_N)
		call asrtr (Memr[yge+j*MC_N], Memr[yge+j*MC_N], MC_N)
		call asrtr (Memr[sge+j*MC_N], Memr[sge+j*MC_N], MC_N)
		call asrtr (Memr[fluxe+j*MC_N], Memr[fluxe+j*MC_N], MC_N)
		call asrtr (Memr[eqwe+j*MC_N], Memr[eqwe+j*MC_N], MC_N)
	    }
	    call amulkr (Memr[conte], scale, Memr[conte], MC_N*ng)
	    call amulkr (Memr[yge], scale, Memr[yge], MC_N*ng)
	    call amulkr (Memr[fluxe], scale, Memr[fluxe], MC_N*ng)
	}

	call amulkr (Memr[yg], scale, Memr[yg], ng)
	wyc = (wyc + slope * wx1) * scale
	slope = slope * scale

	# Compute model spectrum with continuum and plot.
	fit = true
	do i = 1, npts {
	    w = wcs[i1+i-1]
	    Memr[z+i-1] = model (w, dw, nsub, Memr[xg], Memr[yg],
		Memr[sg], ng) + wyc + slope * (w - wx1)
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
	if (fit) {
	    do i = 1, nlines {
		w = Memr[xg+i-1]
		cont = wyc + slope * (w - wx1)
		height = Memr[yg+i-1]
		sigma = Memr[sg+i-1]
		flux = sigma * height * SQ2PI
		if (cont > 0.)
		    eqw = -flux / cont
		else
		    eqw = INDEF

		call printf (
		    "center = %8.6g, flux = %8.4g, eqw = %6.4g, fwhm = %6.4g")
		    call pargr (w)
		    call pargr (flux)
		    call pargr (eqw)
		    call pargr (2.355 * sigma)
		if (fd1 != NULL) {
		    call fprintf (fd1,
			" %9.7g %9.7g %9.6g %9.4g %9.6g %9.4g %9.4g\n")
			call pargr (w)
			call pargr (cont)
			call pargr (flux)
			call pargr (eqw)
			call pargr (height)
			call pargr (sigma)
			call pargr (2.355 * sigma)
		}
		if (fd2 != NULL) {
		    call fprintf (fd2,
			" %9.7g %9.7g %9.6g %9.4g %9.6g %9.4g %9.4g\n")
			call pargr (w)
			call pargr (cont)
			call pargr (flux)
			call pargr (eqw)
			call pargr (height)
			call pargr (sigma)
			call pargr (2.355 * sigma)
		}
		if (!IS_INDEF(sigma0)) {
		    if (fd1 != NULL) {
			call fprintf (fd1,
		"  (%7.5g) (%7w) (%7.4g) (%7.4g) (%7.4g) (%7.4g) (%7.4g)\n")
			    call pargr (Memr[xge+(i-1)*MC_N+MC_SIG])
			    call pargr (Memr[fluxe+(i-1)*MC_N+MC_SIG])
			    call pargr (Memr[eqwe+(i-1)*MC_N+MC_SIG])
			    call pargr (Memr[yge+(i-1)*MC_N+MC_SIG])
			    call pargr (Memr[sge+(i-1)*MC_N+MC_SIG])
			    call pargr (2.355*Memr[sge+(i-1)*MC_N+MC_SIG])
		    }
		    if (fd2 != NULL) {
			call fprintf (fd2,
		"  (%7.5g) (%7w) (%7.4g) (%7.4g) (%7.4g) (%7.4g) (%7.4g)\n")
			    call pargr (Memr[xge+(i-1)*MC_N+MC_SIG])
			    call pargr (Memr[fluxe+(i-1)*MC_N+MC_SIG])
			    call pargr (Memr[eqwe+(i-1)*MC_N+MC_SIG])
			    call pargr (Memr[yge+(i-1)*MC_N+MC_SIG])
			    call pargr (Memr[sge+(i-1)*MC_N+MC_SIG])
			    call pargr (2.355 * Memr[sge+(i-1)*MC_N+MC_SIG])
		    }
		}
	    }
	} else {
	    call mfree (xg, TY_REAL)
	    call mfree (yg, TY_REAL)
	    call mfree (sg, TY_REAL)
	    ng = 0
	}

	call sfree (sp)
end
