include	<math.h>
include	<error.h>
include	<gset.h>

define	NSUB	3	# Number of pixel subsamples
define	MC_N	50	# Monte-Carlo samples
define	MC_P	10	# Percent done interval (percent)
define	MC_SIG	68	# Sigma sample point (percent)

# Profile types.
define  PTYPES  "|gaussian|lorentzian|voigt|"
define  GAUSS           1       # Gaussian profile
define  LORENTZ         2       # Lorentzian profile
define  VOIGT           3       # Voigt profile


# SP_DEBLEND -- Deblend lines in a spectral region.

procedure sp_deblend (sh, gfd, wx1, wy1, wcs, pix, n, fd1, fd2,
	xg, yg, sg, lg, pg, ng)

pointer	sh			# SHDR pointer
pointer	gfd			# GIO file descriptor
real	wx1, wy1		# Cursor position
real	wcs[n]			# Coordinates
real	pix[n]			# Spectrum data
int	n			# Number of points
int	fd1, fd2		# Output file descriptors
pointer	xg, yg, sg, lg, pg	# Pointers to fit parameters
int	ng			# Number of components

int	fit[5], nsub, mc_p, mc_sig, mc_n
int	i, j, i1, npts, nlines, maxlines, wc, key, type, ifit
long	seed
real	w, dw, wyc, wx, wy, wx2, wy2, u, v
real	slope, peak, flux, cont, gfwhm, lfwhm, eqw, scale, sscale, chisq, rms
real	sigma0, invgain, wyc1, slope1, flux1, cont1, eqw1
bool	fitit, fitg, fitl
pointer	xg1, yg1, sg1, lg1
pointer	sp, cmd, x, y, s, z, waves, types, gfwhms, lfwhms, peaks, ym
pointer	conte, xge, yge, sge, lge, fluxe, eqwe

int	clgeti(), clgcur(), open(), fscan(), nscan(), strdic()
real	clgetr(), model(), gasdev(), asumr()
double	shdr_wl()
errchk	dofit, dorefit

define	fitp_	95
define	fitg_	96
define	fitl_	97
define	fitb_	98
define	done_	99

begin
	call smark (sp)
	call salloc (cmd, SZ_FNAME, TY_CHAR)

	# Input cursor is first continuum point now get second continuum point.
	call printf ("d again:")
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
	mc_n = clgeti ("nerrsample")
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
		if (invgain != 0.) {
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

	# Select the lines to be fit.  If no lines return.
	fitit = false
	fitg = false
	fitl = false
	maxlines = 5
	call malloc (waves, maxlines, TY_REAL)
	call malloc (peaks, maxlines, TY_REAL)
	call malloc (gfwhms, maxlines, TY_REAL)
	call malloc (lfwhms, maxlines, TY_REAL)
	call malloc (types, maxlines, TY_INT)
	nlines = 0
	call printf (
	    "Lines ('f'ile, 'g'aussian, 'l'orentzian, 'v'oigt, 't'ype, 'q'uit:")
	while (clgcur ("cursor", wx, wy, wc, key, Memc[cmd], SZ_FNAME) != EOF) {
	    switch (key) {
	    case 'f':
		call clgstr ("linelist", Memc[cmd], SZ_FNAME)
		call printf (
		    "Lines ('f'ile, 'g'aussian, 'l'orentzian, 'v'oigt, 't'ype, 'q'uit:")
		iferr (j = open (Memc[cmd], READ_ONLY, TEXT_FILE)) {
		    call erract (EA_WARN)
		    next
		}
		while (fscan (j) != EOF) {
		    call gargr (wx)
		    if (nscan() < 1)
			next
		    if (wx < min (wcs[1], wcs[n]) || wx > max (wcs[1], wcs[n]))
			next
                    call gargr (peak)
                    call gargwrd (Memc[cmd], SZ_FNAME)
                    call gargr (gfwhm)
                    call gargr (lfwhm)
                    type = strdic (Memc[cmd], Memc[cmd], SZ_FNAME, PTYPES)
		    if (type == 0)
			type = GAUSS
		    switch (nscan()) {
		    case 0:
			next
		    case 1:
			peak = INDEF
			type = GAUSS
			gfwhm = INDEF
			lfwhm = INDEF
		    case 2:
			type = GAUSS
			gfwhm = INDEF
			lfwhm = INDEF
		    case 3:
			gfwhm = INDEF
			lfwhm = INDEF
		    case 4:
			switch (type) {
			case GAUSS:
			    lfwhm = INDEF
			case LORENTZ:
			    lfwhm = gfwhm
			    gfwhm = INDEF
			case VOIGT:
			    lfwhm = INDEF
			}
		    }
		    for (i = 0; i < nlines && wx != Memr[waves+i]; i = i + 1)
			;
		    if (i == nlines) {
			if (nlines == maxlines) {
			    maxlines = maxlines + 5
			    call realloc (waves, maxlines, TY_REAL)
			    call realloc (peaks, maxlines, TY_REAL)
			    call realloc (gfwhms, maxlines, TY_REAL)
			    call realloc (lfwhms, maxlines, TY_REAL)
			    call realloc (types, maxlines, TY_INT)
			}
			Memr[waves+i] = wx
			Memr[peaks+i] = peak
			Memr[gfwhms+i] = gfwhm
			Memr[lfwhms+i] = lfwhm
			Memi[types+i] = type
			switch (type) {
			case GAUSS:
			    fitg = true
			case LORENTZ:
			    fitl = true
			case VOIGT:
			    fitg = true
			    fitl = true
			}
			nlines = nlines + 1
			call gmark (gfd, wx, wy, GM_VLINE, 3., 3.)
		    }
		}
		call close (j)
		next
	    case 'g':
		type = GAUSS
		peak = INDEF
		gfwhm = INDEF
		lfwhm = INDEF
	    case 'l':
		type = LORENTZ
		peak = INDEF
		gfwhm = INDEF
		lfwhm = INDEF
	    case 'v':
		type = VOIGT
		peak = INDEF
		gfwhm = INDEF
		lfwhm = INDEF
	    case 't':
		type = GAUSS
		wx = clgetr ("wavelength")
		peak = INDEF
		gfwhm = INDEF
		lfwhm = INDEF
		call printf (
		"Lines ('f'ile, 'g'aussian, 'l'orentzian, 'v'oigt, 't'ype, 'q'uit:")
	    case 'q':
		call printf ("\n")
		break
	    case 'I':
		call fatal (0, "Interrupt")
	    default:
		call printf (
		"Lines ('f'ile, 'g'aussian, 'l'orentzian, 'v'oigt, 't'ype, 'q'uit:\007")
		next
	    }
	    for (i = 0; i < nlines && wx != Memr[waves+i]; i = i + 1)
		;
	    if (i == nlines) {
		if (nlines == maxlines) {
		    maxlines = maxlines + 5
		    call realloc (waves, maxlines, TY_REAL)
		    call realloc (peaks, maxlines, TY_REAL)
		    call realloc (gfwhms, maxlines, TY_REAL)
		    call realloc (lfwhms, maxlines, TY_REAL)
		    call realloc (types, maxlines, TY_INT)
		}
		Memr[waves+i] = wx
		Memr[peaks+i] = peak
		Memr[gfwhms+i] = gfwhm
		Memr[lfwhms+i] = lfwhm
		Memi[types+i] = type
		switch (type) {
		case GAUSS:
		    fitg = true
		case LORENTZ:
		    fitl = true
		case VOIGT:
		    fitg = true
		    fitl = true
		}
		nlines = nlines + 1
		call gmark (gfd, wx, wy, GM_VLINE, 3., 3.)
	    }
	}
	if (nlines == 0)
	    goto done_

	# Allocate memory.
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

	# Do fits.
	ifit = 0
	fit[3] = 3
	repeat {
fitp_       call printf ("Fit positions (fixed, single, all, quit) ")
	    if (clgcur ("cursor", wx, wy, wc, key, Memc[cmd], SZ_FNAME) == EOF)
		break
	    switch (key) {
	    case 'f':
		fit[2] = 1
	    case 's':
		fit[2] = 2
	    case 'a':
		fit[2] = 3
	    case 'q':
		break
	    default:
		goto fitp_
	    }
	    if (fitg) {
fitg_		call printf ("Fit Gaussian widths (fixed, single, all, quit) ")
		if (clgcur("cursor", wx, wy, wc, key, Memc[cmd], SZ_FNAME)==EOF)
		    break
		switch (key) {
		case 'f':
		    fit[4] = 1
		case 's':
		    fit[4] = 2
		case 'a':
		    fit[4] = 3
		case 'q':
		    break
		default:
		    goto fitg_
		}
	    }
	    if (fitl) {
fitl_		call printf (
		    "Fit Lorentzian widths (fixed, single, all, quit) ")
		if (clgcur("cursor", wx, wy, wc, key, Memc[cmd], SZ_FNAME)==EOF)
		    break
		switch (key) {
		case 'f':
		    fit[5] = 1
		case 's':
		    fit[5] = 2
		case 'a':
		    fit[5] = 3
		case 'q':
		    break
		default:
		    goto fitl_
		}
	    }
fitb_	    call printf ("Fit background (no, yes, quit) ")
	    if (clgcur ("cursor", wx, wy, wc, key, Memc[cmd], SZ_FNAME) == EOF)
		break
	    switch (key) {
	    case 'n':
		fit[1] = 1
	    case 'y':
		fit[1] = 2
	    case 'q':
		break
	    default:
		goto fitb_
	    }
	    call printf ("Fitting...")
	    call flush (STDOUT)

	    # Setup initial estimates.
	    if (ifit == 0) {
		slope = (wy2-wy1) / (wx2-wx1) / scale
		wyc = wy1 / scale - slope * wx1
		eqw = abs (Memr[x+npts-1] - Memr[x]) / nlines
		do i = 0, nlines-1 {
		    w = Memr[waves+i]
		    peak = Memr[peaks+i]
		    gfwhm = Memr[gfwhms+i]
		    lfwhm = Memr[lfwhms+i]
		    type = Memi[types+i]
		    j = max (1, min (n, nint (shdr_wl (sh, double(w)))))
		    Memr[xg+i] = w
		    if (IS_INDEF(peak))
			Memr[yg+i] = pix[j] / scale - wyc - slope * w
		    else
			Memr[yg+i] = peak / scale
		    Memr[sg+i] = 0.
		    Memr[lg+i] = 0.
		    Memi[pg+i] = type
		    switch (type) {
		    case GAUSS:
			if (IS_INDEF(gfwhm))
			    Memr[sg+i] = 0.3 * eqw
			else
			    Memr[sg+i] = gfwhm
		    case LORENTZ:
			if (IS_INDEF(lfwhm))
			    Memr[lg+i] = 0.3 * eqw
			else
			    Memr[lg+i] = lfwhm
		    case VOIGT:
			if (IS_INDEF(Memr[gfwhms+i]))
			    Memr[sg+i] = 0.1 * eqw
			else
			    Memr[sg+i] = gfwhm
			if (IS_INDEF(Memr[lfwhms+i]))
			    Memr[lg+i] = 0.1 * eqw
			else
			    Memr[lg+i] = lfwhm
		    }
		}
	    } else {
		call adivkr (Memr[yg], scale, Memr[yg], ng)
		slope = slope / scale
		wyc = wyc / scale - slope * wx1
	    }

	    nsub = NSUB
	    dw = (wcs[n] - wcs[1]) / (n - 1)
	    iferr (call dofit (fit, Memr[x], Memr[y], Memr[s], npts,
		dw, nsub, wyc, slope,
		Memr[xg], Memr[yg], Memr[sg], Memr[lg], Memi[pg], ng, chisq)) {
		call erract (EA_WARN)
		next
	    }
	    ifit = ifit + 1

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
			Memr[xg1], Memr[yg1], Memr[sg1], Memr[lg1], Memi[pg],
			ng, chisq)

		    do j = 0, ng-1 {
			cont = wyc + slope * Memr[xg+j]
			cont1 = wyc1 + slope1 * Memr[xg+j]
			switch (Memi[pg+j]) {
			case GAUSS:
			    flux = 1.064467 * Memr[yg+j] * Memr[sg+j]
			    flux1 = 1.064467 * Memr[yg1+j] * Memr[sg1+j]
			case LORENTZ:
			    flux = 1.570795 * Memr[yg+j] * Memr[lg+j]
			    flux1 = 1.570795 * Memr[yg1+j] * Memr[lg1+j]
			case VOIGT:
			    call voigt (0., 0.832555*Memr[lg+j]/Memr[sg+j],
				v, u)
			    flux = 1.064467 * Memr[yg+j] * Memr[sg+j] / v
			    call voigt (0., 0.832555*Memr[lg1+j]/Memr[sg1+j],
				v, u)
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

	    fitit = true

	    # Compute model spectrum with continuum and plot.
	    call printf ("Overplot (total, components, both, none) ")
	    if (clgcur ("cursor", wx, wy, wc, key, Memc[cmd], SZ_FNAME) == EOF)
		break

	    rms = 0.
	    do i = 1, npts {
		w = Memr[x+i-1]
	        Memr[z+i-1] = model (w, dw, nsub, Memr[xg], Memr[yg],
		    Memr[sg], Memr[lg], Memi[pg], ng)
	        Memr[z+i-1] = Memr[z+i-1] + wyc + slope * (w - wx1)
		rms = rms + (Memr[z+i-1] / scale - Memr[y+i-1]) ** 2
	    }

	    # Total.
	    if (key == 't' || key == 'b') {
		call gseti (gfd, G_PLTYPE, 2)
		call gseti (gfd, G_PLCOLOR, 2)
		call gpline (gfd, Memr[x], Memr[z], npts)
		call gseti (gfd, G_PLTYPE, 1)
		call gflush (gfd)
	    }

	    # Components.
	    if (key == 'c' || key == 'b') {
		call gseti (gfd, G_PLTYPE, 3)
		call gseti (gfd, G_PLCOLOR, 5)
		do j = 0, ng-1 {
		    do i = 1, npts {
			w = Memr[x+i-1]
			Memr[z+i-1] = model (w, dw, nsub, Memr[xg+j], Memr[yg+j],
			    Memr[sg+j], Memr[lg+j], Memi[pg+j], 1)
			Memr[z+i-1] = Memr[z+i-1] + wyc + slope * (w - wx1)
		    }
		    call gpline (gfd, Memr[x], Memr[z], npts)
		}
		call gseti (gfd, G_PLTYPE, 1)
		call gflush (gfd)
	    }

	    if (key != 'n') {
		call gseti (gfd, G_PLTYPE, 4)
		call gseti (gfd, G_PLCOLOR, 3)
		call gline (gfd, wx1, wyc, wx2, wyc + slope * (wx2 - wx1))
		call gseti (gfd, G_PLTYPE, 1)
		call gflush (gfd)
	    }


	    # Print computed values on status line.
	    i = 1
	    key = ''
	    repeat {
	        switch (key) {
	        case '-':
		    i = i - 1
		    if (i < 1)
		        i = nlines
	        case '+':
		    i = i + 1
		    if (i > nlines)
		        i = 1
	        case 'q':
		    call printf ("\n")
		    break
	        }

		if (key == 'r') {
		    call printf ("\nrms = %8.4g")
			call pargr (scale * sqrt (chisq / npts))
		} else {
		    w = Memr[xg+i-1]
		    cont = wyc + slope * (w - wx1)
		    peak = Memr[yg+i-1]
		    gfwhm = Memr[sg+i-1]
		    lfwhm = Memr[lg+i-1]
		    switch (Memi[pg+i-1]) {
		    case GAUSS:
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
		    case LORENTZ:
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
		    case VOIGT:
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
		}

	        call printf ("  (+,-,r,q):")
	        call flush (STDOUT)
	    } until (clgcur ("cursor",
		wx, wy, wc, key, Memc[cmd], SZ_FNAME) == EOF)
	}

done_
	call printf ("Deblending done\n")
	# Log computed values
	if (fitit) {
	    do i = 1, nlines {
		w = Memr[xg+i-1]
		cont = wyc + slope * (w - wx1)
		peak = Memr[yg+i-1]
		gfwhm = Memr[sg+i-1]
		lfwhm = Memr[lg+i-1]
		switch (Memi[pg+i-1]) {
		case GAUSS:
		    flux = 1.064467 * peak * gfwhm
		case LORENTZ:
		    flux = 1.570795 * peak * lfwhm
		case VOIGT:
		    call voigt (0., 0.832555*lfwhm/gfwhm, v, u)
		    flux = 1.064467 * peak * gfwhm / v
		}

		if (cont > 0.)
		    eqw = -flux / cont
		else
		    eqw = INDEF
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
	    "  (%7.5g) (%7.5g) (%7.4g) (%7.4g) (%7.4g) (%7.4g) (%7.4g)\n")
			    call pargr (Memr[xge+(i-1)*mc_n+mc_sig])
			    call pargr (Memr[conte+(i-1)*mc_n+mc_sig])
			    call pargr (Memr[fluxe+(i-1)*mc_n+mc_sig])
			    call pargr (Memr[eqwe+(i-1)*mc_n+mc_sig])
			    call pargr (Memr[yge+(i-1)*mc_n+mc_sig])
			    call pargr (Memr[sge+(i-1)*mc_n+mc_sig])
			    call pargr (Memr[lge+(i-1)*mc_n+mc_sig])
		    }
		    if (fd2 != NULL) {
			call fprintf (fd2,
	    "  (%7.5g) (%7.5g) (%7.4g) (%7.4g) (%7.4g) (%7.4g) (%7.4g)\n")
			    call pargr (Memr[xge+(i-1)*mc_n+mc_sig])
			    call pargr (Memr[conte+(i-1)*mc_n+mc_sig])
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

	call mfree (waves, TY_REAL)
	call mfree (peaks, TY_REAL)
	call mfree (gfwhms, TY_REAL)
	call mfree (lfwhms, TY_REAL)
	call mfree (types, TY_INT)
	call sfree (sp)
end


# SUBBLEND -- Subtract last fit.

procedure subblend (sh, gfd, x, y, n, wx1, wy1, xg, yg, sg, lg, pg, ng)

pointer	sh			# SHDR pointer
pointer	gfd			# GIO file descriptor
real	wx1, wy1		# Cursor position
real	x[n]			# Spectrum data
real	y[n]			# Spectrum data
int	n			# Number of points
pointer	xg, yg, sg, lg, pg	# Pointers to fit parameters
int	ng			# Number of components

int	i, j, i1, npts, wc, key, nsub
real	wx2, wy2, dw
pointer	sp, cmd

int	clgcur()
real	model()

begin
	if (ng == 0)
	    return

	call smark (sp)
	call salloc (cmd, SZ_FNAME, TY_CHAR)

	# Determine fit range
	call printf ("- again:")
	call flush (STDOUT)
	if (clgcur ("cursor", wx2, wy2, wc, key, Memc[cmd], SZ_FNAME) == EOF) {
	    call sfree (sp)
	    return
	}

	call fixx (sh, wx1, wx2, wy1, wy2, i1, j)
	npts = j - i1 + 1

	dw = (x[n] - x[1]) / (n - 1)
	nsub = NSUB
	do i = 1, npts {
	    y[i1+i-1] = y[i1+i-1] -
		model (x[i1+i-1], dw, nsub, Memr[xg], Memr[yg], Memr[sg],
		    Memr[lg], Memi[pg], ng)
	}

	# Plot subtracted curve
	call gpline (gfd, x[i1], y[i1], npts)
	call gflush (gfd)

	call mfree (xg, TY_REAL)
	call mfree (yg, TY_REAL)
	call mfree (sg, TY_REAL)
	call mfree (lg, TY_REAL)
	call mfree (pg, TY_INT)
	ng = 0
	call sfree (sp)
end


# GASDEV -- Return a normally distributed deviate with zero mean and unit
# variance.  The method computes two deviates simultaneously.
#
# Copyright(c) 2017 Anastasia Galkin
# Reference: G. E. P. Box and Mervin E. Muller, A Note on the Generation of
#            Random Normal Deviates, The Annals of Mathematical Statistics
#            (1958), Vol. 29, No. 2 pp. 610–611

real procedure gasdev (seed)

long	seed

int	count
data	count/0/

real	u1, u2, x
real	urand()

begin
	if (count == 0) {
		repeat {
			u1 = 2 * urand (seed) - 1.
		} until (u1 > 0)
		repeat {
			u2 = 2 * urand (seed) - 1.
		} until (u1 > 0)
		x = sqrt(-2 * log(u1)) * cos(2*PI*u2);
		count = 1
	} else {
		x = sqrt(-2 * log(u1)) * sin(2*PI*u2);
		count = 0
	}
	return (x)
end
