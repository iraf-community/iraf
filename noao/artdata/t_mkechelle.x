include	<error.h>
include	<imhdr.h>
include	<math.h>

define	LEN_UA		20000		# Maximum user header
define	LEN_COMMENT	70		# Maximum comment length

define	PTYPES	"|extracted|gaussian|slit|"
define	EXTRACTED	1		# Extracted format
define	GAUSS		2		# Gaussian (pexp = 2)
define	SLIT		3		# Slit (pexp = 10)


# T_MKECHELLE -- Make echelle spectra.
# Extracted or full two dimensional formats may be created.
# The spectrum may consist of a constant continuum, a blackbody continuum,
# and emission and absorption lines of varying widths and strengths.
# The spectral features may come from a line list or be randomly generated.
# A redshift may be applied to the spectrum.  The order profiles may
# be either a gaussian or a slit with a specified FWHM.  Both the
# spectral features and the profile are subsampled.  The echelle format
# includes a blaze function corrected for light losses to other other
# other orders and reflected components.  If a focal length is specified
# the wavelength nonlinearity is included.

procedure t_mkechelle()

int	images			# List of echelle spectra to be created
int	nc			# Number of columns (across dispersion)
int	nl			# Number of lines (along dispersion)
int	norders			# Number of orders
int	profile			# Profile type
real	width			# Profile width (pixels)
real	scattered		# Scattered light peak intensity
real	xc, yc			# Central pixel postion
real	pixsize			# Pixel size (mm)

int	mc[2]			# Central order
real	f[2]			# Focal length (mm)
real	gmm[2]			# Grating grooves per mm
real	blaze[2]		# Blaze angle (degrees)
real	t[2]			# Angle from blaze angle
real	wc[2]			# Central wavelength
real	disp[2]			# Central dispersion

real	z			# Redshift
real	cont			# Continuum at central wavelength
real	temp			# Blackbody temperture (Kelvin)
int	lines			# List of files containing lines
int	nrandom			# Number of spectral lines
real	peak			# Peak/continuum
real	sigma			# Sigma of lines (Angstroms)
long	seed			# Random number seed
real	subsample		# Subsampling (nxsub param)
real	nsigma			# Dynamic range of gaussian (dynrange param)

int	ncnew, nlnew, nonew
bool	new, flag[2]
int	i, j, k, k1, k2, m, m1, m2, dc
long	seed1
real	mwc, mw1, mw2, dmw, x, x1, x2, dx, w, p, s, xc1, dx1
real	p1, p2, pcen, fwhm, flux, flux1
real	a[2], b[2], c[2], tb[2], cb[2], tt[2], ctb[2], t2tb[2], xmin[2], xmax[2]
real	aplow[2], aphigh[2]
double	w1, dw
pointer	sp, image, fname, apnum, comment
pointer	im, mw, waves, peaks, sigmas, buf, spec, bf1, bf2, asi, data

long	clgetl(), clktime()
int	clgeti(), clgwrd(), imtopenp(), imtgetim()
int	nowhite(), access(), open(), fscan(), nscan()
real	clgetr(), urand(), asigrl()
real	ecx2w(), ecxdx(), ecw2x(), ecw2xr, ecdelta()
pointer	immap(), mw_open(), impl2r(), imgl2r()
bool	clgetb()
errchk	open(), ecgrating()

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (apnum, SZ_FNAME, TY_CHAR)
	call salloc (comment, LEN_COMMENT, TY_CHAR)

	# Get parameters.
	if (clgetb ("make"))
	    images = imtopenp ("images")
	ncnew = clgeti ("ncols")
	nlnew = clgeti ("nlines")
	nonew = clgeti ("norders")
	profile = clgwrd ("profile", Memc[comment], LEN_COMMENT, PTYPES)
	width = clgetr ("width")
	scattered = clgetr ("scattered")
	xc = clgetr ("xc")
	yc = clgetr ("yc")
	pixsize = clgetr ("pixsize")

	f[1] = clgetr ("f")
	mc[1] = clgeti ("order")
	gmm[1] = clgetr ("gmm")
	blaze[1] = clgetr ("blaze")
	t[1] = clgetr ("theta")
	wc[1] = clgetr ("wavelength")
	disp[1] = clgetr ("dispersion")

	f[2] = clgetr ("cf")
	mc[2] = clgeti ("corder")
	gmm[2] = clgetr ("cgmm")
	blaze[2] = clgetr ("cblaze")
	t[2] = clgetr ("ctheta")
	wc[2] = clgetr ("cwavelength")
	disp[2] = clgetr ("cdispersion")

	z = clgetr ("rv")
	if (clgetb ("z"))
	    z = 1 + z
	else {
	    z = z / 299792.5
	    z = sqrt ((1 + z) / (1 - z)) 
	}
	cont = clgetr ("continuum")
	temp = clgetr ("temperature")
	lines = imtopenp ("lines")
	peak = clgetr ("peak")
	sigma = clgetr ("sigma")
	seed = clgetl ("seed")
	if (IS_INDEFL(seed))
	    seed1 = seed1 + clktime (long(0))
	else
	    seed1 = seed
	subsample = 1. / clgeti ("nxsub")
	nsigma = sqrt (2. * log (clgetr ("dynrange")))

	# Substitute defaults for INDEF center parameters
	if (IS_INDEF(xc))
	    xc = (ncnew - 1) / 2.
	if (IS_INDEF(yc))
	    yc = (nlnew - 1) / 2.

	# Derive and check grating parameters.
	do i = 1, 2 {
	    if (mc[i] == 0) {
		if (IS_INDEF(wc[i]) || IS_INDEF(disp[i]))
		    call error (1, "Prism wavelength parameters missing")
		next
	    }
	    if (!IS_INDEF(pixsize)) {
		if (!IS_INDEF(f[i]))
		    f[i] = f[i] / pixsize
		if (!IS_INDEF(disp[i]))
		    disp[i] = disp[i] * pixsize
	    }
	    if (i == 1)
	       flag[i] = true
	    else
	       flag[i] = false
	    iferr (call ecgrating (flag[i], f[i], gmm[i], blaze[i], t[i], mc[i],
		wc[i], disp[i])) {
		if (i == 1)
		    call eprintf ("Echelle grating: ")
		else
		    call eprintf ("Crossdisperser grating: ")
		if (!IS_INDEF(mc[i])&&!IS_INDEF(wc[i])&&!IS_INDEF(disp[i])) {
		    call eprintf ("Using linear dispersion\n")
		    call erract (EA_WARN)
		    flag[i] = true
		} else {
		    call eprintf ("\n")
		    call erract (EA_ERROR)
		}
	    } else
		flag[i] = false
	}

	# List grating parameters if desired.
	if (clgetb ("list"))
	    call eclist (pixsize, f, gmm, blaze, t, mc, wc, disp)
	
	# If not making an image return.
	if (!clgetb ("make")) {
	    call sfree (sp)
	    return
	}

	# Loop through images.  Line list files may be missing.
	Memc[fname] = EOS
	while (imtgetim (images, Memc[image], SZ_FNAME) != EOF) {
	    i = imtgetim (lines, Memc[fname], SZ_FNAME)

	    # Map image and check for existing images.
	    ifnoerr (im = immap (Memc[image], READ_WRITE, LEN_UA)) {
		call eprintf ("%s: ")
		    call pargstr (Memc[image])
		call flush (STDERR)
		if (!clgetb ("clobber")) {
		    call eprintf ("Warning: Image already exists (%s)\n")
			call pargstr (Memc[image])
		    call imunmap (im)
		    next
		}
		new = false

		if (profile == EXTRACTED) {
		    nl = IM_LEN(im,1)
		    norders = IM_LEN(im,2)
		} else {
		    nc = IM_LEN(im,1)
		    nl = IM_LEN(im,2)
		}
	    } else {
		iferr (im = immap (Memc[image], NEW_IMAGE, LEN_UA)) {
		    call erract (EA_WARN)
		    next
		}
		new = true

		nc = ncnew
		nl = nlnew
		norders = nonew

		IM_PIXTYPE(im) = TY_REAL
		IM_NDIM(im) = 2
		if (profile == EXTRACTED) {
		    IM_LEN(im,1) = nl
		    IM_LEN(im,2) = norders
		} else {
		    IM_LEN(im,1) = nc
		    IM_LEN(im,2) = nl
		}
	    }

	    # Set frequently used constants.
	    do i = 1, 2 {
		if (flag[i]) {
		    f[i] = INDEF
		    a[i] = mc[i] * wc[i]
		    b[i] = mc[i] * disp[i]
		    c[i] = PI * mc[i] * disp[i]
		} else {
		    b[i] = 1e7 / gmm[i]
		    a[i] = b[i] * sin (DEGTORAD(t[i]))
		    c[i] = b[i] * PI * cos (DEGTORAD(t[i]))
		    ctb[i] = cos (DEGTORAD(blaze[i]))
		    t2tb[i] = tan (DEGTORAD(2 * blaze[i]))
		    tt[i] = tan (DEGTORAD(t[i] - blaze[i]))
		    tb[i] = tan (DEGTORAD(2 * blaze[i] - t[i]))
		    cb[i] = cos (DEGTORAD(2 * blaze[i] - t[i]))
		    xmin[i] = -1. / max (tt[i], f[i] / yc)
		    xmax[i] = -1. / min (tt[i], -f[i] / yc)
		}
	    }

	    # Set orders.
	    m1 = max (1, mc[1] - (norders-1) / 2)
	    m2 = m1 + norders - 1
	    mwc = mc[1] * wc[1]
	    mw1 = ecx2w (-yc, 1, a, b, f, cb, tb, t2tb, xmin, xmax)
	    mw2 = ecx2w (yc, 1, a, b, f, cb, tb, t2tb, xmin, xmax)
	    dmw = mw2 - mw1
	    if (mc[2] == 0) {
		disp[2] = disp[2] / wc[2]
		dx1 = 1.
	    } else {
		xc1 = xc - ecw2x (mc[2]*wc[1], 2, a, b, f, tb, ctb, t2tb)
		dx1 = ecxdx (xc - xc1, 2, f, tb)
	    }

	    # For 2D images adjust orders to exclude those outside image.
	    if (profile != EXTRACTED) {
		for (; m1<mc[1]; m1=m1+1) {
		    w = mw1 / m1
		    if (mc[2] == 0)
			x = (w - wc[1]) / (w * disp[2]) + xc
		    else
			x = ecw2x (mc[2]*w, 2, a, b, f, tb, ctb, t2tb) + xc1
		    if (x < nc)
			break
		}
		for (; m2>mc[1]; m2=m2-1) {
		    w = mw2 / m2
		    if (mc[2] == 0)
			x = (w - wc[1]) / (w * disp[2]) + xc
		    else
			x = ecw2x (mc[2]*w, 2, a, b, f, tb, ctb, t2tb) + xc1
		    if (x > 0)
			break
		}
		norders = m2 - m1 + 1
	    }

	    # Setup header parameters for new images.
	    if (new) {
		call clgstr ("header", Memc[comment], LEN_COMMENT)
		iferr (call mkh_header (im, Memc[comment], true, false))
		    call erract (EA_WARN)

		call clgstr ("title", IM_TITLE(im), SZ_IMTITLE)
		if (profile == EXTRACTED) {
		    mw = mw_open (NULL, 2)
		    call mw_newsystem (mw, "multispec", 2)
		    call mw_swtype (mw, 1, 1, "multispec", "")
		    call mw_swtype (mw, 2, 1, "multispec", "")
		    if (IS_INDEF(f[1])) {
			call mw_swattrs (mw, 1, "label", "Wavelength")
			call mw_swattrs (mw, 1, "units", "Angstroms")
		    }
		    call smw_open(mw, NULL, im)

		    do m = m1, m2 {
			i = m - m1 + 1
			if (IS_INDEF(f[1])) {
			    w1 = mw1 / m
			    dw = b[1] / m
			    dc = 0
			} else {
			    w1 = 1.
			    dw = 1.
			    dc = -1
			}
			w = mwc / m
			if (mc[2] == 0)
			    x = (w - wc[1]) / (w * disp[2]) + xc
			else
			    x = ecw2x (mc[2]*w, 2, a, b, f, tb, ctb, t2tb) + xc1
			aplow[1] = 1 + x - width
			aphigh[1] = 1 + x + width
			aplow[2] = INDEFR
			aphigh[2] = INDEFR
			call smw_swattrs (mw, i, 1, i, m, dc, w1, dw, nl,
			    0D0, aplow, aphigh, "")
		    }
		    call smw_saveim (mw, im)
		    call smw_close (mw)
		} else
		    call imaddi (im, "DISPAXIS", 2)
	    }

	    # Get the line list if given or create random lines.
	    i = nowhite (Memc[fname], Memc[fname], SZ_FNAME)
	    if (access (Memc[fname], 0, 0) == YES) {
		i = open (Memc[fname], READ_ONLY, TEXT_FILE)
		nrandom = 0
		while (fscan (i) != EOF) {
		    call gargr (w)
		    call gargr (p)
		    call gargr (s)
		    if (nscan() < 1)
			next
		    if (nscan() < 3)
			s = sigma
		    if (nscan() < 2)
			p = peak * urand (seed1)
		    if (nrandom == 0) {
			j = 50
			call malloc (waves, j, TY_REAL)
			call malloc (peaks, j, TY_REAL)
			call malloc (sigmas, j, TY_REAL)
		    } else if (nrandom == j) {
			j = j + 10
			call realloc (waves, j, TY_REAL)
			call realloc (peaks, j, TY_REAL)
			call realloc (sigmas, j, TY_REAL)
		    }
		    Memr[waves+nrandom] = w * z
		    Memr[peaks+nrandom] = p / z
		    Memr[sigmas+nrandom] = s * z
		    nrandom = nrandom + 1
		}
		call close (i)
	    } else {
		nrandom = clgeti ("nrandom")
		call malloc (waves, nrandom, TY_REAL)
		call malloc (peaks, nrandom, TY_REAL)
		call malloc (sigmas, nrandom, TY_REAL)
		j = max (1, mc[1] - (norders-1) / 2)
		do i = 0, nrandom-1 {
		    w = z * (mw1 + dmw * urand (seed1))
		    w = w - dmw * nint ((w - mwc) / dmw)
		    m = j + norders * urand (seed1)
		    Memr[waves+i] = w / m
		    Memr[peaks+i] = peak * urand (seed1) / z
		    Memr[sigmas+i] = sigma * z
		}
		if (nrandom > 0 && Memc[fname] != EOS) {
		    i = open (Memc[fname], NEW_FILE, TEXT_FILE)
		    do j = 0, nrandom-1 {
			call fprintf (i, "%g %g %g\n")
			    call pargr (Memr[waves+j] / z)
			    call pargr (Memr[peaks+j] * z)
			    call pargr (Memr[sigmas+j] / z)
		    }
		    call close (i)
		}
	    }

	    # Find the absolute response of the gratings at the reference
	    # blaze peak.

	    flux = 1.
	    w = wc[1]
	    m = mc[1]
	    do i = m - 1, 1, -1 {
		x = ecw2x (i*w, 1, a, b, f, tb, ctb, t2tb)
		if (IS_INDEF(x))
		    break
		p = ecdelta (x, w, 1, f, c, tt)
		flux = flux + (sin (p) / p) ** 2
		if (abs (p) > 100.)
		    break
	    }
	    do i = m + 1, ARB {
		x = ecw2x (i*w, 1, a, b, f, tb, ctb, t2tb)
		if (IS_INDEF(x))
		    break
		p = ecdelta (x, w, 1, f, c, tt)
		flux = flux + (sin (p) / p) ** 2
		if (abs (p) > 100.)
		    break
	    }
	    j = (a[1] + b[1]) / w
	    do i = j, 1, -1 {
		x = ecw2xr (i*w, 1, a, b, f, tb, ctb, t2tb)
		if (IS_INDEF(x))
		    break
		p = ecdelta (x, w, 1, f, c, tt)
		flux = flux + (sin (p) / p) ** 2
		if (abs (p) > 100.)
		    break
	    }
	    if (mc[2] != 0) {
		x = ecw2x (mc[2]*w, 2, a, b, f, tb, ctb, t2tb)
		p = ecdelta (x, w, 2, f, c, tt)
		if (p != 0.) {
		    p = (sin (p) / p) ** 2
		    if (p != 0.)
		       flux = flux / p
		}
	    }
	    flux1 = flux

	    # Make the 1D spectrum.
	    call malloc (buf, norders*nl, TY_REAL)
	    do m = m1, m2 {
		spec = buf + (m - m1) * nl
		call aclrr (Memr[spec], nl)

		# Make the lines.
		do i = 0, nrandom-1 {
		    w = m * Memr[waves+i]
		    p = Memr[peaks+i] * subsample
		    dx = m * Memr[sigmas+i]
		    x1 = max (-0.499,
			ecw2x (w-nsigma*dx, 1, a, b, f, tb, ctb, t2tb)+yc)
		    x2 = min (nl-0.501,
			ecw2x (w+nsigma*dx, 1, a, b, f, tb, ctb, t2tb)+yc)
		    dx = -0.5 / dx ** 2
		    for (x = x1; x <= x2; x = x + subsample) {
			s = ecx2w (x-yc, 1, a, b, f, cb, tb, t2tb, xmin, xmax)
			j = nint (x)
			Memr[spec+j] = Memr[spec+j] + p * exp (dx*(s-w)**2)
		    }
		}

		# Initialize blackbody function.
		if (temp > 0.) {
		    w  = wc[1] * 1.0e-8
		    x = exp (1.4388 / (w * temp))
		    p1 = w**5 * (x-1.0)
		}

		# Compute blaze peak correction
		flux = 1.
		w = mc[1] * wc[1] / m

		do i = m - 1, 1, -1 {
		    x = ecw2x (i*w, 1, a, b, f, tb, ctb, t2tb)
		    if (IS_INDEF(x))
			break
		    p = ecdelta (x, w, 1, f, c, tt)
		    flux = flux + (sin (p) / p) ** 2
		    if (abs (p) > 100.)
			break
		}
		do i = m + 1, ARB {
		    x = ecw2x (i*w, 1, a, b, f, tb, ctb, t2tb)
		    if (IS_INDEF(x))
			break
		    p = ecdelta (x, w, 1, f, c, tt)
		    flux = flux + (sin (p) / p) ** 2
		    if (abs (p) > 100.)
			break
		}
		j = (a[1] + b[1]) / w
		do i = j, 1, -1 {
		    x = ecw2xr (i*w, 1, a, b, f, tb, ctb, t2tb)
		    if (IS_INDEF(x))
			break
		    p = ecdelta (x, w, 1, f, c, tt)
		    flux = flux + (sin (p) / p) ** 2
		    if (abs (p) > 100.)
			break
		}

		do i = 0, nl-1 {
		    w = ecx2w (i-yc, 1, a, b, f, cb, tb, t2tb, xmin, xmax) / m

		    # Scale by continuum.
		    p = cont
		    if (temp > 0.) {
			x2 = w * 1e-8
			x1 = exp (1.4388 / (x2 * temp))
			p = p * (p1 / x2**5 / (x1-1.0))
		    }
		    if (p > 0.)
			Memr[spec+i] = max (0., p * (1. + Memr[spec+i]))

		    # Apply blaze functions and pixel size corrections.
		    x = 1 / flux
		    p = ecdelta (i-yc, w, 1, f, c, tt)
		    if (p != 0.)
			x = x * (sin (p) / p) ** 2

		    if (mc[2] != 0) {
			s = ecw2x (mc[2]*w, 2, a, b, f, tb, ctb, t2tb)
			p = ecdelta (s, w, 2, f, c, tt)
			if (p != 0.)
			   x = x * (sin (p) / p) ** 2
		    }

		    dx = ecxdx (i-yc, 1, f, tb) * mc[1] / m
		    if (mc[2] != 0)
			dx = dx * ecxdx (s, 2, f, tb)

		    Memr[spec+i] = Memr[spec+i] * flux1 * x * dx / dx1
		}
	    }

	    # Write 1D spectrum or create 2D spectrum.
	    if (profile == EXTRACTED) {
		do i = 1, norders {
		    spec = buf + (i - 1) * nl
		    if (new)
			call amovr (Memr[spec], Memr[impl2r(im,i)], nl)
		    else
			call aaddr (Memr[spec], Memr[imgl2r(im,i)],
			    Memr[impl2r(im,i)], nl)
		}
	    } else {
		# Make scattered light response.
		if (scattered > 0.) {
		    call malloc (bf1, nc, TY_REAL)
		    call malloc (bf2, nl, TY_REAL)
		    if (mc[2] != 0) {
			do i = 0, nc-1 {
			    s = i - xc1
			    w = ecx2w (s, 2, a, b, f, cb, tb, t2tb,
				xmin, xmax) / mc[2]
			    p = ecdelta (s, w, 2, f, c, tt)
			    if (p == 0.)
				Memr[bf1+i] = scattered
			    else
				Memr[bf1+i] = scattered * (sin (p) / p) ** 2
			}
		    } else
			call amovkr (scattered, Memr[bf1], nc)

		    s = wc[1] - disp[1] * yc
		    do i = 0, nl - 1 {
			w = ecx2w (i-yc, 1, a, b, f, cb, tb, t2tb, xmin, xmax) /
			    mc[1]
			p = ecdelta (i-yc, w, 1, f, c, tt)
			if (p == 0.)
			    Memr[bf2+i] = 1.
			else
			    Memr[bf2+i] = (sin (p) / p) ** 2
		    }
		} else
		    bf1 = NULL

		# Make the profile templates stored as an interpolation function
		# with the returned center, fwhm, and flux.

		switch (profile) {
		case GAUSS:
		    call mkprof (2., asi, pcen, fwhm, flux)
		case SLIT:
		    call mkprof (10., asi, pcen, fwhm, flux)
		}

		# Now expand the 1D spectra into 2D profiles.
		dx = fwhm / width
		do i = 0, nl-1 {
		    data = impl2r (im, i+1)
		    if (new)
			call aclrr (Memr[data], nc)
		    else
			call amovr (Memr[imgl2r(im,i+1)], Memr[data], nc)

		    if (bf1 != NULL)
			do j = 0, nc-1
			    Memr[data+j] = Memr[data+j] +
				Memr[bf1+j] * Memr[bf2+i]
			
		    w = ecx2w (i-yc, 1, a, b, f, cb, tb, t2tb, xmin, xmax)
		    do j = 0, norders-1 {
			x = w / (m1 + j)
			p = Memr[buf+j*nl+i] / flux
			if (mc[2] == 0)
			    x = (x - wc[1]) / (x * disp[2]) + xc
			else
			    x = ecw2x (mc[2]*x, 2, a, b, f, tb, ctb, t2tb) +
				xc1
			p1 = max (-0.49, x - pcen / dx)
			p2 = min (nc - 0.51, x + pcen / dx)
			if (p1 >= p2)
			    next

			k1 = p1 + 0.5
			k2 = p2 + 0.5
			x1 = (p1 - x) * dx + pcen + 1
			x2 = (min (p2, k1 + 0.5) - x) * dx + pcen + 1
			x1 = max (1., x1)
			x2 = max (1., x2)

			m = data + k1
			Memr[m] = Memr[m] + p * asigrl (asi, x1, x2)
			do k = k1+1, k2-1 {
			   x1 = x2
			   x2 = x1 + dx
			   m = m + 1
			   Memr[m] = Memr[m] + p * asigrl (asi, x1, x2)
			}
			x1 = x2
			x2 = (p2 - x) * dx + pcen + 1
			m = m + 1
			Memr[m] = Memr[m] + p * asigrl (asi, x1, x2)
		    }
		}

		call asifree (asi)
	    }

	    call mfree (buf, TY_REAL)
	    if (bf1 != NULL) {
		call mfree (bf1, TY_REAL)
		call mfree (bf2, TY_REAL)
	    }
	    call mfree (waves, TY_REAL)
	    call mfree (peaks, TY_REAL)
	    call mfree (sigmas, TY_REAL)

	    # Add comment history of task parameters.
	    if (clgetb ("comments")) {
		call strcpy ("# ", Memc[comment], LEN_COMMENT)
		call cnvtime (clktime (0), Memc[comment+2], LEN_COMMENT-2)
		call mkh_comment (im, Memc[comment])
		call mkh_comment (im, "begin        mkechelle")
		call mkh_comment1 (im, "profile", 's')
		if (profile != EXTRACTED) {
		    call mkh_comment1 (im, "width", 'r')
		    call mkh_comment1 (im, "scattered", 'r')
		}
		call mkh_comment1 (im, "norders", 'i')
		call sprintf (Memc[comment], LEN_COMMENT, "%9txc%24t%g")
		    call pargr (1+xc)
		call mkh_comment (im, Memc[comment])
		call sprintf (Memc[comment], LEN_COMMENT, "%9tyc%24t%g")
		    call pargr (1+yc)
		call mkh_comment (im, Memc[comment])
		call mkh_comment1 (im, "pixsize", 'r')

		call sprintf (Memc[comment], LEN_COMMENT, "%9tf%24t%g")
		    if (IS_INDEF(pixsize) || IS_INDEF(f[1]))
			call pargr (f[1])
		    else
			call pargr (f[1] * pixsize)
		call mkh_comment (im, Memc[comment])
		call sprintf (Memc[comment], LEN_COMMENT, "%9tgmm%24t%g")
		    call pargr (gmm[1])
		call mkh_comment (im, Memc[comment])
		call sprintf (Memc[comment], LEN_COMMENT, "%9tblaze%24t%g")
		    call pargr (blaze[1])
		call mkh_comment (im, Memc[comment])
		call sprintf (Memc[comment], LEN_COMMENT, "%9ttheta%24t%g")
		    call pargr (t[1])
		call mkh_comment (im, Memc[comment])
		call sprintf (Memc[comment], LEN_COMMENT, "%9torder%24t%d")
		    call pargi (mc[1])
		call mkh_comment (im, Memc[comment])
		call sprintf (Memc[comment], LEN_COMMENT, "%9twavelength%24t%g")
		    call pargr (wc[1])
		call mkh_comment (im, Memc[comment])
		call sprintf (Memc[comment], LEN_COMMENT, "%9tdispersion%24t%g")
		    if (IS_INDEF(pixsize) || IS_INDEF(disp[1]))
			call pargr (disp[1])
		    else
			call pargr (disp[1] / pixsize)
		call mkh_comment (im, Memc[comment])

		call sprintf (Memc[comment], LEN_COMMENT, "%9tcf%24t%g")
		    if (IS_INDEF(pixsize) || IS_INDEF(f[2]))
			call pargr (f[2])
		    else
			call pargr (f[2] * pixsize)
		call mkh_comment (im, Memc[comment])
		call sprintf (Memc[comment], LEN_COMMENT, "%9tcgmm%24t%g")
		    call pargr (gmm[2])
		call mkh_comment (im, Memc[comment])
		call sprintf (Memc[comment], LEN_COMMENT, "%9tcblaze%24t%g")
		    call pargr (blaze[2])
		call mkh_comment (im, Memc[comment])
		call sprintf (Memc[comment], LEN_COMMENT, "%9tctheta%24t%g")
		    call pargr (t[2])
		call mkh_comment (im, Memc[comment])
		call sprintf (Memc[comment], LEN_COMMENT, "%9tcorder%24t%d")
		    call pargi (mc[2])
		call mkh_comment (im, Memc[comment])
		call sprintf (Memc[comment], LEN_COMMENT, "%9tcwavelength%24t%g")
		    call pargr (wc[2])
		call mkh_comment (im, Memc[comment])
		call sprintf (Memc[comment], LEN_COMMENT, "%9tcdispersion%24t%g")
		    if (IS_INDEF(pixsize) || IS_INDEF(disp[2]))
			call pargr (disp[2])
		    else
			call pargr (disp[2] / pixsize)
		call mkh_comment (im, Memc[comment])

		call mkh_comment1 (im, "rv", 'r')
		call mkh_comment1 (im, "z", 'b')
		call mkh_comment1 (im, "continuum", 'r')
		call mkh_comment1 (im, "temperature", 'r')
		if (nrandom > 0) {
		    if (Memc[fname] != EOS)
			call mkh_comment1 (im, "lines", 's')
		    call sprintf (Memc[comment], LEN_COMMENT, "%9tnlines%24t%d")
			call pargi (nrandom)
		    call mkh_comment (im, Memc[comment])
		    call mkh_comment1 (im, "peak", 'r')
		    call mkh_comment1 (im, "sigma", 'r')
		    call mkh_comment1 (im, "seed", 'i')
		}
	    }

	    call imunmap (im)
	}

	call imtclose (images)
	call imtclose (lines)
	call sfree (sp)
end


# Definitions of INDEF parameter flags.
define	F	1B
define	G	2B
define	B	4B
define	T	10B
define	M	20B
define	W	40B
define	D	100B

# Combinations
define	FG	3B
define	FB	5B
define	FT	11B
define	FM	21B
define	FW	41B
define	GB	6B
define	GT	12B
define	GW	42B
define	GD	102B
define	BT	14B
define	BM	24B
define	BW	44B
define	BD	104B
define	TM	30B
define	TW	50B
define	TD	110B
define	MW	60B
define	MD	120B
define	WD	140B


# ECGRATING -- Derive and check grating parameters.

procedure ecgrating (e, f, g, b, t, m, w, d)

bool	e
real	f,  g, b, t, w, d, x
int	m

int	i, flags
define	err_	10

begin
	if (!IS_INDEF(f)) {
	    if (f <= 0.)
		f = INDEF
	}
	if (!IS_INDEF(g)) {
	    if (g <= 0.)
		g = INDEF
	    else
		g = g / 1e7
	}
	if (!IS_INDEF(b)) {
	    b = DEGTORAD (b)
	    if (b == 0. && t == 0.)
		t = INDEF
	}
	if (!IS_INDEF(t)) {
	    t = DEGTORAD (t)
	    if (t > PI && !IS_INDEF(b))
		t = t - TWOPI + b
	}
	if (!IS_INDEFI(m) && m <= 0)
	    m = INDEFI
	if (!IS_INDEF(w) && w <= 0.)
	    w = INDEF
	if (!IS_INDEF(d) && d <= 0.)
	    d = INDEF

	flags = 0
	if (IS_INDEF(f))
	    flags = flags + F
	if (IS_INDEF(g))
	    flags = flags + G
	if (IS_INDEF(b))
	    flags = flags + B
	if (IS_INDEF(t))
	    flags = flags + T
	if (IS_INDEFI(m))
	    flags = flags + M
	if (IS_INDEF(w))
	    flags = flags + W
	if (IS_INDEF(d))
	    flags = flags + D

	switch (flags) {
	case 0, F, G, B, T, M, W, D:
	    switch (flags) {
	    case F:
		f = cos (2 * b - t) / (g * m * d)
	    case G: 
		g = (sin (t) + sin (2 * b - t)) / (m * w)
		if (g == 0.)
		    g = INDEF
	    case B:
		if (t > PI) {
		    x = g * m * w / (2 * cos (t))
		    if (abs (x) > 1.)
			goto err_
		    b = asin (x)
		    t = t - TWOPI + b
		} else {
		    x = g * m * w - sin (t)
		    if (abs (x) > 1.)
			goto err_
		    b = (t + asin (x)) / 2
		}
	    case T:
		x = g * m * w / (2 * sin(b))
		if (abs (x) > 1.)
		    goto err_
		if (e)
		    t = b + acos (x)
		else
		    t = b - acos (x)
	    case M:
		m = max (1, nint ((sin(t) + sin(2*b-t)) / (g * w)))
	    }
	    if (!IS_INDEF(g)) {
		w = (sin (t) + sin (2 * b - t)) / (g * m)
		d = cos (2 * b - t) / (f * g * m)
	    }
	case FG:
	    x = (sin (t) + sin (2 * b - t)) / (m * w)
	    if (x == 0.)
		goto err_
	    g = x
	    f = cos (2 * b - t) / (g * m * d)
	case FB:
	    if (t > PI) {
		x = g * m * w / (2 * cos (t))
		if (abs (x) > 1.)
		    goto err_
		b = asin (x)
		t = t - TWOPI + b
	    } else {
		x = g * m * w - sin (t)
		if (abs (x) > 1.)
		    goto err_
		b = (t + asin (x)) / 2
	    }
	    f = cos (2 * b - t) / (g * m * d)
	case FT:
	    x = g * m * w / (2 * sin (b))
	    if (abs (x) > 1.)
		goto err_
	    if (e)
		t = b + acos (x)
	    else
		t = b - acos (x)
	    f = cos (2 * b - t) / (g * m * d)
	case FM:
	    m = nint ((sin (t) + sin (2 * b - t)) / (g * w))
	    f = cos (2 * b - t) / (g * m * d)
	    w = (sin (t) + sin (2 * b - t)) / (g * m)
	    d = cos (2 * b - t) / (f * g * m)
	case FW:
	    w = (sin (t) + sin (2 * b - t)) / (g * m)
	    f = cos (2 * b - t) / (g * m * d)
	case GB:
	    x = f * d / w
	    if (t > PI) {
		b = atan (1 / (2 * x - tan (t)))
		t = t - TWOPI + b
	    } else {
		x = (tan (t) - x) / (1 + 2 * x * tan (t))
		b = atan (x + sqrt (1 + x * x))
	    }
	    g = (sin (t) + sin (2 * b - t)) / (m * w)
	case GT:
	    t = b + atan (2 * f * d / w - 1 / tan (b))
	    g = (sin (t) + sin (2 * b - t)) / (m * w)
	case GW:
	    g = cos (2 * b - t) / (f * m * d)
	    if (g == 0.)
		g = INDEF
	    else
		w = (sin (t) + sin (2 * b - t)) / (g * m)
	case GD:
	    x = (sin (t) + sin (2 * b - t)) / (m * w)
	    if (x == 0.)
		goto err_
	    g = x
	    d = cos (2 * b - t) / (f * g * m)
	case BT:
	    x = f * g * m * d
	    if (abs (x) > 1.)
		goto err_
	    x = acos (x)
	    x = g * m * w - sin (x)
	    if (abs (x) > 1.)
		goto err_
	    t = asin (x)
	    b = (acos (f * g * m * d) + t) / 2
	case BM:
	    x = f * d / w
	    if (t > PI) {
		b = atan (1 / (2 * x - tan (t)))
		t = t - TWOPI + b
	    } else {
		x = (tan (t) - x) / (1 + 2 * x * tan (t))
		b = atan (x + sqrt (1 + x * x))
	    }
	    m = max (1, nint ((sin(t) + sin(2*b-t)) / (g * w)))
	    b = (t + asin (g * m * w - sin (t))) / 2
	    w = (sin (t) + sin (2 * b - t)) / (g * m)
	    d = cos (2 * b - t) / (f * g * m)
	case BW:
	    b = (t + acos (f * g * m * d)) / 2
	    w = (sin (t) + sin (2 * b - t)) / (g * m)
	case BD:
	    if (t > PI) {
		x = g * m * w / (2 * cos (t))
		if (abs (x) > 1.)
		    goto err_
		b = asin (x)
		t = t - TWOPI + b
	    } else {
		x = g * m * w - sin (t)
		if (abs (x) > 1.)
		    goto err_
		b = (t + asin (x)) / 2
	    }
	    d = cos (2 * b - t) / (f * g * m)
	case TM:
	    x = f * d / w
	    x = b + 2 * atan (x - 1 / (2 * tan (b)))
	    i = max (1, nint ((sin(x) + sin(2*b-x)) / (g * w)))
	    x = g * i * w / (2 * sin (b))
	    if (abs (x) > 1.)
		goto err_
	    if (e)
		t = b + acos (x)
	    else
		t = b - acos (x)
	    m = i
	    w = (sin (t) + sin (2 * b - t)) / (g * m)
	    d = cos (2 * b - t) / (f * g * m)
	case TW:
	    x = f * g * m * d
	    if (abs (x) > 1.)
		goto err_
	    t = 2 * b - acos (x)
	    w = (sin (t) + sin (2 * b - t)) / (g * m)
	case TD:
	    x = g * m * w / (2 * sin (b))
	    if (abs (x) > 1.)
		goto err_
	    if (e)
		t = b + acos (x)
	    else
		t = b - acos (x)
	    d = cos (2 * b - t) / (f * g * m)
	case MW:
	    m = max (1, nint (cos (2 * b - t) / (f * g * d)))
	    w = (sin (t) + sin (2 * b - t)) / (g * m)
	    d = cos (2 * b - t) / (f * g * m)
	case MD:
	    m = max (1, nint ((sin(t) + sin(2*b-t)) / (g * w)))
	    w = (sin (t) + sin (2 * b - t)) / (g * m)
	    d = cos (2 * b - t) / (f * g * m)
	case WD:
	    w = (sin (t) + sin (2 * b - t)) / (g * m)
	    d = cos (2 * b - t) / (f * g * m)
	}

	if (!IS_INDEF(g))
	    g = g * 1e7
	if (!IS_INDEF(b))
	    b = RADTODEG (b)
	if (!IS_INDEF(t))
	    t = RADTODEG (t)
			   
	if (IS_INDEF(f) || IS_INDEF(g) || IS_INDEF(b) || IS_INDEF(t) ||
	    IS_INDEF(m) || IS_INDEF(w) || IS_INDEF(d))
	    call error (1,
		"Insufficient information to to resolve grating parameters")

	return

err_	if (!IS_INDEF(g))
	    g = g * 1e7
	if (!IS_INDEF(b))
	    b = RADTODEG (b)
	if (!IS_INDEF(t))
	    t = RADTODEG (t)
	call error (2, "Impossible combination of grating parameters")
end


# ECLIST -- List grating parameters.

procedure eclist (p, f, g, b, t, m, w, d)

real	p, f[2], g[2], b[2], t[2], w[2], d[2]
int	m[2]

begin
	call printf ("Echelle grating parameters:\n")
	    call printf ("  Focal length = %g %s\n")
		if (IS_INDEF(p) || IS_INDEF(f[1]))
		    call pargr (f[1])
		else
		    call pargr (f[1] * p)
		if (IS_INDEF(p))
		    call pargstr ("pixels")
		else
		    call pargstr ("mm")
	    call printf ("  Grating = %g grooves/mm\n")
		call pargr (g[1])
	    call printf ("  Blaze angle = %g degrees\n")
		call pargr (b[1])
	    call printf ("  Incidence angle = %g degrees\n")
		call pargr (t[1])
	    call printf ("  Reference order = %d\n")
		call pargi (m[1])
	    call printf (
		"  Blaze wavelength of reference order = %g Angstroms\n")
		call pargr (w[1])
	    call printf (
		"  Blaze dispersion of reference order = %g Angstroms/%s\n")
		if (IS_INDEF(p) || IS_INDEF(d[1]))
		    call pargr (d[1])
		else
		    call pargr (d[1] / p)
		if (IS_INDEF(p))
		    call pargstr ("pixels")
		else
		    call pargstr ("mm")

	if (m[2] == 0.) {
	    call printf ("Crossdisperser prism parameters:\n")
		call printf ("  Reference wavelength = %g Angstroms/pixel\n")
		    call pargr (w[2])
		call printf (
		"  Dispersion at reference wavelength = %g Angstroms/%s\n")
		    if (IS_INDEF(p) || IS_INDEF(d[2]))
			call pargr (d[2])
		    else
			call pargr (d[2] / p)
		    if (IS_INDEF(p))
			call pargstr ("pixels")
		    else
			call pargstr ("mm")
	} else {
	    call printf ("Crossdisperser grating parameters:\n")
		call printf ("  Focal length = %g %s\n")
		    if (IS_INDEF(p) || IS_INDEF(f[2]))
			call pargr (f[2])
		    else
			call pargr (f[2] * p)
		    if (IS_INDEF(p))
			call pargstr ("pixels")
		    else
			call pargstr ("mm")
		call printf ("  Grating = %g grooves/mm\n")
		    call pargr (g[2])
		call printf ("  Blaze angle = %g degrees\n")
		    call pargr (b[2])
		call printf ("  Incidence angle = %g degrees\n")
		    call pargr (t[2])
		call printf ("  Order = %d\n")
		    call pargi (m[2])
		call printf (
		    "  Blaze wavelength = %g Angstroms\n")
		    call pargr (w[2])
		call printf (
		    "  Blaze dispersion = %g Angstroms/%s\n")
		    if (IS_INDEF(p) || IS_INDEF(d[2]))
			call pargr (d[2])
		    else
			call pargr (d[2] / p)
		    if (IS_INDEF(p))
			call pargstr ("pixels")
		    else
			call pargstr ("mm")
	}
	call flush (STDOUT)
end


# ECX2W -- Given pixel position return wavelength.

real procedure ecx2w (x, i, a, b, f, cb, tb, t2tb, xmin, xmax)

real	x, a[2], b[2], f[2], cb[2], tb[2], t2tb[2], xmin[2], xmax[2], w
int	i

begin
    if (IS_INDEF(f[i]))
	return (a[i] + b[i] * x)
    w = x / f[1]
    w = a[i] + b[i] * cb[i] / sqrt (1 + w * w) * (w + tb[i])
    return (w)
end


# ECX2WR -- Given pixel position return wavelength of reflected component.

real procedure ecx2wr (x, i, a, b, f, cb, tb, t2tb, xmin, xmax)

real	x, a[2], b[2], f[2], cb[2], tb[2], t2tb[2], xmin[2], xmax[2], w
int	i

begin
    if (IS_INDEF(f[i]))
	return (INDEF)

    w = x / f[i]
    if (x <= xmin[i] || x >= xmax[i])
	return (INDEF)

    w = (w - t2tb[i]) / (1 + w * t2tb[i])
    w = a[i] + b[i] * cb[i] /  sqrt (1 + w * w) * (w + tb[i])
    return (w)
end


# ECXDX -- Given pixel position return pixel size per unit wavelength
# normalized to the central pixel.

real procedure ecxdx (x, i, f, tb)

real	x, f[2], tb[2], dx
int	i

begin
	if (IS_INDEF(f[i]))
	    return (1.)

	dx = x / f[i]
	dx = (1 - dx * tb[i]) / sqrt ((1 + dx * dx) ** 3)
	return (dx)
end


# ECW2X -- Given wavelength return pixel position.

real procedure ecw2x (w, i, a, b, f, tb, ctb, t2tb)

real	w, a[2], b[2], f[2], tb[2], ctb[2], t2tb[2], x
int	i

begin
	if (IS_INDEF(f[i]))
	    return ((w - a[i]) / b[i])

	x = (w - a[i]) / b[i]
	if (x >= 1. || x <= -ctb[i])
	    return (INDEF)
	x = x / sqrt (1 - x * x)
	x = f[i] * (x - tb[i]) / (1 + x * tb[i])
	return (x)

end


# ECW2XR -- Given wavelength return pixel position of reflected component.

real procedure ecw2xr (w, i, a, b, f, tb, ctb, t2tb)

real	w, a[2], b[2], f[2], tb[2], ctb[2], t2tb[2], x
int	i

begin
	if (IS_INDEF(f[i]))
	    return (INDEF)

	x = (w - a[i]) / b[i]
	if (x >= 1. || x <= ctb[i])
	    return (INDEF)
	x = x / sqrt (1 - x * x)
	x = (x - tb[i]) / (1 + x * tb[i])
	x = f[i] * (x + t2tb[i]) / (1 - x * t2tb[i])
	return (x)
end


# ECDELTA -- Given pixel position and wavelength return blaze function
# phase angle.

real procedure ecdelta (x, w, i, f, c, tt)

real	x, w, f[2], c[2], tt[2], d
int	i

begin
	if (IS_INDEF(f[i]))
	    return (c[i] / w * x)

	d = x / f[i]
	d = 1 / sqrt (1 + d * d)
	d = c[i] / w * (d * x / f[i] + tt[i] * (1 - d))
	return (d)
end
