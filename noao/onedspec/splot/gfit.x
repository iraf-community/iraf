include	<error.h>
include	<mach.h>
include	<gset.h>

define	SQ2PI	2.5066283

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

int	bkgfit, posfit, sigfit
int	i, j, i1, npts, nlines, wc, key
real	w, wyc, wx, wy, wx2, wy2
real	slope, height, flux, cont, sigma, eqw, scale, chisq
bool	fit
pointer	sp, cmd, x, y, z

int	clgcur()
real	model()
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
	call salloc (z, npts, TY_REAL)

	# Scale the data.
	scale = 0.
	do i = 1, npts {
	    Memr[x+i-1] = wcs[i1+i-1]
	    Memr[y+i-1] = pix[i1+i-1]
	    scale = max (scale, abs (Memr[y+i-1]))
	}
	call adivkr (Memr[y], scale, Memr[y], npts)

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

	w = Memr[x+j-1]
	wy = min (0.99, max (0.01, abs (Memr[y+j-1] - wyc - slope * w) / wx))
	sigma = sqrt (-0.5 * (w-Memr[xg])**2 / log (wy))
	w = Memr[x+j+1]
	wy = min (0.99, max (0.01, abs (Memr[y+j+1] - wyc - slope * w) / wx))
	sigma = sigma + sqrt (-0.5 * (w-Memr[xg])**2 / log (wy))
	Memr[sg] = sigma / 2

	iferr (call dofit (bkgfit, posfit, sigfit, Memr[x], Memr[y], npts,
	    wyc, slope, Memr[xg], Memr[yg], Memr[sg], ng, chisq)) {
	    call erract (EA_WARN)
	    fit = false
	    goto done_
	}
	call amulkr (Memr[yg], scale, Memr[yg], ng)
	wyc = (wyc + slope * wx1) * scale
	slope = slope * scale

	# Compute model spectrum with continuum and plot.
	fit = true
	do i = 1, npts {
	    w = wcs[i1+i-1]
	    Memr[z+i-1] = model (w, Memr[xg], Memr[yg], Memr[sg], ng)
	    Memr[z+i-1] = Memr[z+i-1] + wyc + slope * (w - wx1)
	}

	call gseti (gfd, G_PLTYPE, 2)
	call gpline (gfd, wcs[i1], Memr[z], npts)
	call gseti (gfd, G_PLTYPE, 3)
	call gline (gfd, wx1, wyc, wx2, wyc + slope * (wx2 - wx1))
	call gseti (gfd, G_PLTYPE, 1)
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
	    }
	} else {
	    call mfree (xg, TY_REAL)
	    call mfree (yg, TY_REAL)
	    call mfree (sg, TY_REAL)
	    ng = 0
	}

	call sfree (sp)
end
