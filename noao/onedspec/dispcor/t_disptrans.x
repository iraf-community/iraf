include	<error.h>
include	<imhdr.h>
include	<math/curfit.h>
include	<smw.h>
include	<units.h>

define	AIRVAC	"|none|air2vac|vac2air|"
define	NONE	1		# No correction
define	AIR2VAC	2		# Correct air to vacuum
define	VAC2AIR	3		# Correct vacuum to air


# T_DISPTRANS -- Tranform dispersion systems and apply air-vac conversion.
# This task uses the UNITS package to convert the input dispersion
# coordinates to the desired output coordinates.  An air to vacuum or
# vacuum to air correction is made.  Since the input and output units
# may not be linearly related and the MWCS supports only polynomial
# representations a cubic splines are fit to the desired output coordinates
# until an error tolerance is reached.  The user may then select to
# store the new WCS as either the spline approximation or to linearize
# the coordinates by resampling the data.  Note that if the input and
# output units ARE linearly related and there is no air/vacuum conversion
# then linearization or storing of a nonlinear dispersion function is
# skipped.  The operations are done in double precision.

procedure t_disptrans ()

int	inlist			# List of input spectra
int	outlist			# List of output spectra
pointer	units			# Output dispersion units
double	maxerr			# Maximum error (in pixels)
bool	linearize		# Linearize ouptut dispersion?
bool	verbose			# Verbose?

int	air			# Air-vacuum conversion?
double	t			# Temperture in degrees C
double	p			# Pressure in mmHg
double	f			# Water vapour pressure in mmHg

int	i, j, n, nw, format, dtype, dtype1, axis[2]
double	err, w1, dw
pointer	ptr, in, out, mwin, mwout, ctin, ctout, sh, cv, inbuf, outbuf
pointer	sp, input, output, title, coeff, x, y, w, nx

bool	clgetb(), streq()
int	clgwrd(), imtopenp(), imtgetim()
double	clgetd(), shdr_lw(), dcveval()
pointer	immap(), smw_openim(), smw_sctran(), mw_open(), imgl3r(), impl3r()
errchk	immap, impl3r
errchk	smw_openim, smw_gwattrs, shdr_open, mw_open
errchk	dt_airvac, dt_cvfit, dt_setwcs, dispcor

data	axis/1,2/

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (units, SZ_FNAME, TY_CHAR)
	call salloc (title, SZ_LINE, TY_CHAR)
	coeff = NULL

	# Parameters
	inlist = imtopenp ("input")
	outlist = imtopenp ("output")
	call clgstr ("units", Memc[units], SZ_FNAME)
	maxerr = clgetd ("error")
	linearize = clgetb ("linearize")
	verbose = clgetb ("verbose")
	air = clgwrd ("air", Memc[input], SZ_FNAME, AIRVAC)
	t = clgetd ("t")
	p = clgetd ("p")
	f = clgetd ("f")

	# Loop over input images.
	while (imtgetim (inlist, Memc[input], SZ_FNAME) != EOF) {
	    if (imtgetim (outlist, Memc[output], SZ_FNAME) == EOF)
		call strcpy (Memc[input], Memc[output], SZ_FNAME)

	    iferr {
		in = NULL
		out = NULL
		mwin = NULL
		mwout = NULL
		ctin = NULL
		ctout = NULL
		sh = NULL
		cv = NULL

		# Open input and output images and wcs.
		if (streq (Memc[input], Memc[output]))
		    ptr = immap (Memc[input], READ_WRITE, 0)
		else
		    ptr = immap (Memc[input], READ_ONLY, 0)
		in = ptr

		if (streq (Memc[input], Memc[output]))
		    ptr = in
		else
		    ptr = immap (Memc[output], NEW_COPY, in)
		out = ptr

		ptr = smw_openim (in); mwin = ptr
		format = SMW_FORMAT(mwin)
		switch (format) {
		case SMW_ND:
		    call error (1,
			"DISPTRANS does not apply to 2D and 3D images")
		case SMW_ES:
		    call smw_esms (mwin)
		}

		if (IM_NDIM(out) == 3 && IM_LEN(out,3) == 1)
		    IM_NDIM(out) = 2
		i = max (2, IM_NDIM(out))
		ptr = mw_open (NULL, i); mwout = ptr
		call mw_newsystem (mwout, "multispec", i)
		call mw_swtype (mwout, axis, 2, "multispec", "")
		if (i == 3)
		    call mw_swtype (mwout, 3, 1, "linear", "")
		call smw_open (mwout, NULL, out)
		    
		# Allocate and set arrays.
		call malloc (x, SMW_LLEN(mwin,1), TY_DOUBLE)
		call malloc (y, SMW_LLEN(mwin,1), TY_DOUBLE)
		call malloc (w, SMW_LLEN(mwin,1), TY_DOUBLE)
		call malloc (nx, SMW_NSPEC(mwin), TY_INT)
		do i = 1, SMW_LLEN(mwin,1)
		    Memd[x+i-1] = i

		# Set the output MWCS dispersion function.
		# Only compute new coordinates once if possible.

		dtype = DCLINEAR
		do i = 1, SMW_NSPEC(mwin) {
		    if (format == SMW_MS || i == 1) {
			call shdr_open (in, mwin, i, 1, INDEFI, SHDATA, sh)
			call shdr_units (sh, Memc[units])
			n = SN(sh)
			do j = 1, n
			    Memd[y+j-1] = shdr_lw (sh, Memd[x+j-1])
			call dt_airvac (sh, Memd[y], n, air, t, p, f)

			# Fit dispersion function.
			dtype1 = DCLINEAR
			call dt_cvfit (cv, CHEBYSHEV, 2, Memd[x], Memd[y],
			    Memd[w], n, err)
			if (err > maxerr) {
			    dtype1 = DCFUNC
			    do j = 1, n-4 {
				call dt_cvfit (cv, SPLINE3, j, Memd[x],
				    Memd[y], Memd[w], n, err)
				if (err <= maxerr)
				    break
			    }
			}

			w1 = dcveval (cv, 1D0)
			dw = (dcveval (cv, double(n)) - w1) / (n - 1)
		    }
		    if (linearize) {
			call dt_setwcs (cv, mwin, mwin, i, dtype1, w1, dw)
			call dt_setwcs (cv, mwin, mwout, i, DCLINEAR, w1, dw)
			if (dtype1 != DCLINEAR)
			    dtype = dtype1
		    } else
			call dt_setwcs (cv, mwin, mwout, i, dtype1, w1, dw)
		    Memi[nx+i-1] = n
		}
		call dcvfree (cv)

		# Set label and units.  The check on unit class is done
		# so that if not a velocity the dictionary expansion
		# unit is used.  However for velocity the units do not
		# include the reference coordinate so the user string
		# is used.

		call mw_swattrs (SMW_MW(mwout,0), 1, "label", LABEL(sh))
		if (UN_CLASS(UN(sh)) != UN_VEL) {
		    call mw_swattrs (SMW_MW(mwout,0), 1, "units", UNITS(sh))
		    call mw_swattrs (SMW_MW(mwout,0), 1, "units_display",
			UNITS(sh))
		} else {
		    call mw_swattrs (SMW_MW(mwout,0), 1, "units", Memc[units])
		    call mw_swattrs (SMW_MW(mwout,0), 1, "units_display",
			Memc[units])
		}

		# Linearize or copy the pixels as requested.
		if (linearize && dtype != DCLINEAR) {
		    ptr = smw_sctran (mwin, "world", "logical", 3); ctin = ptr
		    ptr = smw_sctran (mwout, "logical", "world", 3); ctout = ptr
		    n = IM_LEN(in,1)
		    do j = 1, IM_LEN(out,3) {
			do i = 1, IM_LEN(out,2) {
			    nw = Memi[nx+i-1]
			    inbuf = imgl3r (in, i, j)
			    outbuf = impl3r (out, i, j)
			    call dispcor (ctin, i, ctout, i, Memr[inbuf], n,
				Memr[outbuf], nw, NO)
			    if (nw < n)
				call amovkr (Memr[outbuf+nw-1], Memr[outbuf+nw],
				    n-nw)
			}
		    }
		    call smw_ctfree (ctin)
		    call smw_ctfree (ctout)
		} else if (in != out) {
		    n = IM_LEN(in,1)
		    do j = 1, IM_LEN(out,3) {
			do i = 1, IM_LEN(out,2) {
			    inbuf = imgl3r (in, i, j)
			    outbuf = impl3r (out, i, j)
			    call amovr (Memr[inbuf], Memr[outbuf], n)
			}
		    }
		}

		# Verbose output
		if (verbose) {
		    call printf ("%s: Dispersion transformed to %s")
			call pargstr (Memc[output])
			call pargstr (UNITS(sh))
		    switch (air) {
		    case 1:
			call printf (".\n")
		    case 2:
			call printf (" in vacuum with\n")
			call printf (
			    "  t = %.4g C, p = %.6g mmHg, f = %.4g mmHg.\n")
			    call pargd (t)
			    call pargd (p)
			    call pargd (f)
		    case 3:
			call printf (" in air with\n")
			call printf (
			    "  t = %.4g C, p = %.6g mmHg, f = %.4g mmHg.\n")
			    call pargd (t)
			    call pargd (p)
			    call pargd (f)
		    }
		    call flush (STDOUT)
		}

	    } then {
		if (out != NULL && out != in) {
		    call imunmap (out)
		    call imdelete (Memc[output])
		}
		call erract (EA_WARN)
	    }

	    # Finish up.
	    call mfree (x, TY_DOUBLE)
	    call mfree (y, TY_DOUBLE)
	    call mfree (w, TY_DOUBLE)
	    call mfree (nx, TY_INT)
	    if (mwout != NULL && out != NULL)
		call smw_saveim (mwout, out)
	    if (sh != NULL)
		call shdr_close (sh)
	    if (ctin != NULL)
		call smw_ctfree (ctin)
	    if (ctout != NULL)
		call smw_ctfree (ctout)
	    if (mwin != NULL)
		call smw_close (mwin)
	    if (mwout != NULL)
		call smw_close (mwout)
	    if (out != NULL && out != in)
		call imunmap (out)
	    if (in != NULL)
		call imunmap (in)
	}

	call imtclose (inlist)
	call imtclose (outlist)
	call mfree (coeff, TY_CHAR)
	call sfree (sp)
end


# DT_AIRVAC -- Convert dispersion coordinates to air or vacuum values.
# The coordinates are first transformed to microns since that is what
# the formulas expect.  After correction they are transformed back to the
# original units.  The index of refraction formulas used are from
# Allen's Astrophysical Quantities (1973).

procedure dt_airvac (sh, x, n, air, t, p, f)

pointer	sh		#I Spectrum pointer
double	x[n]		#U Dispersion vector
int	n		#I Number of pixels
int	air		#I Correction type
double	t		#I Temperture in deg C
double	p		#I Total pressure in mmHg
double	f		#I Water vapour pressure in mmHg

int	i
double	x2, a
pointer	un, un_open()
errchk	un_open, un_ctrand

begin
	if (air == NONE)
	    return

	un = un_open ("microns")
	call un_ctrand (UN(sh), un, x, x, n)
	do i = 1, n {
	    x2 = 1 / x[i] **2
	    a = 64.328 + 29498.1 / (146 - x2) + 255.4 / (41 - x2)
	    a = a * p * (1 + (1.049 - 0.0157 * t) * 1e-6 * p) /
		(720.883 * (1 + 0.003661 * t))
	    a = a - (0.0624 - 0.000680 * x2) / (1 + 0.003661 * t) * f
	    a = 1 + a / 1e6
	    switch (air) {
	    case AIR2VAC:
		x[i] = a * x[i]
	    case VAC2AIR:
		x[i] = x[i] / a
	    }
	}
	call un_ctrand (un, UN(sh), x, x, n)
	call un_close (un)
end


# DT_CVFIT -- Fit a dispersion function and return the curfit pointer and
# maximum error in pixels.

procedure dt_cvfit (cv, func, order, x, y, w, n, maxerr)

pointer	cv		#O Fitted dispersion function
int	func		#I Dispersion function type
int	order		#I Dispersion function order
double	x[n]		#I Pixel coordinates
double	y[n]		#I Desired world coordinates
double	w[n]		#O Errors in pixels
int	n		#I Number of pixels
double	maxerr		#O Maximum error

int	i
double	minerr, dcveval()

begin
	if (cv != NULL)
	    call dcvfree (cv)
	call dcvinit (cv, func, order, x[1], x[n])
	call dcvfit (cv, x, y, w, n, WTS_UNIFORM, i)
	do i = 2, n-1
	    w[i] = abs ((y[i] - dcveval (cv, x[i])) / ((y[i+1] - y[i-1]) / 2))
	w[1] = abs ((y[1] - dcveval (cv, x[1])) / (y[2] - y[1]))
	w[n] = abs ((y[n] - dcveval (cv, x[n])) / (y[n] - y[n-1]))
	call alimd (w, n, minerr, maxerr)
end


# DT_SETWCS -- Set the multispec WCS.  If the type is nonlinear then
# the fitted function is stored.

procedure dt_setwcs (cv, mwin, mwout, l, dtype, w1, dw)

pointer	cv		#I Dispersion function
pointer mwin		#I Input SMW pointer
pointer mwout		#I Output, SMW pointer
int	l		#I Image line
int	dtype		#I Dispersion function type
double	w1		#I Coordinate of first pixel
double	dw		#I Coordinate interval at first physical pixel

int	i, ap, bm, dt, nw, n, fd, dcvstati(), stropen()
double	a, b, z, lw, up
pointer	sp, title, coeff, coeffs

begin
	call smark (sp)
	call salloc (title, SZ_LINE, TY_CHAR)

	coeff = NULL
	call smw_gwattrs (mwin, l, 1, ap, bm, dt, a, b, nw, z, lw, up, coeff)
	call smw_gapid (mwin, l, 1, Memc[title], SZ_LINE)

	switch (dtype) {
	case DCFUNC:
	    n = dcvstati (cv, CVNSAVE)
	    call malloc (coeffs, n, TY_DOUBLE)
	    call dcvsave (cv, Memd[coeffs])
	    call realloc (coeff, 20*(n+2), TY_CHAR)
	    fd = stropen (Memc[coeff], 20*(n+2), NEW_FILE)
	    call fprintf (fd, "1 0 %d %d")
	    call pargi (nint (Memd[coeffs]))
	    call pargi (nint (Memd[coeffs+1]))
	    do i = 2, n-1 {
		call fprintf (fd, " %g")
		    call pargd (Memd[coeffs+i])
	    }
	    call close (fd)
	    call mfree (coeffs, TY_DOUBLE)
	default:
	    Memc[coeff] = EOS
	}
	dt = dtype
	a = w1
	b = dw
	z = 0.
	call smw_swattrs (mwout, l, 1, ap, bm, dt, a, b, nw, z, lw, up,
	    Memc[coeff])
	call smw_sapid (mwout, l, 1, Memc[title])

	call mfree (coeff, TY_CHAR)
	call sfree (sp)
end
