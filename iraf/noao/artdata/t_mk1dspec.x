include	<error.h>
include	<imhdr.h>

define	LEN_UA		20000			# Maximum user header
define	LEN_COMMENT	70			# Maximum comment length

# Profile types.
define  PTYPES  "|gaussian|lorentzian|voigt|"
define  GAUSS           1       # Gaussian profile
define  LORENTZ         2       # Lorentzian profile
define  VOIGT           3       # Voigt profile


# T_MK1DSPEC -- Make one dimensional spectra.  New images may be created
# or existing images modified.  The continuum may be a slope and/or
# a blackbody.  A line list may be given or random lines generated.
# The lines may be emission or absorption and may have varying
# widths and strengths.  Subsampling is used.

procedure t_mk1dspec()

int	ilist			# List of input spectra (input param)
int	olist			# List of output spectra (output param)

int	line			# Line number
int	ap			# Aperture
int	beam			# Beam
int	nw			# Number of pixels (ncols param or imlen)
double	w0			# Starting wavelength (wstart param)
double	wpc			# Wavelength per pix (wstart/wend params)
double	z			# Redshift

double	cont			# Continuum at first pixel
double	slope			# Continuum slope per pixel
double	temp			# Blackbody temperture (Kelvin)
int	fnu			# F-nu flux?

int	llist			# List of files containing lines (lines param)
pointer	profile			# Profile type
double	peak			# Peak/continuum
double	gfwhm			# Sigma of Gaussian (Angstroms)
double	lfwhm			# FWHM of Lorentzian (Angstroms)
int	nlines			# Number of lines
double	subsample		# Subsampling (nxsub param)
double	ngfwhm			# Dynamic range of gaussian (dynrange param)
double	nlfwhm			# Dynamic range of lorentzian (dynrange param)
long	seed			# Random number seed

bool	new, ranlist
int	i, j, dtype, ptype
long	seed1
double	w, x, x1, x2, x3, z1
real	v, u, aplow[2], aphigh[2]
pointer	sp, input, output, lines, comment, coeff
pointer	in, out, mw, ptypes, waves, peaks, gfwhms, lfwhms, spec, buf

long	clgetl(), clktime()
int	clgeti(), clgwrd(), imtopenp(), imtgetim()
int	nowhite(), access(), open(), fscan(), nscan(), strdic()
real	urand()
double	clgetd()
pointer	immap(), mw_open(), smw_openim(), imgl2d(), impl2d()
bool	clgetb(), streq()
errchk	open()

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (lines, SZ_FNAME, TY_CHAR)
	call salloc (comment, LEN_COMMENT, TY_CHAR)
	call salloc (profile, SZ_FNAME, TY_CHAR)
	coeff = NULL

	# Get file lists and fixed parameters.
	ilist = imtopenp ("input")
	olist = imtopenp ("output")
	llist = imtopenp ("lines")
	subsample = 1. / clgeti ("nxsub")
	x1 = clgetd ("dynrange")
	ngfwhm = 0.424661 * sqrt (2. * log (x1))
	nlfwhm = sqrt (0.5 * (x1 - 1))
	z = clgetd ("rv")
	if (clgetb ("z"))
	    z = 1 + z
	else {
	    z = z / 299792.5
	    z = sqrt ((1 + z) / (1 - z)) 
	}

	# Loop through input images.  Missing output images take input
	# image name.  Line list files may be missing.

	Memc[lines] = EOS
	while (imtgetim (ilist, Memc[input], SZ_FNAME) != EOF) {
	    if (imtgetim (olist, Memc[output], SZ_FNAME) == EOF)
	        call strcpy (Memc[input], Memc[output], SZ_FNAME)
	    i = imtgetim (llist, Memc[lines], SZ_FNAME)

	    # Map images.  Check for new, existing, and in-place images.
	    if (streq (Memc[input], Memc[output])) {
		ifnoerr (in = immap (Memc[input], READ_WRITE, 0)) {
		    iferr (mw = smw_openim (in)) {
			call imunmap (in)
			call erract (EA_WARN)
			next
		    }
		    out = in
	            new = false
	        } else {
		    iferr (out = immap (Memc[output], NEW_IMAGE, LEN_UA)) {
			call erract (EA_WARN)
			next
		    }
		    in = out
	            new = true

		    call clgstr ("header", Memc[comment], LEN_COMMENT)
		    iferr (call mkh_header (out, Memc[comment], true, false))
			call erract (EA_WARN)

		    IM_LEN(out,1) = clgeti ("ncols")
		    IM_LEN(out,2) = clgeti ("naps")
		    if (IM_LEN(out,2) == 1)
			IM_NDIM(out) = 1
		    else
			IM_NDIM(out) = 2
	            IM_PIXTYPE(out) = TY_REAL
		    call clgstr ("title", IM_TITLE(out), SZ_IMTITLE)

		    i = IM_NDIM(out)
		    mw = mw_open (NULL, i)
		    call mw_newsystem (mw, "equispec", i)
		    call mw_swtype (mw, 1, 1, "linear", "")
		    if (i > 1)
			call mw_swtype (mw, 2, 1, "linear", "")
		    call mw_swattrs (mw, 1, "label", "Wavelength")
		    call mw_swattrs (mw, 1, "units", "Angstroms")
		    call smw_open (mw, NULL, out)

		    dtype = -1
		    nw = IM_LEN(out,1)
		    w0 = 1.
	    	    wpc = 1.
		    aplow[1] = INDEF
		    aplow[2] = INDEF
		    aphigh[1] = INDEF
		    aphigh[2] = INDEF
		    do i = 1, IM_LEN(out,2)
			call smw_swattrs (mw, i, 1, i, i, dtype, w0, wpc, nw,
			    0D0, aplow, aphigh, "")
	        }
	    } else {
	        iferr (in = immap (Memc[input], READ_ONLY, 0)) {
		    call erract (EA_WARN)
		    next
		}
	        iferr (out = immap (Memc[output], NEW_COPY, in)) {
		    call erract (EA_WARN)
		    call imunmap (in)
		    next
		}
		iferr (mw = smw_openim (in)) {
		    call imunmap (in)
		    call imunmap (out)
		    call erract (EA_WARN)
		    next
		}
	        new = false
	    }

	    line = max (1, min (clgeti ("ap"), IM_LEN(out,2)))
	    call smw_gwattrs (mw, line, 1, ap, beam, dtype, w0, wpc, nw,
		z1, aplow, aphigh, coeff)

	    if (dtype < 0) {
		dtype = 0
		nw = min (clgeti ("ncols"), IM_LEN(out,1))
		w0 = clgetd ("wstart")
		wpc = (clgetd ("wend") - w0) / (nw - 1)
		call smw_swattrs (mw, line, 1, ap, beam, dtype, w0, wpc, nw,
		    0D0, aplow, aphigh, "")
	    }

	    # Get the line list if given or create random lines.
	    ranlist = false
	    i = nowhite (Memc[lines], Memc[lines], SZ_FNAME)
	    if (access (Memc[lines], 0, 0) == YES) {
	        i = open (Memc[lines], READ_ONLY, TEXT_FILE)
	        nlines = 0
		dtype = clgwrd ("profile", Memc[profile], SZ_FNAME, PTYPES)
		x1 = clgetd ("peak")
		x2 = clgetd ("gfwhm")
		x3 = clgetd ("lfwhm")
	        seed = clgetl ("seed")
		if (IS_INDEFL(seed))
		    seed1 = seed1 + clktime (long (0))
		else
		    seed1 = seed
	        while (fscan (i) != EOF) {
		    call gargd (w)
		    call gargd (peak)
		    call gargwrd (Memc[profile], SZ_FNAME)
		    call gargd (gfwhm)
		    call gargd (lfwhm)
		    ptype = strdic (Memc[profile], Memc[profile], SZ_FNAME,
			PTYPES)
		    if (ptype == 0)
			ptype = dtype
		    switch (nscan()) {
		    case 0:
			next
		    case 1:
			peak = x1 * urand (seed1)
			ptype = dtype
			gfwhm = x2
			lfwhm = x3
		    case 2:
			ptype = dtype
			gfwhm = x2
			lfwhm = x3
		    case 3:
			gfwhm = x2
			lfwhm = x3
		    case 4:
			switch (ptype) {
			case GAUSS:
			    lfwhm = x3
			case LORENTZ:
			    lfwhm = gfwhm
			    gfwhm = x2
			case VOIGT:
			    lfwhm = x3
			}
		    }

		    if (nlines == 0) {
		        j = 50
		        call malloc (ptypes, j, TY_INT)
		        call malloc (waves, j, TY_DOUBLE)
		        call malloc (peaks, j, TY_DOUBLE)
		        call malloc (gfwhms, j, TY_DOUBLE)
		        call malloc (lfwhms, j, TY_DOUBLE)
		    } else if (nlines == j) {
		        j = j + 10
		        call realloc (ptypes, j, TY_INT)
		        call realloc (waves, j, TY_DOUBLE)
		        call realloc (peaks, j, TY_DOUBLE)
		        call realloc (gfwhms, j, TY_DOUBLE)
		        call realloc (lfwhms, j, TY_DOUBLE)
		    }
		    Memi[ptypes+nlines] = ptype
		    Memd[waves+nlines] = z * w
		    Memd[peaks+nlines] = peak / z
		    Memd[gfwhms+nlines] = z * gfwhm
		    Memd[lfwhms+nlines] = z * lfwhm
		    nlines = nlines + 1
	        }
	        call close (i)
	    } else {
	        nlines = clgeti ("nlines")
		ptype = clgwrd ("profile", Memc[profile], SZ_FNAME, PTYPES)
	        peak = clgetd ("peak")
	        gfwhm = clgetd ("gfwhm")
	        lfwhm = clgetd ("lfwhm")
	        seed = clgetl ("seed")
		if (IS_INDEFL(seed))
		    seed1 = seed1 + clktime (long (0))
		else
		    seed1 = seed
	        call malloc (ptypes, nlines, TY_INT)
	        call malloc (waves, nlines, TY_DOUBLE)
	        call malloc (peaks, nlines, TY_DOUBLE)
	        call malloc (gfwhms, nlines, TY_DOUBLE)
	        call malloc (lfwhms, nlines, TY_DOUBLE)
	        do i = 0, nlines-1 {
		    w = z * (w0 + wpc * (nw - 1) * urand (seed1))
		    x = (w - w0) / wpc / (nw - 1)
		    if (x < 0)
			x = x - int (x - 1)
		    else
		        x = x - int (x)
		    w = w0 + wpc * (nw - 1) * x
		    Memi[ptypes+i] = ptype
		    Memd[waves+i] = w
		    Memd[peaks+i] = peak / z * urand (seed1)
		    Memd[gfwhms+i] = z * gfwhm
		    Memd[lfwhms+i] = z * lfwhm
	        }
	        if (nlines > 0 && Memc[lines] != EOS) {
	            i = open (Memc[lines], NEW_FILE, TEXT_FILE)
		    do j = 0, nlines-1 {
			switch (Memi[ptypes+j]) {
			case GAUSS:
			    call fprintf (i, "%g %g %10s %g\n")
			    call pargd (Memd[waves+j] / z)
			    call pargd (Memd[peaks+j] * z)
			    call pargstr ("gaussian")
			    call pargd (Memd[gfwhms+j] / z)
			case LORENTZ:
			    call fprintf (i, "%g %g %10s %g\n")
			    call pargd (Memd[waves+j] / z)
			    call pargd (Memd[peaks+j] * z)
			    call pargstr ("lorentzian")
			    call pargd (Memd[lfwhms+j] / z)
			case VOIGT:
			    call fprintf (i, "%g %g %10s %g %g\n")
			    call pargd (Memd[waves+j] / z)
			    call pargd (Memd[peaks+j] * z)
			    call pargstr ("voigt")
			    call pargd (Memd[gfwhms+j] / z)
			    call pargd (Memd[lfwhms+j] / z)
			}
		    }
	            call close (i)
	        }
	    }

	    # Make the spectrum.
	    spec = impl2d (out, line)
	    if (new)
	        call aclrd (Memd[spec], IM_LEN(in,1))
	    else
		call amovd (Memd[imgl2d(in, line)], Memd[spec], IM_LEN(in,1))

	    # Make the lines.
	    call calloc (buf, nw, TY_DOUBLE)
	    do i = 0, nlines-1 {
		ptype = Memi[ptypes+i]
	        w = (Memd[waves+i] - w0) / wpc + 1.
	        peak = Memd[peaks+i] * subsample
	        gfwhm = Memd[gfwhms+i] / abs(wpc)
	        lfwhm = Memd[lfwhms+i] / abs(wpc)
		x3 = max (ngfwhm*gfwhm, min (20D0, nlfwhm)*lfwhm)
	        x1 = max (1.0D0, w - x3)
	        x2 = min (double (nw), w + x3)
		switch (ptype) {
		case GAUSS:
		    x3 = -0.360674 * gfwhm**2
		    for (x = x1; x <= x2; x = x + subsample) {
			j = buf + int (x - 0.5)
			Memd[j] = Memd[j] + peak * exp ((x-w)**2 / x3)
		    }
		case LORENTZ:
		    x3 = 0.25 * lfwhm**2
		    for (x = x1; x <= x2; x = x + subsample) {
			j = buf + int (x - 0.5)
			Memd[j] = Memd[j] + peak / (1 + (x-w)**2 / x3)
		    }
		case VOIGT:
		    x3 = 1.66511 / gfwhm
		    cont = (lfwhm / 2 )  * x3
		    call voigt (0., real(cont), v, u)
		    peak = peak / v
		    for (x = x1; x <= x2; x = x + subsample) {
			j = buf + int (x - 0.5)
			call voigt (real((x-w)*x3), real(cont), v, u)
			Memd[j] = Memd[j] + peak * v
		    }
		}
	    }

	    # Make the continuum.
	    cont = clgetd ("continuum")
	    slope = clgetd ("slope")
	    temp = clgetd ("temperature")
	    if (clgetb ("fnu"))
		fnu = 3
	    else
		fnu = 5
	    if (temp > 0.) {
	        w  = w0 * 1.0e-8
	        x1 = exp (1.4388 / (w * temp))
	        x2 = w**fnu * (x1-1.0)

		w = w / z
	        wpc = wpc * 1.0e-8 / z
	    }
	    do i = 0, nw-1 {
	        x = cont + slope / wpc * ((w0 + wpc * i) / z - w0)
	        if (temp > 0.) {
		    x1 = exp (1.4388 / (w * temp))
		    x = x * (x2 / w**fnu / (x1-1.0))
		    w = w + wpc
	        }
		if (x > 0.)
	            Memd[spec+i] = Memd[spec+i] +
			max (0.0D0, x * (1. + Memd[buf+i]))
		else
		    Memd[spec+i] = Memd[spec+i] + Memd[buf+i]
	    }

	    call mfree (ptypes, TY_INT)
	    call mfree (waves, TY_DOUBLE)
	    call mfree (peaks, TY_DOUBLE)
	    call mfree (gfwhms, TY_DOUBLE)
	    call mfree (lfwhms, TY_DOUBLE)
	    call mfree (buf, TY_DOUBLE)

	    # Add comment history of task parameters.
	    if (clgetb ("comments")) {
		call strcpy ("# ", Memc[comment], LEN_COMMENT)
		call cnvtime (clktime (0), Memc[comment+2], LEN_COMMENT-2)
		call mkh_comment (out, Memc[comment])
		call mkh_comment (out, "begin    mk1dspec")
		call mkh_comment1 (out, "ap", 'i')
		call mkh_comment1 (out, "rv", 'd')
		call mkh_comment1 (out, "z", 'b')
		call mkh_comment1 (out, "wstart", 'd')
		call mkh_comment1 (out, "wend", 'd')
		call mkh_comment1 (out, "continuum", 'd')
		call mkh_comment1 (out, "slope", 'd')
		call mkh_comment1 (out, "temperature", 'd')
		call mkh_comment1 (out, "fnu", 'b')
		if (nlines > 0) {
		    if (Memc[lines] != EOS)
			call mkh_comment1 (out, "lines", 's')
		    call sprintf (Memc[comment], LEN_COMMENT, "%9tnlines%24t%d")
			call pargi (nlines)
		    call mkh_comment (out, Memc[comment])
		    if (ranlist) {
			call mkh_comment1 (out, "profile", 's')
			call mkh_comment1 (out, "peak", 'd')
			call mkh_comment1 (out, "gfwhm", 'd')
			call mkh_comment1 (out, "lfwhm", 'd')
			call mkh_comment1 (out, "seed", 'i')
		    }
		}
	    }

	    call smw_saveim (mw, out)
	    call smw_close (mw)
	    if (in != out)
	        call imunmap (in)
	    call imunmap (out)
	}

	call mfree (coeff, TY_CHAR)
	call imtclose (ilist)
	call imtclose (olist)
	call imtclose (llist)
	call sfree (sp)
end
