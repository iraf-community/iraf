include	<error.h>
include	<imhdr.h>

define	LEN_UA		20000			# Maximum user header
define	LEN_COMMENT	70			# Maximum comment length

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
double	sigma			# Sigma of lines (Angstroms)
double	peak			# Peak/continuum
int	nlines			# Number of lines
double	subsample		# Subsampling (nxsub param)
double	nsigma			# Dynamic range of gaussian (dynrange param)
long	seed			# Random number seed

bool	new, ranlist
int	i, j, dtype
double	w, x, x1, x2, z1
real	aplow[2], aphigh[2]
pointer	sp, input, output, lines, comment, apnum, coeff
pointer	in, out, mw, waves, peaks, sigmas, spec, buf

long	clgetl(), clktime()
int	clgeti(), imtopenp(), imtgetim(), imaccess()
int	nowhite(), access(), open(), fscan(), nscan()
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
	call salloc (apnum, SZ_FNAME, TY_CHAR)
	coeff = NULL

	# Get file lists and fixed parameters.
	ilist = imtopenp ("input")
	olist = imtopenp ("output")
	llist = imtopenp ("lines")
	subsample = 1. / clgeti ("nxsub")
	nsigma = sqrt (2. * log (clgetd ("dynrange")))
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
	        if (imaccess (Memc[input], 0) == YES) {
	            iferr (in = immap (Memc[input], READ_WRITE, 0)) {
			call erract (EA_WARN)
			next
		    }
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
		x1 = clgetd ("peak")
		x2 = clgetd ("sigma")
	        seed = clgetl ("seed")
	        while (fscan (i) != EOF) {
		    call gargd (w)
		    call gargd (peak)
		    call gargd (sigma)
		    if (nscan() < 1)
		        next
		    if (nscan() < 3)
	    	        sigma = x2
		    if (nscan() < 2)
	    	        peak = x1 * urand (seed)
		    if (nlines == 0) {
		        j = 50
		        call malloc (waves, j, TY_DOUBLE)
		        call malloc (peaks, j, TY_DOUBLE)
		        call malloc (sigmas, j, TY_DOUBLE)
		    } else if (nlines == j) {
		        j = j + 10
		        call realloc (waves, j, TY_DOUBLE)
		        call realloc (peaks, j, TY_DOUBLE)
		        call realloc (sigmas, j, TY_DOUBLE)
		    }
		    Memd[waves+nlines] = z * w
		    Memd[peaks+nlines] = peak / z
		    Memd[sigmas+nlines] = z * sigma
		    nlines = nlines + 1
	        }
	        call close (i)
	    } else {
	        nlines = clgeti ("nlines")
	        peak = clgetd ("peak")
	        sigma = clgetd ("sigma")
	        seed = clgetl ("seed")
	        call malloc (waves, nlines, TY_DOUBLE)
	        call malloc (peaks, nlines, TY_DOUBLE)
	        call malloc (sigmas, nlines, TY_DOUBLE)
	        do i = 0, nlines-1 {
		    w = z * (w0 + wpc * (nw - 1) * urand (seed))
		    x = (w - w0) / wpc / (nw - 1)
		    if (x < 0)
			x = x - int (x - 1)
		    else
		        x = x - int (x)
		    w = w0 + wpc * (nw - 1) * x
		    Memd[waves+i] = w
		    Memd[peaks+i] = peak / z * urand (seed)
		    Memd[sigmas+i] = z * sigma
	        }
	        if (nlines > 0 && Memc[lines] != EOS) {
	            i = open (Memc[lines], NEW_FILE, TEXT_FILE)
		    do j = 0, nlines-1 {
			call fprintf (i, "%g %g %g\n")
			    call pargd (Memd[waves+j] / z)
			    call pargd (Memd[peaks+j] * z)
			    call pargd (Memd[sigmas+j] / z)
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
	        w = (Memd[waves+i] - w0) / wpc + 1.
	        peak = Memd[peaks+i] * subsample
	        sigma = Memd[sigmas+i] / wpc
	        x1 = max (1.0D0, w - nsigma * sigma)
	        x2 = min (double (nw), w + nsigma * sigma)
	        cont = -0.5 / sigma**2
	        for (x = x1; x <= x2; x = x + subsample) {
		    j = buf + int (x - 0.5)
		    Memd[j] = Memd[j] + peak * exp (cont * (x-w)**2)
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

	    call mfree (waves, TY_DOUBLE)
	    call mfree (peaks, TY_DOUBLE)
	    call mfree (sigmas, TY_DOUBLE)
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
			call mkh_comment1 (out, "peak", 'd')
			call mkh_comment1 (out, "sigma", 'd')
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
