include	<error.h>
include	<imhdr.h>

define	MAX_HDR		20000			# Maximum user header
define	LEN_COMMENT	70			# Maximum comment length

# T_MK1DSPEC -- Make one dimensional spectra.  New images may be created
# or existing images modified.  The continuum may be a slope and/or
# a blackbody.  A line list may be given or random lines generated.
# The lines may be emission or absorption and may have varying
# widths and strengths.  Subsampling is used.

procedure t_mk1dspec()

int	ilist			# List of input spectra (input param)
int	olist			# List of output spectra (output param)

int	nw			# Number of pixels (ncols param or imlen)
real	w0			# Starting wavelength (wstart param)
real	wpc			# Wavelength per pix (wstart/wend params)
real	z			# Redshift

real	cont			# Continuum at first pixel
real	slope			# Continuum slope per pixel
real	temp			# Blackbody temperture (Kelvin)

int	llist			# List of files containing lines (lines param)
real	sigma			# Sigma of lines (Angstroms)
real	peak			# Peak/continuum
int	nlines			# Number of lines
real	subsample		# Subsampling (nxsub param)
real	nsigma			# Dynamic range of gaussian (dynrange param)
long	seed			# Random number seed

bool	new, ranlist
int	i, j
real	w, x, x1, x2
pointer	sp, input, output, lines, comment
pointer	in, out, waves, peaks, sigmas, spec, buf

long	clgetl(), clktime()
int	clgeti(), imtopenp(), imtlen(), imtgetim(), imaccess()
int	nowhite(), access(), open(), fscan(), nscan()
real	clgetr(), urand()
pointer	immap(), imgl1r(), impl1r()
bool	clgetb(), streq()
errchk	open()

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (lines, SZ_FNAME, TY_CHAR)
	call salloc (comment, LEN_COMMENT, TY_CHAR)

	# Get file lists and fixed parameters.
	ilist = imtopenp ("input")
	olist = imtopenp ("output")
	llist = imtopenp ("lines")
	subsample = 1. / clgeti ("nxsub")
	nsigma = sqrt (2. * log (clgetr ("dynrange")))
	z = clgetr ("rv")
	if (clgetb ("z"))
	    z = 1 + z
	else {
	    z = z / 299792.5
	    z = sqrt ((1 + z) / (1 - z)) 
	}

	if (max (1, imtlen (olist)) != imtlen (ilist))
	    call error (1, "Output image list does not match input image list")

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
		    out = in
	            new = false
	        } else {
	            iferr (in = immap (Memc[input], NEW_IMAGE, 0)) {
			call erract (EA_WARN)
			next
		    }
		    out = in
	            new = true

	            IM_NDIM(out) = 1
	            IM_LEN(out,1) = clgeti ("ncols")
	            IM_PIXTYPE(out) = TY_REAL
		    call clgstr ("title", IM_TITLE(out), SZ_IMTITLE)

		    nw = IM_LEN(out,1)
		    w0 = clgetr ("wstart")
	    	    wpc = (clgetr ("wend") - w0) / (nw - 1)
		    call imaddr (out, "W0", w0)
		    call imaddr (out, "WPC", wpc)
		    call imaddr (out, "CRPIX1", 1.)
		    call imaddr (out, "CRVAL1", w0)
		    call imaddr (out, "CDELT1", wpc)
		    call imaddi (out, "DC-FLAG", 0)

		    call mko_header (out)
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
	        new = false
	    }
	    call get_hdr (in, w0, wpc, nw)

	    # Get the line list if given or create random lines.
	    ranlist = false
	    i = nowhite (Memc[lines], Memc[lines], SZ_FNAME)
	    if (access (Memc[lines], 0, 0) == YES) {
	        i = open (Memc[lines], READ_ONLY, TEXT_FILE)
	        nlines = 0
		x1 = clgetr ("peak")
		x2 = clgetr ("sigma")
	        seed = clgetl ("seed")
	        while (fscan (i) != EOF) {
		    call gargr (w)
		    call gargr (peak)
		    call gargr (sigma)
		    if (nscan() < 1)
		        next
		    if (nscan() < 3)
	    	        sigma = x2
		    if (nscan() < 2)
	    	        peak = x1 * urand (seed)
		    if (nlines == 0) {
		        j = 50
		        call malloc (waves, j, TY_REAL)
		        call malloc (peaks, j, TY_REAL)
		        call malloc (sigmas, j, TY_REAL)
		    } else if (nlines == j) {
		        j = j + 10
		        call realloc (waves, j, TY_REAL)
		        call realloc (peaks, j, TY_REAL)
		        call realloc (sigmas, j, TY_REAL)
		    }
		    Memr[waves+nlines] = z * w
		    Memr[peaks+nlines] = peak / z
		    Memr[sigmas+nlines] = z * sigma
		    nlines = nlines + 1
	        }
	        call close (i)
	    } else {
	        nlines = clgeti ("nlines")
	        peak = clgetr ("peak")
	        sigma = clgetr ("sigma")
	        seed = clgetl ("seed")
	        call malloc (waves, nlines, TY_REAL)
	        call malloc (peaks, nlines, TY_REAL)
	        call malloc (sigmas, nlines, TY_REAL)
	        do i = 0, nlines-1 {
		    w = z * (w0 + wpc * (nw - 1) * urand (seed))
		    x = (w - w0) / wpc / (nw - 1)
		    if (x < 0)
			x = x - int (x - 1)
		    else
		        x = x - int (x)
		    w = w0 + wpc * (nw - 1) * x
		    Memr[waves+i] = w
		    Memr[peaks+i] = peak / z * urand (seed)
		    Memr[sigmas+i] = z * sigma
	        }
	        if (nlines > 0 && Memc[lines] != EOS) {
	            i = open (Memc[lines], NEW_FILE, TEXT_FILE)
		    do j = 0, nlines-1 {
			call fprintf (i, "%g %g %g\n")
			    call pargr (Memr[waves+j] / z)
			    call pargr (Memr[peaks+j] * z)
			    call pargr (Memr[sigmas+j] / z)
		    }
	            call close (i)
	        }
	    }

	    # Add comment history of task parameters.
	    call strcpy ("# ", Memc[comment], LEN_COMMENT)
	    call cnvtime (clktime (0), Memc[comment+2], LEN_COMMENT-2)
	    call mko_comment (out, Memc[comment])
	    call mko_comment (out, "begin\tmk1dspec")
	    call mko_comment1 (out, "rv", 'r', Memc[comment])
	    call mko_comment1 (out, "z", 'b', Memc[comment])
	    call mko_comment1 (out, "wstart", 'r', Memc[comment])
	    call mko_comment1 (out, "wend", 's', Memc[comment])
	    call mko_comment1 (out, "continuum", 'r', Memc[comment])
	    call mko_comment1 (out, "slope", 'r', Memc[comment])
	    call mko_comment1 (out, "temperature", 'r', Memc[comment])
	    if (nlines > 0) {
		if (Memc[lines] != EOS)
	            call mko_comment1 (out, "lines", 's', Memc[comment])
		call sprintf (Memc[comment], LEN_COMMENT, "\tnlines%24t%d")
		    call pargi (nlines)
		call mko_comment (out, Memc[comment])
		if (ranlist) {
	            call mko_comment1 (out, "peak", 'r', Memc[comment])
	            call mko_comment1 (out, "sigma", 'r', Memc[comment])
		    call mko_comment1 (out, "seed", 'i', Memc[comment])
		}
	    }

	    # Make the spectrum.
	    spec = impl1r (out)
	    if (new)
	        call aclrr (Memr[spec], nw)
	    else
		call amovr (Memr[imgl1r(in)], Memr[spec], nw)

	    # Make the lines.
	    call calloc (buf, nw, TY_REAL)
	    do i = 0, nlines-1 {
	        w = (Memr[waves+i] - w0) / wpc + 1.
	        peak = Memr[peaks+i] * subsample
	        sigma = Memr[sigmas+i] / wpc
	        x1 = max (1., w - nsigma * sigma)
	        x2 = min (real (nw), w + nsigma * sigma)
	        cont = -0.5 / sigma**2
	        for (x = x1; x <= x2; x = x + subsample) {
		    j = buf + int (x - 0.5)
		    Memr[j] = Memr[j] + peak * exp (cont * (x-w)**2)
	        }
	    }

	    # Make the continuum.
	    cont = clgetr ("continuum")
	    slope = clgetr ("slope")
	    temp = clgetr ("temperature")
	    if (temp > 0.) {
	        w  = w0 * 1.0e-8
	        x1 = exp (1.4388 / (w * temp))
	        x2 = w**5 * (x1-1.0)

		w = w / z
	        wpc = wpc * 1.0e-8 / z
	    }
	    do i = 0, nw-1 {
	        x = cont + slope / wpc * ((w0 + wpc * i) / z - w0)
	        if (temp > 0.) {
		    x1 = exp (1.4388 / (w * temp))
		    x = x * (x2 / w**5 / (x1-1.0))
		    w = w + wpc
	        }
		if (x > 0.)
	            Memr[spec+i] = Memr[spec+i] +
			max (0., x * (1. + Memr[buf+i]))
		else
		    Memr[spec+i] = Memr[spec+i] + Memr[buf+i]
	    }

	    call mfree (waves, TY_REAL)
	    call mfree (peaks, TY_REAL)
	    call mfree (sigmas, TY_REAL)
	    call mfree (buf, TY_REAL)
	    if (in != out)
	        call imunmap (in)
	    call imunmap (out)
	}

	call imtclose (ilist)
	call imtclose (olist)
	call imtclose (llist)
	call sfree (sp)
end


# GET_HDR -- Get header parameters.

procedure get_hdr (im, w0, wpc, nw)

pointer	im			# IMIO pointer
real	w0			# Starting wavelength
real	wpc			# Wavelength per pixel
int	nw			# Number of pixels

real	crpix, imgetr()

begin
	nw = IM_LEN(im, 1)
	crpix = 1.
	iferr (w0 = imgetr (im, "w0")) {
	    iferr (crpix = imgetr (im, "crpix1"))
		    crpix = 1.
	    iferr (w0 = imgetr (im, "crval1"))
		w0 = 1.
	}
	iferr (wpc = imgetr (im, "wpc"))
	    iferr (wpc = imgetr (im, "cdelt1"))
		wpc = 1.
	    
	w0 = w0 - wpc * (crpix - 1.)
	if (abs (w0) < 0.001) {
	    w0 = w0 * 1e10
	    wpc = wpc * 1e10
	}
end
