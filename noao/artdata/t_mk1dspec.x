include	<error.h>
include	<imhdr.h>


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

bool	new
int	i, j
real	w, x, x1, x2
pointer	sp, input, output, lines, in, out, waves, peaks, sigmas, spec, buf

long	clgetl()
int	clgeti(), imtopenp(), imtlen(), imtgetim(), imaccess()
int	nowhite(), access(), open(), fscan(), nscan()
real	clgetr(), urand()
pointer	immap(), imgl1r(), impl1r()
bool	streq()
errchk	open()

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (lines, SZ_FNAME, TY_CHAR)

	# Get file lists and fixed parameters.
	ilist = imtopenp ("input")
	olist = imtopenp ("output")
	llist = imtopenp ("lines")
	subsample = 1. / clgeti ("nxsub")
	nsigma = sqrt (2. * log (clgetr ("dynrange")))

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
	    i = nowhite (Memc[lines], Memc[lines], SZ_FNAME)
	    if (access (Memc[lines], 0, 0) == YES) {
	        i = open (Memc[lines], READ_ONLY, TEXT_FILE)
	        nlines = 0
	        while (fscan (i) != EOF) {
		    call gargr (w)
		    call gargr (peak)
		    call gargr (sigma)
		    if (nscan() < 1)
		        next
		    if (nscan() < 3)
	    	        sigma = clgetr ("sigma")
		    if (nscan() < 2)
	    	        peak = clgetr ("peak")
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
		    Memr[waves+nlines] = w
		    Memr[peaks+nlines] = peak
		    Memr[sigmas+nlines] = sigma
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
	        w = wpc * (nw - 1)
	        do i = 0, nlines-1 {
		    Memr[waves+i] = w0 + w * urand (seed)
		    Memr[peaks+i] = peak * urand (seed)
		    Memr[sigmas+i] = sigma
	        }
	        if (nlines > 0 && Memc[lines] != EOS) {
	            i = open (Memc[lines], NEW_FILE, TEXT_FILE)
		    do j = 0, nlines-1 {
			call fprintf (i, "%g %g %g\n")
			    call pargr (Memr[waves+j])
			    call pargr (Memr[peaks+j])
			    call pargr (Memr[sigmas+j])
		    }
	            call close (i)
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
	        wpc = wpc * 1.0e-8
	        x1 = exp (1.4388 / (w * temp))
	        x2 = w**5 * (x1-1.0)
	    }
	    do i = 0, nw-1 {
	        x = cont + slope * i
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
