include	<error.h>
include	<gset.h>
include	<imset.h>
include	<imhdr.h>
include	<math.h>
include	<math/iminterp.h>
include	<pkg/gtools.h>
include	<smw.h>
include	<units.h>
include	<pkg/xtanswer.h>

# Tweak data object definitions.
define	TWK_SLEN	999		  # Length of sample region string
define	TWK_LEN		580		  # Length of data object

define	TWK_TYPE	Memc[P2C($1)]	  # Tweak type (maxchars=19)
define	TWK_SH		Memi[$1+11]	  # Spectrum pointer
define	TWK_CAL		Memi[$1+12]	  # Calibration pointer
define	TWK_WAVE	Memi[$1+13]	  # Pointer to wavelengths
define	TWK_SPEC	Memi[$1+14]	  # Pointer to calibrated spectrum
define	TWK_SHIFT	Memr[P2R($1+15)]  # Shift
define	TWK_DSHIFT	Memr[P2R($1+16)]  # Shift step
define	TWK_SCALE	Memr[P2R($1+17)]  # Scaling factor
define	TWK_DSCALE	Memr[P2R($1+18)]  # Scaling factor step
define	TWK_RG		Memi[$1+19]	  # Range pointer
define	TWK_RMS		Memr[P2R($1+20)]  # RMS in sample regions
define	TWK_OFFSET	Memr[P2R($1+21)]  # Offset in graphs
define	TWK_BOX		Memi[$1+22]	  # Boxcar smoothing size
define	TWK_THRESH	Memr[P2R($1+23)]  # Calibration threshold
define	TWK_SAMPLE	Memc[P2C($1+30)]  # Sample regions (maxchars=999)
define	TWK_HELP	Memc[P2C($1+530)] # Help file (maxchars=99)

# Tweak types.
define	SKYTWEAK	1		# Sky subtraction
define	TELLURIC	2		# Telluric division

# Secondary graph types.
define	GNONE		0		# No graph
define	GCAL		1		# Graph calibration spectrum
define	GDATA		2		# Graph data spectrum


# T_SKYTWEAK -- Sky subtract spectra with shift and scale tweaking.
# The sky calibration spectra are scaled and shifted to best subtract
# sky features.  Automatic and interactive methods are provided.

procedure t_skytweak ()

pointer	twk		# TWK data object

begin
	call malloc (twk, TWK_LEN, TY_STRUCT)
	call strcpy ("SKYTWEAK", TWK_TYPE(twk), 19)
	call strcpy ("onedspec$doc/skytweak.key", TWK_HELP(twk), 99)
	call tweak (twk)
	call mfree (twk, TY_STRUCT)
end


# T_TELLURIC -- Correct spectra for telluric features.
# The telluric calibration spectra are scaled by raising to a power (Beers law)
# and shifted to best remove telluric features.  Automatic and interactive
# methods are provided.

procedure t_telluric ()

pointer	twk		# TWK data object

begin
	call malloc (twk, TWK_LEN, TY_STRUCT)
	call strcpy ("TELLURIC", TWK_TYPE(twk), 19)
	call strcpy ("onedspec$doc/telluric.key", TWK_HELP(twk), 99)
	call tweak (twk)
	call mfree (twk, TY_STRUCT)
end


# TWEAK -- Tweak spectra for shift and scale before applying a correction.
# This procedure implements both sky subtraction and telluric division.

procedure tweak (twk)

pointer	twk		#I TWK data object

pointer	inlist		# Input list
pointer	outlist		# Output list
pointer	callist		# Calibration list
bool	xcorr		# Cross correlate for initial shift
bool	tweakrms	# Tweak to minimize RMS?
bool	ignoreaps	# Ignore aperture numbers?
int	lag		# Cross correlation lag
bool	interactive	# Interactive?

int	i, j, k, n, nout, ncal, answer
real	shift, scale, fcor, ical, mean
pointer	sp, input, output, calname, temp
pointer	in, smw, sh, out, pcal, cal, x, y, data, tmp

int	clgeti(), imtgetim(), imtlen()
bool	clgetb(), streq()
real	clgetr(), asieval()
double	shdr_wl(), shdr_lw()
pointer	imtopenp(), immap(), smw_openim(), impl3r(), imgl3r()
errchk	immap, smw_openim, shdr_open, twk_gcal, twk_tweak, impl3r, imgl3r

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (calname, SZ_FNAME, TY_CHAR)
	call salloc (temp, SZ_LINE, TY_CHAR)
	call malloc (TWK_WAVE(twk), 1000, TY_DOUBLE)
	call malloc (TWK_SPEC(twk), 1000, TY_REAL)

	# Get task parameters.
	inlist = imtopenp ("input")
	outlist = imtopenp ("output")
	callist = imtopenp ("cal")
	ignoreaps = clgetb ("ignoreaps")
	if (TWK_TYPE(twk) == 'T')
	    TWK_THRESH(twk) = clgetr ("threshold")
	TWK_SHIFT(twk) = clgetr ("shift")
	TWK_SCALE(twk) = clgetr ("scale")
	xcorr = clgetb ("xcorr")
	tweakrms = clgetb ("tweakrms")
	interactive = clgetb ("interactive")
	if (interactive)
	    answer = YES
	else
	    answer = ALWAYSNO
	call clgstr ("sample", TWK_SAMPLE(twk), TWK_SLEN)
	lag = clgeti ("lag")
	TWK_DSHIFT(twk) = max (0., clgetr ("dshift"))
	TWK_DSCALE(twk) = max (0., min (0.99, clgetr ("dscale")))
	TWK_OFFSET(twk) = clgetr ("offset")
	TWK_BOX(twk) = max (1, clgeti ("smooth"))

	if (imtlen (inlist) != imtlen (callist) && imtlen (callist) != 1) {
	    call imtclose (inlist)
	    call imtclose (outlist)
	    call imtclose (callist)
	    call sfree (sp)
	    call error (1, "Image lists do not match")
	}

	# Loop over all input images.
	sh = NULL
	ncal = 0
	while (imtgetim (inlist, Memc[input], SZ_FNAME) != EOF) {
	    if (imtgetim (callist, Memc[calname], SZ_FNAME) != EOF) {
		if (ncal > 0) {
		    do i = 0, ncal-1 {
			cal = Memi[pcal+i]
			call asifree (IM(cal))
			call smw_close (MW(cal))
			call shdr_close (cal)
		    }
		    call mfree (pcal, TY_POINTER)
		    ncal = 0
		}
	    }

	    in = NULL; smw = NULL; sh = NULL; out = NULL
	    iferr {
		# Set output image.  Use a temporary image when output=input.
		if (imtlen (outlist) > 0) {
		    if (imtgetim (outlist, Memc[output], SZ_FNAME) == EOF)
			break
		} else
		    call strcpy (Memc[input], Memc[output], SZ_FNAME)

		# Map the input image.
		tmp = immap (Memc[input], READ_ONLY, 0); in = tmp
		tmp = smw_openim (in); smw = tmp
		if (smw == SMW_ND)
		    call error (1, "NDSPEC data not supported")
		call shdr_open (in, smw, 1, 1, INDEFI, SHHDR, sh)

		# Map the output image.
		if (streq (Memc[input], Memc[output]))
		    call mktemp ("temp", Memc[temp], SZ_LINE)
		else
		    call strcpy (Memc[output], Memc[temp], SZ_LINE)
		tmp = immap (Memc[temp], NEW_COPY, in); out = tmp
		if (IM_PIXTYPE(out) != TY_DOUBLE)
		    IM_PIXTYPE(out) = TY_REAL

		# Determine airmass if needed.
		if (TWK_TYPE(twk) == 'T') {
		    if (IS_INDEF(AM(sh))) {
			call printf ("%s: ")
			    call pargstr (Memc[input])
			call flush (STDOUT)
			AM(sh) = clgetr ("airmass")
		    }
		}

		# Calibrate all spectra in the image.
		# Only the first band is done.
		do i = 1, IM_LEN(in,2) {

		    # Get the spectra.
		    call shdr_open (in, smw, i, 1, INDEFI, SHDATA, sh)
		    call realloc (TWK_WAVE(twk), SN(sh), TY_DOUBLE)
		    x = TWK_WAVE(twk)
		    do k = 1, SN(sh) {
			Memd[x] = shdr_lw (sh, double(k))
			x = x + 1
		    }
		    if (ignoreaps)
			call twk_gcal (twk, Memc[calname], INDEFI,
			    pcal, ncal, cal)
		    else
			call twk_gcal (twk, Memc[calname], AP(sh),
			    pcal, ncal, cal)

		    # Determine the shift and scale.
		    TWK_SH(twk) = sh
		    TWK_CAL(twk) = cal
		    call realloc (SY(cal), SN(sh), TY_REAL)
		    call realloc (TWK_SPEC(twk), SN(sh), TY_REAL)
		    if (answer == NO || answer == YES) {
			call printf ("%s%s: ")
			    call pargstr (IMNAME(sh))
			    call pargstr (IMSEC(sh))
			call flush (STDOUT)
			call xt_clanswer ("answer", answer)
		    }
		    if (answer == YES || answer == ALWAYSYES)
			interactive = true
		    else
			interactive = false
		    call twk_tweak (twk, xcorr, tweakrms, interactive, lag)
		    shift = TWK_SHIFT(twk)
		    if (TWK_TYPE(twk) == 'T')
			scale = TWK_SCALE(twk) * AM(sh) / AM(cal)
		    else
			scale = TWK_SCALE(twk)

		    # Calibrate the output spectrum.
		    nout = 0
		    mean = 0.
		    x = TWK_WAVE(twk)
		    y = SY(sh)
		    n = SN(sh)
		    data = impl3r (out, i, 1)
		    do k = 1, n {
			ical = shdr_wl (cal, Memd[x]) + shift
			if (ical < 1. || ical > SN(cal)) {
			    if (ical < 0.5 || ical > SN(cal) + 0.5)
				nout = nout + 1
			    ical = max (1., min (real(SN(cal)), ical))
			}
			if (TWK_TYPE(twk) == 'T') {
			    fcor = max (TWK_THRESH(twk),
				asieval (IM(cal),ical)) ** scale
			    Memr[data] = Memr[y] / fcor
			    mean = mean + fcor
			} else {
			    fcor = asieval (IM(cal),ical) * scale
			    Memr[data] = Memr[y] - fcor
			}
			x = x + 1
			y = y + 1
			data = data + 1
		    }
		    mean = mean / n
		    if (TWK_TYPE(twk) == 'T')
			call amulkr (Memr[data-n], mean, Memr[data-n], n)
		    do k = n+1, IM_LEN(out,1) {
			Memr[data] = 0
			data = data + 1
		    }

		    # Log the results.
		    if (i == 1) {
			call printf ("%s:\n  Output: %s - %s\n")
			    call pargstr (TWK_TYPE(twk))
			    call pargstr (Memc[output])
			    call pargstr (IM_TITLE(out))
		    }
		    call printf ("  Input: %s%s - %s\n")
			call pargstr (IMNAME(sh))
			call pargstr (IMSEC(sh))
			call pargstr (TITLE(sh))
		    call printf ("  Calibration: %s%s - %s\n")
			call pargstr (IMNAME(cal))
			call pargstr (IMSEC(cal))
			call pargstr (TITLE(cal))
		    call printf ("  Tweak: shift = %.2f, scale = %.3f")
			call pargr (shift)
			call pargr (TWK_SCALE(twk))
		    if (TWK_TYPE(twk) == 'T') {
			call printf (", normalization = %.4g\n")
			    call pargr (mean)
		    } else
			call printf ("\n")
		    if (nout > 0) {
			call printf (
			"  WARNING: %d pixels outside of calibration limits\n")
			    call pargi (nout)
		    }
		    call flush (STDOUT)
		}
		do j = 2, IM_LEN(in,3) {
		    do i = 1, IM_LEN(in,2) {
			y = imgl3r (in, i, j)
			data = impl3r (out, i, j)
			call amovr (Memr[y], Memr[data], IM_LEN(out,1))
		    }
		}
	    } then {
		call erract (EA_WARN)
		if (out != NULL) {
		    call imunmap (out)
		    if (!streq (Memc[input], Memc[output]))
			call imdelete (Memc[output])
		}
	    }

	    # Finish up this image.
	    if (in != NULL)
		call imunmap (in)
	    if (smw != NULL) {
		call smw_close (smw)
		if (sh != NULL)
		    MW(sh) = NULL
	    }
	    if (out != NULL) {
		call imunmap (out)
		if (streq (Memc[input], Memc[output])) {
		    call imdelete (Memc[input])
		    call imrename (Memc[temp], Memc[output])
		}
	    }
	}

	# Finish up.
	if (ncal > 0) {
	    do i = 0, ncal-1 {
		cal = Memi[pcal+i]
		call asifree (IM(cal))
		call smw_close (MW(cal))
		call shdr_close (cal)
	    }
	    call mfree (pcal, TY_POINTER)
	}
	if (sh != NULL)
	    call shdr_close (sh)
	call imtclose (inlist)
	call imtclose (outlist)
	call imtclose (callist)
	call mfree (TWK_SPEC(twk), TY_REAL)
	call sfree (sp)
end


# TWK_GCAL -- Get calibration data
# An interpolation function is fit and stored in the image pointer field.
# For efficiency the calibration data is saved by aperture so that additional
# calls simply return the data pointer.

procedure twk_gcal (twk, calname, ap, pcal, ncal, cal)

pointer	twk			# TWK data object
char	calname[ARB]		# Calibration image name
int	ap			# Aperture
pointer	pcal			# Pointer to cal data
int	ncal			# Number of active cal data structures
pointer	cal			# Calibration data structure

int	i, clgwrd()
pointer	sp, str, im, smw, immap(), smw_openim()
real	clgetr()
errchk	immap, smw_openim, shdr_open, asifit

begin
	# Check for previously saved calibration
	for (i=0; i<ncal; i=i+1) {
	    cal = Memi[pcal+i]
	    if (AP(cal) == ap)
		return
	}

	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Allocate space for a new data pointer and get the calibration data.

	if (ncal == 0)
	    call malloc (pcal, 10, TY_POINTER)
	else if (mod (ncal, 10) == 0)
	    call realloc (pcal, ncal+10, TY_POINTER)

	im = immap (calname, READ_ONLY, 0)
	smw = smw_openim (im)
	cal = NULL
	call shdr_open (im, smw, 1, 1, ap, SHDATA, cal)
	AP(cal) = ap
	Memi[pcal+ncal] = cal
	ncal = ncal + 1
	call imunmap (im)

	call asiinit (im, clgwrd ("interp", Memc[str], SZ_FNAME,II_FUNCTIONS))
	call asifit (im, Memr[SY(cal)], SN(cal))
	IM(cal) = im

	# Determine airmass if needed.
	if (TWK_TYPE(twk) == 'T') {
	    if (IS_INDEF(AM(cal))) {
		call printf ("%s: ")
		    call pargstr (calname)
		call flush (STDOUT)
		AM(cal) = clgetr ("airmass")
	    }
	}

	call sfree (sp)
end


# TWK_TWEAK -- Determine the shift and scale using automatic and interactive
# methods.

procedure twk_tweak (twk, xcorr, tweakrms, interactive, lag)

pointer	twk		#I TWK data object
bool	xcorr		#I Cross correlate for shift
bool	tweakrms	#I Tweak by minimizing RMS?
bool	interactive	#I Interactive fitting?
int	lag		#I Cross correlation lag

int	i, n, nlag
real	ical, asieval()
double	shdr_wl()
pointer	sh, cal, rg, asi, x, y, rg_xrangesd()
errchk	twk_rmsmin, twk_fit

begin
	sh = TWK_SH(twk)
	cal = TWK_CAL(twk)

	# Set ranges.
	rg = rg_xrangesd (TWK_SAMPLE(twk), Memd[TWK_WAVE(twk)], SN(sh))
	call rg_order (rg)
	call rg_merge (rg)
	TWK_RG(twk) = rg

	# Cross correlate for shift.
	if (xcorr && lag > 0) {
	    n = SN(sh)
	    nlag =  n + 2 * lag
	    call malloc (x, nlag, TY_REAL)
	    call malloc (y, nlag, TY_REAL)

	    do i = 0, n-1 {
		ical = max (1D0, min (double(SN(cal)),
		    shdr_wl (cal, Memd[TWK_WAVE(twk)+i])))
		Memr[y+i] = asieval (IM(cal), ical)
	    }

	    call twk_prep (Memr[y], n, Memr[x], nlag)
	    call twk_prep (Memr[SY(sh)], n, Memr[y], nlag)
	    call twk_xcorr (Memr[x], Memr[y], i, rg, lag, asi, TWK_SHIFT(twk),
		ical, 0.5)
	    call asifree (asi)
	    call mfree (x, TY_REAL)
	    call mfree (y, TY_REAL)
	}

	# Tweak by minimizing RMS.
	if (tweakrms)
	    call twk_rmsmin (twk)

	# Do interactive step.
	if (interactive)
	    call twk_fit (twk)

	call rg_free (TWK_RG(twk))
end


# TWK_PREP -- Prepare spectra for correlation: fit continuum, subtract, taper

procedure twk_prep (in, nin, out, nout)

real	in[nin]			# Input spectrum
int	nin			# Number of pixels in input spectrum
real	out[nout]		# Output spectrum
int	nout			# Number of pixels output spectrum (nin+2*lag)

int	i, lag
real	cveval()
pointer	sp, x, w, ic, cv

begin
	call smark (sp)
	call salloc (x, nin, TY_REAL)
	call salloc (w, nin, TY_REAL)

	call ic_open (ic)
	call ic_pstr (ic, "function", "chebyshev")
	call ic_puti (ic, "order", 3)
	call ic_putr (ic, "low", 3.)
	call ic_putr (ic, "high", 1.)
	call ic_puti (ic, "niterate", 5)
	call ic_putr (ic, "grow", 1.)
	call ic_putr (ic, "xmin", 1.)
	call ic_putr (ic, "xmax", real(nin))

	do i = 1, nin {
	    Memr[x+i-1] = i
	    Memr[w+i-1] = 1
	}
	call ic_fit (ic, cv, Memr[x], in, Memr[w], nin, YES, YES, YES, YES)

	lag = (nout - nin) / 2
	do i = 1-lag, 0
	    out[i+lag] = 0.
	do i = 1, lag-1
	    out[i+lag] = (1-cos (PI*i/lag))/2 * (in[i] - cveval (cv, real(i))) 
	do i = lag, nin-lag+1
	    out[i+lag] = (in[i] - cveval (cv, real(i))) 
	do i = nin-lag+2, nin
	    out[i+lag] = (1-cos (PI*(nin+1-i)/lag))/2 *
		(in[i] - cveval (cv, real(i))) 
	do i = nin+1, nin+lag
	    out[i+lag] = 0.

	call cvfree (cv)
	call ic_closer (ic)
	call sfree (sp)
end


# TWK_XCORR -- Correlate spectra, fit profile, and measure center/width

procedure twk_xcorr (spec1, spec2, npix, rg, lag, asi, center, width, level)

real	spec1[npix]		# First spectrum
real	spec2[npix]		# Second spectrum
int	npix			# Number of pixels in spectra
pointer	rg			# Ranges
int	lag			# Maximum correlation lag
pointer	asi			# Pointer to correlation profile interpolator
real	center			# Center of profile
real	width			# Width of profile
real	level			# Level at which width is determined

int	i, j, k, n, ishift, nprof, rg_inrange()
real	x, p, pmin, pmax, asieval()
pointer	sp, prof

begin
	nprof = 2 * lag + 1

	call smark (sp)
	call salloc (prof, nprof, TY_REAL)

	ishift = nint (center)
	n = 0
	do j = -lag, lag {
	    p = 0.
	    do i = 1+lag, npix-lag {
		if (rg_inrange (rg, i-lag) == NO)
		    next
		k = i - j - ishift
		if (k < 1 || k > npix)
		    next
		p = p + spec1[i] * spec2[k]
		n = n + 1
	    }
	    Memr[prof+j+lag] = p
	}
	if (n < 10 * nprof) {
	    call sfree (sp)
	    return
	}

	# Fit interpolator
	call asiinit (asi, II_SPLINE3)
	call asifit (asi, Memr[prof], nprof)

	# Find the minimum and maximum
	center = 1.
	pmin = asieval (asi, 1.)
	pmax = pmin
	for (x=1; x<=nprof; x=x+.01) {
	    p = asieval (asi, x)
	    if (p < pmin)
		pmin = p
	    if (p > pmax) {
		pmax = p
		center = x
	    }
	}

	# Normalize
	pmax = pmax - pmin
	do i = 0, nprof-1
	    Memr[prof+i] = (Memr[prof+i] - pmin) / pmax

	call asifit (asi, Memr[prof], nprof)

	# Find the equal flux points
	for (x=center; x>=1 && asieval (asi,x)>level; x=x-0.01)
	    ;
	width = x
	for (x=center; x<=nprof && asieval (asi,x)>level; x=x+0.01)
	    ;
	width = (x - width - 0.01) / sqrt (2.)
	center = center - lag - 1 + ishift

	call sfree (sp)
end


# TWK_RMSMIN -- Tweak shift and scale to minimize the RMS.
# This changes the shift and scale parameters but not the step.

procedure twk_rmsmin (twk)

pointer	twk		#I TWK data object

int	i
real	lastshift, lastscale
errchk	twk_ashift, twk_ascale

begin
	lastshift = INDEFR
	lastscale = INDEFR
	do i = 1, 2 {
	    if (TWK_SHIFT(twk) == lastshift && TWK_SCALE(twk) == lastscale)
		break
	    lastshift = TWK_SHIFT(twk)
	    call twk_ashift (twk)

	    if (TWK_SHIFT(twk) == lastshift && TWK_SCALE(twk) == lastscale)
		break
	    lastscale = TWK_SCALE(twk)
	    call twk_ascale (twk)
	}
end


# TWK_ASCALE -- Automatically determine scale by minimizing RMS.

procedure twk_ascale (twk)

pointer	twk		#I TWK data object

int	i
real	shift, oscale, dscale, lastscale, scale[3], rms[3]
errchk	twk_spec

begin
	dscale = TWK_DSCALE(twk)
	if (dscale == 0.)
	    return
	oscale = TWK_SCALE(twk)
	shift = TWK_SHIFT(twk)
	do i = 1, 3 {
	    scale[i] = (1 - (i - 2) * dscale) * oscale
	    call twk_spec (twk, shift, scale[i])
	    rms[i] = TWK_RMS(twk)
	    lastscale = TWK_SCALE(twk)
	}
	while (dscale > 0.01) {
	    if (scale[1] / oscale < 0.5 || scale[3] / oscale > 2.) {
		TWK_SCALE(twk) = oscale
		break
	    }
	    if (rms[1] < rms[2]) {
		scale[3] = scale[2]
		scale[2] = scale[1]
		scale[1] = (1 - dscale) * scale[2]
		rms[3] = rms[2]
		rms[2] = rms[1]
		call twk_spec (twk, shift, scale[1])
		rms[1] = TWK_RMS(twk)
		lastscale = TWK_SCALE(twk)
	    } else if (rms[3] < rms[2]) {
		scale[1] = scale[2]
		scale[2] = scale[3]
		scale[3] = (1+dscale) * scale[2]
		rms[1] = rms[2]
		rms[2] = rms[3]
		call twk_spec (twk, shift, scale[3])
		rms[3] = TWK_RMS(twk)
		lastscale = TWK_SCALE(twk)
	    } else {
		dscale = dscale / 2
		scale[1] = (1-dscale) * scale[2]
		scale[3] = (1+dscale) * scale[2]
		call twk_spec (twk, shift, scale[1])
		rms[1] = TWK_RMS(twk)
		call twk_spec (twk, shift, scale[3])
		rms[3] = TWK_RMS(twk)
		lastscale = TWK_SCALE(twk)
	    }
	    if (rms[1] < rms[2])
		TWK_SCALE(twk) = scale[1]
	    else if (rms[3] < rms[2])
		TWK_SCALE(twk) = scale[3]
	    else
		TWK_SCALE(twk) = scale[2]
	}

	if (TWK_SCALE(twk) != lastscale)
	    call twk_spec (twk, shift, TWK_SCALE(twk))
end


# TWK_ASHIFT -- Automatically determine shift by minimizing RMS.

procedure twk_ashift (twk)

pointer	twk		#I TWK data object

int	i
real	scale, oshift, dshift, lastshift, shift[3], rms[3]
errchk	twk_spec

begin
	dshift = TWK_DSHIFT(twk)
	if (dshift == 0.)
	    return
	oshift = TWK_SHIFT(twk)
	scale = TWK_SCALE(twk)
	do i = 1, 3 {
	    shift[i] = oshift + dshift * (i - 2)
	    call twk_spec (twk, shift[i], scale)
	    rms[i] = TWK_RMS(twk)
	    lastshift = TWK_SHIFT(twk)
	}
	while (dshift > 0.01) {
	    if (abs (oshift - shift[2]) > 2.) {
		TWK_SHIFT(twk) = oshift
		break
	    }
	    if (rms[1] < rms[2]) {
		shift[3] = shift[2]
		shift[2] = shift[1]
		shift[1] = shift[2] - dshift
		rms[3] = rms[2]
		rms[2] = rms[1]
		call twk_spec (twk, shift[1], scale)
		rms[1] = TWK_RMS(twk)
		lastshift = TWK_SHIFT(twk)
	    } else if (rms[3] < rms[2]) {
		shift[1] = shift[2]
		shift[2] = shift[3]
		shift[3] = shift[2] + dshift
		rms[1] = rms[2]
		rms[2] = rms[3]
		call twk_spec (twk, shift[3], scale)
		rms[3] = TWK_RMS(twk)
		lastshift = TWK_SHIFT(twk)
	    } else {
		dshift = dshift / 2
		shift[1] = shift[2] - dshift
		call twk_spec (twk, shift[1], scale)
		rms[1] = TWK_RMS(twk)
		shift[3] = shift[2] + dshift
		call twk_spec (twk, shift[3], scale)
		rms[3] = TWK_RMS(twk)
		lastshift = TWK_SHIFT(twk)
	    }
	    if (rms[1] < rms[2])
		TWK_SHIFT(twk) = shift[1]
	    else if (rms[3] < rms[2])
		TWK_SHIFT(twk) = shift[3]
	    else
		TWK_SHIFT(twk) = shift[2]
	}

	if (TWK_SHIFT(twk) != lastshift)
	    call twk_spec (twk, TWK_SHIFT(twk), scale)
end


# TWK_SPEC -- Evaluate the calibrated spectrum with the specified shift
# and scale.  Compute the RMS within the sample regions.  Apply a
# smoothing if necessary.  The output spectrum and shift and scale
# used are returned in the TWK data structure.

procedure twk_spec (twk, shift, scale)

pointer	twk		#I TWK data object
real	shift		#I Shift
real	scale		#I Scale

char	type
pointer	sh, cal, asi, x, y, ycal, z, rg, temp
int	i, j, k, n, ncal, nstat, box, rg_inrange()
real	thresh, amratio, norm, sum1, sum2, xcal, xcal1, zval, asieval()
double	shdr_wl()

begin
	# Dereference the data structures.
	type = TWK_TYPE(twk)
	sh = TWK_SH(twk)
	cal = TWK_CAL(twk)
	asi = IM(cal)
	x = TWK_WAVE(twk)
	y = SY(sh)
	ycal = SY(cal)
	z = TWK_SPEC(twk)
	n = SN(sh)
	ncal = SN(cal)
	rg = TWK_RG(twk)
	thresh = TWK_THRESH(twk)
	amratio = AM(sh) / AM(cal)

	# Evaluate the calibrated spectrum and the statistics.
	norm = 0.
	sum1 = 0.
	sum2 = 0.
	nstat = 0
	do i = 0, n-1 {
	    # Spectra
	    xcal = shdr_wl (cal, Memd[x+i]) + shift
	    xcal1 = max (1., min (real(ncal), xcal))
	    #Memr[ycal+i] = asieval (asi, xcal1) ** (amratio * scale)
	    #Memr[z+i] = Memr[y+i] / (Memr[ycal+i]
	    Memr[ycal+i] = asieval (asi, xcal1)
	    if (type == 'T') {
		Memr[ycal+i] = max (thresh, Memr[ycal+i])
		if (Memr[ycal+i] <= 0.)
		    call error (1,
	    "Calibration spectrum negative or zero (set threshold parameter)")
		Memr[z+i] = Memr[y+i] / (Memr[ycal+i] ** (amratio * scale))
	    } else
		Memr[z+i] = Memr[y+i] - (Memr[ycal+i] * scale)
	    norm = norm + Memr[z+i]
	}

	do i = 3, n-4 {
	    # Statistics
	    if (rg_inrange (rg, i+1) == NO)
		next
#	    if (xcal < 1 || xcal > ncal)
#		next
#	    zval = Memr[z+i]
	    zval = Memr[z+i] - (Memr[z+i-3] + Memr[z+i+3]) / 2
	    sum1 = sum1 + zval
	    sum2 = sum2 + zval * zval
	    nstat = nstat + 1
	}

	# Normalize
	if (TWK_TYPE(twk) == 'T') {
	    norm = norm / n
	    if (norm > 0.) {
		call adivkr (Memr[z], norm, Memr[z], n)
		sum1 = sum1 / norm
		sum2 = sum2 / norm / norm
	    }
	}

	# RMS
	if (nstat == 0)
	    TWK_RMS(twk) = INDEF
	else
	    TWK_RMS(twk) = sqrt (nstat * sum2 - sum1**2) / nstat

	TWK_SHIFT(twk) = shift
	TWK_SCALE(twk) = scale

	# Smooth
	if (TWK_BOX(twk) > 1) {
	    call malloc (temp, n, TY_REAL)
	    box = TWK_BOX(twk)
	    box = min (n, box)
	    i = (1-box) / 2
	    sum1 = 0.
	    for (j=i; j<i+box; j=j+1)
		sum1 = sum1 + Memr[z+max(0,j)]
	    for (k=0; k<n; k=k+1) {
		Memr[temp+k] = sum1
		sum1 = sum1 - Memr[z+max(0,i)] + Memr[z+min(n-1,j)]
		i = i + 1
		j = j + 1
	    }
	    call adivkr (Memr[temp], real(box), Memr[z], n)
	    call mfree (temp, TY_REAL)
	}
end


# TWK_FIT -- Interactive fitting procedure.

procedure twk_fit (twk)

pointer	twk		#I TWK data object

int	i, j, n, newgraph, newdata, key, wcs, pix, clgcur(), gt_geti()
int	graph1, graph2
real	wx, wy, shift[3], scale[3], dy, gt_getr()
double	shdr_wl()
pointer	sp, str, cmd, z[3]
pointer	sh, gp, gt[2], gopen(), gt_init(), rg_xrangesd()
errchk	twk_spec, twk_rmsmin

begin
	sh = TWK_SH(twk)
	n = SN(sh)

	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call salloc (z[1], n, TY_REAL)
	call salloc (z[3], n, TY_REAL)
	z[2] = TWK_SPEC(twk)

	# Initialize the graphics.
	gp = gopen ("stdgraph", NEW_FILE+AW_DEFER, STDGRAPH)
	gt[1] = gt_init ()
	call sprintf (Memc[str], SZ_LINE,
	    "%s: spectrum = %s%s, calibration = %s%s")
	    call pargstr (TWK_TYPE(twk))
	    call pargstr (IMNAME(sh))
	    call pargstr (IMSEC(sh))
	    call pargstr (IMNAME(TWK_CAL(twk)))
	    call pargstr (IMSEC(TWK_CAL(twk)))
	call gt_sets (gt[1], GTTITLE, Memc[str])
	if (UN_LABEL(UN(sh)) != EOS) {
	    call gt_sets (gt[1], GTXLABEL, UN_LABEL(UN(sh)))
	    call gt_sets (gt[1], GTXUNITS, UN_UNITS(UN(sh)))
	} else
	    call gt_sets (gt[1], GTXLABEL, "Pixels")
	call gt_sets (gt[1], GTTYPE, "line")

	gt[2] = gt_init ()
	if (UN_LABEL(UN(sh)) != EOS) {
	    call gt_sets (gt[2], GTXLABEL, UN_LABEL(UN(sh)))
	    call gt_sets (gt[2], GTXUNITS, UN_UNITS(UN(sh)))
	} else
	    call gt_sets (gt[2], GTXLABEL, "Pixels")
	call gt_sets (gt[2], GTTYPE, "line")

	# Cursor loop.
	if (TWK_DSCALE(twk) > 0.)
	    graph1 = 'y'
	else
	    graph1 = 'x'
	graph2 = GCAL
	newdata = YES
	key = 'r'
	repeat {
	    switch (key) {
	    case ':':
		call twk_colon (Memc[cmd], twk, gp, gt, wcs, newdata, newgraph)
	    case '?':
		call twk_colon ("help", twk, gp, gt, wcs, newdata, newgraph)
	    case 'a':
		call twk_rmsmin (twk)
		newdata = YES
	    case 'c':
		if (graph2 == GCAL)
		    graph2 = GNONE
		else
		    graph2 = GCAL
		call gt_setr (gt[2], GTYMIN, INDEF)
		call gt_setr (gt[2], GTYMAX, INDEF)
		newgraph = YES
	    case 'd':
		if (graph2 == GDATA)
		    graph2 = GNONE
		else
		    graph2 = GDATA
		call gt_setr (gt[2], GTYMIN, INDEF)
		call gt_setr (gt[2], GTYMAX, INDEF)
		newgraph = YES
	    case 'e':
		switch (graph1) {
		case 'x':
		    if (TWK_DSHIFT(twk) == 0.)
			TWK_DSHIFT(twk) = 0.1
		    else
			TWK_DSHIFT(twk) = 2 * TWK_DSHIFT(twk)
		case 'y':
		    if (TWK_DSCALE(twk) == 0.)
			TWK_DSCALE(twk) = 0.1
		    else
			TWK_DSCALE(twk) = min (0.99, 2 * TWK_DSCALE(twk))
		}
		newdata = YES
	    case 'q':
		break
	    case 'r':
		newgraph = YES
	    case 's':
		dy = wx
		call printf ("s to add sample region or n for new regions:\n")
	        if (clgcur ("cursor",wx,wy,wcs,key,Memc[cmd],SZ_LINE) == EOF)
		    break
		switch (key) {
		case 'n':
		    call rg_free (TWK_RG(twk))
		    call sprintf (TWK_SAMPLE(twk), TWK_SLEN, "%g:%g")
			call pargr (dy)
			call pargr (wx)
		    TWK_RG(twk) = rg_xrangesd (TWK_SAMPLE(twk),
			Memd[TWK_WAVE(twk)], SN(sh))
		    newdata = YES
		case 's':
		    call rg_free (TWK_RG(twk))
		    if (TWK_SAMPLE(twk) == '*') {
			call sprintf (TWK_SAMPLE(twk), TWK_SLEN, "%g:%g")
			    call pargr (dy)
			    call pargr (wx)
		    } else {
			call sprintf (Memc[cmd], SZ_LINE, ",%g:%g")
			    call pargr (dy)
			    call pargr (wx)
			call strcat (Memc[cmd], TWK_SAMPLE(twk), TWK_SLEN)
		    }
		    TWK_RG(twk) = rg_xrangesd (TWK_SAMPLE(twk),
			Memd[TWK_WAVE(twk)], SN(sh))
		    newdata = YES
		}
	    case 'w':
		call gt_window (gt[wcs], gp, "cursor", newgraph)
		if (wcs == 1) {
		    call gt_setr (gt[2], GTXMIN, gt_getr (gt[1], GTXMIN))
		    call gt_setr (gt[2], GTXMAX, gt_getr (gt[1], GTXMAX))
		    call gt_seti (gt[2], GTXFLIP, gt_geti (gt[1], GTXFLIP))
		} else {
		    call gt_setr (gt[1], GTXMIN, gt_getr (gt[2], GTXMIN))
		    call gt_setr (gt[1], GTXMAX, gt_getr (gt[2], GTXMAX))
		    call gt_seti (gt[1], GTXFLIP, gt_geti (gt[2], GTXFLIP))
		}
	    case 'x', 'y':
		pix = max (1, min (n, nint (shdr_wl (sh, double (wx))))) - 1
		j = 1
		dy = abs (wy - Memr[z[j]+pix])
		do i = 2, 3
		    if (abs (wy - Memr[z[i]+pix]) < dy) {
			j = i
			dy = abs (wy - Memr[z[j]+pix])
		    }
		TWK_SHIFT(twk) = shift[j]
		TWK_SCALE(twk) = scale[j]
		if (j == 2 && graph1 == key) {
		    if (key == 'x')
			TWK_DSHIFT(twk) = TWK_DSHIFT(twk) / 2.
		    else if (key == 'y')
			TWK_DSCALE(twk) = TWK_DSCALE(twk) / 2.
		}
		if (TWK_DSHIFT(twk) == 0.)
		    graph1 = 'y'
		else if (TWK_DSHIFT(twk) == 0.)
		    graph1 = 'x'
		else
		    graph1 = key
		newdata = YES
	    default:
		call printf ("\007\n")
	    }

	    if (newdata == YES) {
		if (graph1 == 'x') {
		    shift[1] = TWK_SHIFT(twk) - TWK_DSHIFT(twk)
		    shift[2] = TWK_SHIFT(twk)
		    shift[3] = TWK_SHIFT(twk) + TWK_DSHIFT(twk)
		    scale[1] = TWK_SCALE(twk)
		    scale[2] = TWK_SCALE(twk)
		    scale[3] = TWK_SCALE(twk)
		} else if (graph1 == 'y') {
		    shift[1] = TWK_SHIFT(twk)
		    shift[2] = TWK_SHIFT(twk)
		    shift[3] = TWK_SHIFT(twk)
		    scale[1] = TWK_SCALE(twk) * (1 - TWK_DSCALE(twk))
		    scale[2] = TWK_SCALE(twk)
		    scale[3] = TWK_SCALE(twk) * (1 + TWK_DSCALE(twk))
		}
		iferr {
		    TWK_SPEC(twk) = z[1]
		    call twk_spec (twk, shift[1], scale[1])
		    call asubkr (Memr[z[1]], TWK_OFFSET(twk), Memr[z[1]], n)
		    TWK_SPEC(twk) = z[3]
		    call twk_spec (twk, shift[3], scale[3])
		    call aaddkr (Memr[z[3]], TWK_OFFSET(twk), Memr[z[3]], n)
		    TWK_SPEC(twk) = z[2]
		    call twk_spec (twk, shift[2], scale[2])
		    newdata = NO
		} then {
		    TWK_SPEC(twk) = z[2]
		    call gt_free (gt[1])
		    call gt_free (gt[2])
		    call gclose (gp)
		    call sfree (sp)
		    call erract (EA_ERROR)
		}

		call sprintf (Memc[str], SZ_LINE, "scale = %5g")
		    call pargr (TWK_SCALE(twk))
		if (graph1 == 'y') {
		    call sprintf (Memc[cmd], SZ_LINE, " +/- %6g")
			call pargr (TWK_DSCALE(twk))
		    call strcat (Memc[cmd], Memc[str], SZ_LINE)
		}
		call sprintf (Memc[cmd], SZ_LINE, ", shift = %.2f")
		    call pargr (TWK_SHIFT(twk))
		call strcat (Memc[cmd], Memc[str], SZ_LINE)
		if (graph1 == 'x') {
		    call sprintf (Memc[cmd], SZ_LINE, " +/- %.2f")
			call pargr (TWK_DSHIFT(twk))
		    call strcat (Memc[cmd], Memc[str], SZ_LINE)
		}
		call sprintf (Memc[cmd], SZ_LINE, ", offset = %3g")
		    call pargr (TWK_OFFSET(twk))
		call strcat (Memc[cmd], Memc[str], SZ_LINE)
		call sprintf (Memc[cmd], SZ_LINE, ", rms = %.3g")
		    call pargr (TWK_RMS(twk))
		call strcat (Memc[cmd], Memc[str], SZ_LINE)
		call gt_sets (gt[1], GTCOMMENTS, Memc[str])

		newgraph = YES
	    }

	    if (newgraph == YES) {
		call twk_graph (twk, gp, gt, graph1, graph2, Memr[SX(sh)],
		    Memr[z[1]], Memr[z[2]], Memr[z[3]], SN(sh))
		newgraph = NO
	    }
	} until (clgcur ("cursor", wx, wy, wcs, key, Memc[cmd], SZ_LINE) == EOF)

	call gt_free (gt[1])
	call gt_free (gt[2])
	call gclose (gp)
	call sfree (sp)
end


# TWK_GRAPH -- Make the interactive graph.

procedure twk_graph (twk, gp, gt, graph1, graph2, x, y1, y2, y3, npts)

pointer	twk		#I TWK data object
pointer	gp		#I GIO pointer
pointer	gt[2]		#I GTOOLS pointer
int	graph1		#I Type for graph 1
int	graph2		#I Type for graph 2
real	x[npts]		#I X values
real	y1[npts]	#I Y values
real	y2[npts]	#I Y values
real	y3[npts]	#I Y values
int	npts		#I Number of values

real	xmin, xmax, ymin, ymax, xmin1, xmax1, ymin1, ymax1

begin
	call gclear (gp)
	call gseti (gp, G_WCS, 1)
	if (graph2 != GNONE) {
	    call gsview (gp, 0.1, 0.9, 0.4, 0.9)
	    call gseti (gp, G_XLABELTICKS, NO)
	    call gt_seti (gt[1], GTDRAWXLABELS, NO)
	}
	call gt_ascale (gp, gt[1], x, y1, npts)
	call ggwind (gp, xmin, xmax, ymin, ymax)
	call gt_ascale (gp, gt[1], x, y2, npts)
	call ggwind (gp, xmin1, xmax1, ymin1, ymax1)
	xmin = min (xmin, xmin1)
	xmax = max (xmax, xmax1)
	ymin = min (ymin, ymin1)
	ymax = max (ymax, ymax1)
	call gt_ascale (gp, gt[1], x, y3, npts)
	call ggwind (gp, xmin1, xmax1, ymin1, ymax1)
	xmin = min (xmin, xmin1)
	xmax = max (xmax, xmax1)
	ymin = min (ymin, ymin1)
	ymax = max (ymax, ymax1)
	call gswind (gp, xmin, xmax, ymin, ymax)
	call gt_swind (gp, gt[1])
	call gt_labax (gp, gt[1])

	call gt_plot (gp, gt[1], x, y1, npts)
	call gt_plot (gp, gt[1], x, y2, npts)
	call gt_plot (gp, gt[1], x, y3, npts)
	call rg_gxmarkr (gp, TWK_SAMPLE(twk), x, npts, 1)

	switch (graph2) {
	case GCAL:
	    call gseti (gp, G_WCS, 2)
	    call gseti (gp, G_YNMAJOR, 3)
	    call gseti (gp, G_XLABELTICKS, YES)
	    call gt_seti (gt[2], GTDRAWXLABELS, YES)
	    call gt_seti (gt[2], GTDRAWTITLE, NO)
	    call gt_ascale (gp, gt[2], x, Memr[SY(TWK_CAL(twk))], npts)
	    call gsview (gp, 0.1, 0.9, 0.1, 0.4)
	    call gswind (gp, xmin, xmax, INDEF, INDEF)
	    call gt_swind (gp, gt[2])
	    call gt_labax (gp, gt[2])
	    call gt_plot (gp, gt[2], x, Memr[SY(TWK_CAL(twk))], npts)
	case GDATA:
	    call gseti (gp, G_WCS, 2)
	    call gseti (gp, G_YNMAJOR, 3)
	    call gseti (gp, G_XLABELTICKS, YES)
	    call gt_seti (gt[2], GTDRAWXLABELS, YES)
	    call gt_seti (gt[2], GTDRAWTITLE, NO)
	    call gt_ascale (gp, gt[2], x, Memr[SY(TWK_SH(twk))], npts)
	    call gsview (gp, 0.1, 0.9, 0.1, 0.4)
	    call gswind (gp, xmin, xmax, INDEF, INDEF)
	    call gt_swind (gp, gt[2])
	    call gt_labax (gp, gt[2])
	    call gt_plot (gp, gt[2], x, Memr[SY(TWK_SH(twk))], npts)
	}
end


# List of colon commands.
define	CMDS	"|help|shift|scale|dshift|dscale|offset|smooth|sample|"
define	HELP	1	# Print help
define	SHIFT	2	# Shift
define	SCALE	3	# Scale factor
define	DSHIFT	4	# Shift intervale
define	DSCALE	5	# Scale factor interval
define	OFFSET	6	# Offset
define	SMOOTH	7	# Boxcar smoothing
define	SAMPLE	8	# Sample

# TWK_COLON -- Act on colon commands.

procedure twk_colon (command, twk, gp, gt, wcs, newdata, newgraph)

char	command[ARB]		#I Colon command
pointer	twk			#I TWK data object
pointer	gp			#I GIO
pointer	gt[2]			#I GTOOLS
int	wcs			#I WCS
int	newdata			#O New data flag
int	newgraph		#O New graph flag

int	ncmd, ival, gt_geti(), strdic(), nscan()
real	rval, gt_getr()
pointer	sp, cmd, rg, rg_xrangesd()

begin
	# Check for GTOOLS command.
	if (command[1] == '/') {
	    call gt_colon (command, gp, gt[wcs], newgraph)
	    if (wcs == 1) {
		call gt_setr (gt[2], GTXMIN, gt_getr (gt[1], GTXMIN))
		call gt_setr (gt[2], GTXMAX, gt_getr (gt[1], GTXMAX))
		call gt_seti (gt[2], GTXFLIP, gt_geti (gt[1], GTXFLIP))
	    } else {
		call gt_setr (gt[1], GTXMIN, gt_getr (gt[2], GTXMIN))
		call gt_setr (gt[1], GTXMAX, gt_getr (gt[2], GTXMAX))
		call gt_seti (gt[1], GTXFLIP, gt_geti (gt[2], GTXFLIP))
	    }
	    return
	}

	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Scan the command string.
	call sscan (command)
	call gargwrd (Memc[cmd], SZ_LINE)
	ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, CMDS)

	# Execute command.
	switch (ncmd) {
	case HELP:
	    call gpagefile (gp, TWK_HELP(twk), TWK_TYPE(twk))
	case SHIFT:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("shift %g\n")
		    call pargr (TWK_SHIFT(twk))
	    } else {
		TWK_SHIFT(twk) = rval
		newdata = YES
	    }
	case SCALE:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("scale %g\n")
		    call pargr (TWK_SCALE(twk))
	    } else {
		TWK_SCALE(twk) = rval
		newdata = YES
	    }
	case DSHIFT:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("dshift %g\n")
		    call pargr (TWK_DSHIFT(twk))
	    } else {
		TWK_DSHIFT(twk) = rval
		newdata = YES
	    }
	case DSCALE:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("dscale %g\n")
		    call pargr (TWK_DSCALE(twk))
	    } else {
		if (rval < 0. || rval >= 1.)
		    call printf ("dscale must be between zero and one\007\n")
		else {
		    TWK_DSCALE(twk) = rval
		    newdata = YES
		}
	    }
	case OFFSET:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("offset %g\n")
		    call pargr (TWK_OFFSET(twk))
	    } else if (rval != TWK_OFFSET(twk)) {
		TWK_OFFSET(twk) = rval
		call gt_setr (gt[1], GTYMIN, INDEF)
		call gt_setr (gt[1], GTYMAX, INDEF)
		newdata = YES
	    }
	case SMOOTH:
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("smooth %d\n")
		    call pargi (TWK_BOX(twk))
	    } else {
		ival = max (1, ival)
		if (ival != TWK_BOX(twk)) {
		    TWK_BOX(twk) = max (1, ival)
		    newdata = YES
		}
	    }
	case SAMPLE:
	    call gargstr (Memc[cmd], SZ_LINE)
	    if (Memc[cmd] == EOS) {
		call printf ("sample %s\n")
		    call pargstr (TWK_SAMPLE(twk))
	    } else {
		ifnoerr (rg = rg_xrangesd (Memc[cmd+1], Memd[TWK_WAVE(twk)],
		    SN(TWK_SH(twk)))) {
		    call rg_free (TWK_RG(twk))
		    call strcpy (Memc[cmd+1], TWK_SAMPLE(twk), TWK_SLEN)
		    TWK_RG(twk) = rg
		    newdata = YES
		} else
		    call erract (EA_WARN)
	    }
	default:
	    call printf ("\007\n")
	}

	call sfree (sp)
end
