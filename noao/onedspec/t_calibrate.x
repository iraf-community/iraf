include	<error.h>
include	<imhdr.h>
include	<math/iminterp.h>
include "shdr.h"

define	EXTN_LOOKUP	10	# Interp index for de-extinction
define	VLIGHT	2.997925e18	# Speed of light, Angstroms/sec

# T_CALIBRATE -- Apply extinction correction and flux calibration to spectra.
# The sensitivity function derived from the tasks STANDARD and SENSFUNC
# are applied to the given spectra.  The output may be the same as the
# input or new spectra may be created.
#
# The sensitivity function is contained in an image having its aperture
# number indicated by the trailing integer of the image filename.
# An option, "ignoreaps", can be set to override the appending of the
# aperture number on those cases where no aperture correspondence is
# appropriate.

procedure t_calibrate ()

pointer	inlist		# Input list
pointer	outlist		# Output list
pointer	sens		# Sensitivity image root name
bool	ignoreaps	# Ignore aperture numbers?
bool	extinct		# Apply extinction correction?
bool	flux		# Apply flux calibration?
bool	fnu		# Calibration flux in FNU?

bool	doextinct, doflux
int	i, j, enwaves, ncal
pointer	sp, input, output, temp
pointer	obs, in, mw, sh, out, ewaves, emags, pcal, cal, data

bool	clgetb(), streq()
int	imtgetim(), imtlen()
pointer	imtopenp(), immap(), smw_openim(), impl3r()
errchk	immap, smw_openim, shdr_open, ext_load, cal_getflux, cal_extn, cal_flux

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (sens, SZ_FNAME, TY_CHAR)
	call salloc (temp, SZ_LINE, TY_CHAR)

	# Get task parameters.
	inlist = imtopenp ("input")
	outlist = imtopenp ("output")
	call clgstr ("records", Memc[temp], SZ_LINE)
	call odr_openp (inlist, Memc[temp])
	call odr_openp (outlist, Memc[temp])
	call clgstr ("sensitivity", Memc[sens], SZ_FNAME)
	ignoreaps = clgetb ("ignoreaps")
	extinct = clgetb ("extinct")
	flux = clgetb ("flux")
	fnu = clgetb ("fnu")

	if (!extinct && !flux)
	    call error (0, "No calibration correction specified")

	# Loop over all input images.
	sh = NULL
	obs = NULL
	enwaves = 0
	ncal = 0
	while (imtgetim (inlist, Memc[input], SZ_FNAME) != EOF) {
	    if (imtlen (outlist) > 0) {
		if (imtgetim (outlist, Memc[output], SZ_FNAME) == EOF)
		    break
	    } else
		call strcpy (Memc[input], Memc[output], SZ_FNAME)

	    # Map the image and check its calibration status.
	    iferr (in = immap (Memc[input], READ_ONLY, 0)) {
		call erract (EA_WARN)
		next
	    }
	    mw = smw_openim (in)
	    call shdr_open (in, mw, 1, 1, INDEFI, SHDATA, sh)
	    
	    if (DC(sh) == DCNO) {
		call eprintf ("WARNING: [%s] has no dispersion function\n")
		    call pargstr (Memc[input])
		call mw_close (mw)
		call imunmap (in)
		next
	    }

	    doextinct = extinct && (EC(sh) == ECNO)
	    doflux = flux && (FC(sh) == FCNO)
	    if (!(doextinct || doflux)) {
		call eprintf ("WARNING: [%s] is already calibrated\n")
		    call pargstr (Memc[input])
		call mw_close  (mw)
		call imunmap (in)
		next
	    }

	    # Map the output image.
	    if (streq (Memc[input], Memc[output]))
		call mktemp ("temp", Memc[temp], SZ_LINE)
	    else
		call strcpy (Memc[output], Memc[temp], SZ_LINE)
	    out = immap (Memc[temp], NEW_COPY, in)
	    IM_PIXTYPE(out) = TY_REAL

	    do i = 1, IM_LEN(out,2) {
		call shdr_open (in, mw, i, 1, INDEFI, SHDATA, sh)

		call printf ("[%s][%d]: %s\n")
		    call pargstr (Memc[output])
		    call pargi (AP(sh))
		    call pargstr (TITLE(sh))

		do j = 1, IM_LEN(out,3) {
		    call shdr_open (in, mw, i, j, INDEFI, SHDATA, sh)
		    if (doextinct) {
			if (enwaves == 0)
			    call ext_load (ewaves, emags, enwaves)
			call cal_extn (sh, obs, Memr[ewaves], Memr[emags],
			    enwaves)
		    }
		    if (doflux) {
			if (ignoreaps)
			    AP(sh) = INDEFI
			call cal_getflux (Memc[sens], AP(sh), fnu,
			    pcal, ncal, cal)
			call cal_flux (sh, cal)
		    }

		    data = impl3r (out, i, j)
		    if (IM_LEN(out,1) > SN(sh))
			call aclrr (Memr[data], IM_LEN(out,1))
		    call amovr (Memr[SY(sh)], Memr[data], SN(sh))
		}

		if (doextinct)
		    call printf ("  Extinction correction applied\n")
		if (doflux)
		    call printf ("  Flux calibration applied\n")
		call flush (STDOUT)
	    }

	    call imaddr (out, "AIRMASS", AM(sh))
	    call imaddi (out, "EX-FLAG", EC(sh))
	    call imaddi (out, "CA-FLAG", FC(sh))

	    call mw_close (mw)
	    call imunmap (in)
	    call imunmap (out)
	    if (streq (Memc[input], Memc[output])) {
		call imdelete (Memc[input])
		call imrename (Memc[temp], Memc[output])
	    }
	}

	# Free space
	if (enwaves > 0) {
	    call mfree (ewaves, TY_REAL)
	    call mfree (emags, TY_REAL)
	}
	if (ncal > 0) {
	    do i = 0, ncal-1 {
		cal = Memi[pcal+i]
		call asifree (IM(cal))
		call mw_close (MW(cal))
		call shdr_close (cal)
	    }
	    call mfree (pcal, TY_POINTER)
	}
	if (obs != NULL)
	    call obsclose (obs)
	call shdr_close (sh)
	call imtclose (inlist)
	call imtclose (outlist)
	call sfree (sp)
end


# CAL_EXTN -- Apply extinction correction

procedure cal_extn (sh, obs, ewaves, emags, enwaves)

pointer	sh		# Spectrum data
pointer	obs		# Observatory
real	ewaves[enwaves]	# Extinction wavelength array
real	emags[enwaves]	# Extinction magnitude array
int	enwaves		# Number of wavelengths in extinctions arrays

int	i, j, n
real	latitude, a, ext, obsgetr()
bool	newobs, obshead
pointer	observatory, x, y
errchk	obsimopen, get_airm

begin
	# Determine airmass if needed.
	if (IS_INDEF (AM(sh))) {
	    call malloc (observatory, SZ_FNAME, TY_CHAR)
	    call clgstr ("observatory", Memc[observatory], SZ_FNAME)
	    call obsimopen (obs, IM(sh), Memc[observatory], NO, newobs, obshead)
	    if (newobs)
		call obslog (obs, "CALIBRATE", "latitude", STDOUT)
	    latitude = obsgetr (obs, "latitude")
	    call get_airm (RA(sh), DEC(sh), HA(sh), ST(sh), latitude,
		AM(sh))
	    call mfree (observatory, TY_CHAR)
	}

	# Apply extinction correction.
	a = AM(sh)
	x = SX(sh)
	y = SY(sh)
	n = SN(sh)
	call intrp0 (EXTN_LOOKUP)
	do i = 1, n {
	    call intrp (EXTN_LOOKUP, ewaves, emags, enwaves, Memr[x], ext, j)
	    Memr[y] = Memr[y] * 10.0 ** (0.4 * a * ext)
	    x = x + 1
	    y = y + 1
	}
	    
	EC(sh) = ECYES
end


# CAL_FLUX -- Apply flux calibration

procedure cal_flux (sh, cal)

pointer	sh		# Spectrum data
pointer	cal		# Calibration data

int	i, n, ncal, nout
real	time, ical, dw, asieval()
double	shdr_lw(), shdr_wl()
pointer	asi, x, y
errchk	asifit

begin
	x = SX(sh)
	y = SY(sh)
	n = SN(sh)
	asi = IM(cal)
	ncal = SN(cal)
	nout = 0

	time = IT(sh)
	if (IS_INDEF (time) || time <= 0.)
	    time = 1.0

	do i = 1, n {
	    ical = shdr_wl (cal, double (Memr[x]))
	    if (ical < 1. || ical > ncal) {
		if (ical < 0.5 || ical > ncal + 0.5)
		    nout = nout + 1
		ical = max (1., min (real (ncal), ical))
	    }
	    dw = shdr_lw (sh, double (i+0.5)) - shdr_lw (sh, double (i-0.5))
	    Memr[y] = Memr[y] * asieval (asi, ical) / dw / time
	    x = x + 1
	    y = y + 1
	}

	if (nout > 0) {
	    call eprintf (
	    "  WARNING: %d pixels lie outside of flux calibration limits\n")
		call pargi (nout)
	}

	FC(sh) = FCYES
end


# CAL_GETFLUX -- Get flux calibration data
# The sensitivity spectrum is in peculiar magnitudish units of 2.5 log10
# [counts/sec/A / ergs/cm2/s/A].  This is converted back to reasonable
# numbers to be multiplied into the data spectra.  An interpolation function
# is then fit and stored in the image pointer field.  For efficiency the
# calibration data is saved by aperture so that additional calls simply
# return the data pointer.

procedure cal_getflux (sens, ap, fnu, pcal, ncal, cal)

char	sens[ARB]		# Sensitivity function image or rootname
int	ap			# Aperture
bool	fnu			# Fnu units?
pointer	pcal			# Pointer to cal data
int	ncal			# Number of active cal data structures
pointer	cal			# Calibration data structure

int	i, j, n, onedinterp()
pointer	sp, fname, im, mw, x, y, immap(), smw_openim()
errchk	immap, smw_openim, shdr_open, asifit, onedinterp

begin
	# Check for previously saved calibration
	for (i=0; i<ncal; i=i+1) {
	    cal = Memi[pcal+i]
	    if (AP(cal) == ap)
		break
	}

	# Allocate space for a new data pointer, get the calibration data,
	# and convert to calibration array.

	if (i >= ncal) {
	    call smark (sp)
	    call salloc (fname, SZ_FNAME, TY_CHAR)

	    if (ncal == 0)
		call malloc (pcal, 10, TY_POINTER)
	    else if (mod (ncal, 10) == 0)
		call realloc (pcal, ncal+10, TY_POINTER)

	    if (IS_INDEFI(ap))
	        call strcpy (sens, Memc[fname], SZ_FNAME)
	    else {
		call sprintf (Memc[fname], SZ_FNAME, "%s.%04d")
		    call pargstr (sens)
		    call pargi (ap)
	    }

	    im = immap (Memc[fname], READ_ONLY, 0)
	    mw = smw_openim (im)
	    cal = NULL
	    call shdr_open (im, mw, 1, 1, ap, SHDATA, cal)
	    Memi[pcal+i] = cal
	    call imunmap (im)

	    x = SX(cal)
	    y = SY(cal)
	    n = SN(cal)
	    do j = 1, n {
		Memr[y] = 10.0 ** (-0.4 * Memr[y])
		if (fnu) {
		    Memr[y] = Memr[y] * Memr[x] ** 2 / VLIGHT
		    x = x + 1
		}
		y = y + 1
	    }

	    call asiinit (im, onedinterp())
	    call asifit (im, Memr[SY(cal)], n)
	    IM(cal) = im

	    call mfree (SX(cal), TY_REAL)
	    call mfree (SY(cal), TY_REAL)

	    call sfree (sp)
	}
end
