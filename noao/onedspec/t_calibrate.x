include	<error.h>
include	<imhdr.h>
include	<math/iminterp.h>
include "idsmtn.h"

define	NRANGES		100	# Maximum number of aperture ranges
define	EXTN_LOOKUP	10	# Interp index for de-extinction
define	VLIGHT	2.997925e18	# Speed of light, Angstroms/sec

# Flux calibration structure defined for each aperture.
define	CAL_LEN		5
define	CAL_BEAM	Memi[$1]	# Beam number
define	CAL_W0		Memr[$1+1]	# Starting wavelength
define	CAL_WPC		Memr[$1+2]	# Wavelength per channel
define	CAL_NPTS	Memi[$1+3]	# Number of points
define	CAL_DATA	Memi[$1+4]	# Pointer to calibration data

# T_CALIBRATE -- Apply extinction correction and flux calibration to spectra.
#  The sensitivity function derived from the tasks STANDARD and SENSFUNC
#  are applied to the given spectra.  The output may be the same as the
#  input or new spectra may be created.
#
#  The sensitivity function is contained in an image having its aperture
#  number indicated by the trailing integer of the image filename.
#  An option, "ignoreaps", can be set to override the appending of the
#  aperture number on those cases where no aperture correspondence is
#  appropriate.

procedure t_calibrate ()

pointer	odrin		# Input list
pointer	odrout		# Output list
pointer	sens		# Sensitivity image root name
int	aps		# Aperture list
bool	ignoreaps	# Ignore aperture numbers?
bool	extinct		# Apply extinction correction?
bool	flux		# Apply flux calibration?
bool	fnu		# Calibration flux in FNU?
real	latitude	# Latitude of observation

int	i, extnwaves, nbeams, npts, beam
pointer	sp, input, output, temp, ids
pointer	in, out, data, extwaves, extmags, beams

real	clgetr()
bool	clgetb(), is_in_range(), streq()
int	decode_ranges(), odr_getim(), odr_len()
pointer	immap(), imgl1r(), imgl2r(), impl1r(), impl2r()
errchk	immap

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (temp, SZ_FNAME, TY_CHAR)
	call salloc (sens, SZ_FNAME, TY_CHAR)
	call salloc (aps, 3*NRANGES, TY_INT)
	call salloc (ids, LEN_IDS, TY_STRUCT)
	call salloc (POINT(ids), MAX_NCOEFF, TY_REAL)

	# Get task parameters.
	if (clgetb ("recformat"))
	    call odr_open2 ("input", "output", "records", odrin, odrout)
	else
	    call odr_open2 ("input", "output", "", odrin, odrout)

	call clgstr ("sensitivity", Memc[sens], SZ_FNAME)
	call clgstr ("apertures", Memc[input], SZ_FNAME)
	ignoreaps = clgetb ("ignoreaps")
	extinct = clgetb ("extinct")
	flux = clgetb ("flux")
	fnu = clgetb ("fnu")
	latitude = clgetr ("latitude")

	if (!extinct && !flux)
	    call error (0, "No calibration correction specified")

	if (decode_ranges (Memc[input], Memi[aps], NRANGES, i) == ERR)
	    call error (0, "Bad aperture list specification")

	# Loop over all input images.
	extnwaves = 0
	nbeams = 0
	while (odr_getim (odrin, Memc[input], SZ_FNAME) != EOF) {
	    if (odr_len (odrout) > 0) {
		if (odr_getim (odrout, Memc[output], SZ_FNAME) == EOF)
		    break
	    } else
		call strcpy (Memc[input], Memc[output], SZ_FNAME)

	    # Map the image and check its calibration status.
	    iferr (in = immap (Memc[input], READ_ONLY, 0)) {
		call erract (EA_WARN)
		next
	    }
	    call load_ids_hdr (ids, in, 1)
	    if (!is_in_range (Memi[aps], BEAM(ids))) {
		call imunmap (in)
		next
	    }
	    if (DC_FLAG(ids) == -1) {
		call eprintf ("WARNING: [%s] is not dispersion corrected\n")
		    call pargstr (Memc[input])
		call imunmap (in)
		next
	    }
	    if (!((extinct && EX_FLAG(ids)==-1)||(flux && CA_FLAG(ids)==-1))) {
		call eprintf ("WARNING: [%s] is already calibrated\n")
		    call pargstr (Memc[input])
		call imunmap (in)
		next
	    }

	    # Map the output image.
	    if (streq (Memc[input], Memc[output]))
		call mktemp ("temp", Memc[temp], SZ_FNAME)
	    else
		call strcpy (Memc[output], Memc[temp], SZ_FNAME)
	    out = immap (Memc[temp], NEW_COPY, in)
	    IM_PIXTYPE(out) = TY_REAL

	    do i = 1, IM_LEN(out,2) {
	        # Get spectrum and apply calibrations.
	        if (IM_NDIM(out) == 1) {
		    data = impl1r (out, 1)
		    call amovr (Memr[imgl1r(in)], Memr[data], IM_LEN(out,1))
	        } else {
		    if (i > 1)
	                call load_ids_hdr (ids, in, i)
		    data = impl2r (out, i)
		    call amovr (Memr[imgl2r(in,i)], Memr[data], IM_LEN(out,1))
		}
		data = data + NP1(ids)
		npts = NP2(ids) - NP1(ids)

		beam = BEAM(ids)
		call printf ("[%s][%d]: %s\n")
		    call pargstr (Memc[output])
		    call pargi (beam)
		    call pargstr (IM_TITLE(out))

		if (ignoreaps)
		    beam = INDEFI

		if (extinct && (EX_FLAG(ids) == -1))
		    call cal_extn (ids, latitude, extwaves, extmags, extnwaves,
			Memr[data], npts)
		if (flux && (CA_FLAG(ids) == -1))
		    call cal_flux (ids, Memc[sens], fnu, beam, beams, nbeams,
			Memr[data], npts)

		call flush (STDOUT)
	    }
	    call store_keywords (ids, out)

	    call imunmap (in)
	    call imunmap (out)
	    if (streq (Memc[input], Memc[output])) {
		call imdelete (Memc[input])
		call imrename (Memc[temp], Memc[output])
	    }
	}

	# Free space
	if (extnwaves > 0) {
	    call mfree (extwaves, TY_REAL)
	    call mfree (extmags, TY_REAL)
	}
	if (nbeams > 0) {
	    do i = 0, nbeams-1 {
	        call mfree (CAL_DATA(Memi[beams+i]), TY_REAL)
		call mfree (Memi[beams+i], TY_STRUCT)
	    }
	    call mfree (beams, TY_INT)
	}
	call odr_close (odrin)
	call odr_close (odrout)
	call sfree (sp)
end


procedure cal_extn (ids, latitude, extwaves, extmags, extnwaves, data, npts)

pointer	ids		# Image header
real	latitude	# Observatory latitude
pointer	extwaves	# Pointer to extinction wavelength array
pointer	extmags		# Pointer to extinction magnitude array
int	extnwaves	# Number of wavelength points in extinctions arrays
real	data[npts]	# Data to be extinction corrected
int	npts		# Number of data points

int	i, j
real	w, ext
errchk	get_airm

begin
	# Load extinction table if needed.
	if (extnwaves == 0)
	    call ext_load (extwaves, extmags, extnwaves)

	# Determine airmass if needed.
	if (IS_INDEF (AIRMASS(ids)))
	    call get_airm (RA(ids), DEC(ids), HA(ids), ST(ids), latitude,
		AIRMASS(ids))

	# Apply extinction correction.
	call intrp0 (EXTN_LOOKUP)
	if (DC_FLAG(ids) == 0)
	    do i = 1, npts {
	        w = W0(ids) + (i-1) * WPC(ids)
	        call intrp (EXTN_LOOKUP, Memr[extwaves], Memr[extmags],
		    extnwaves, w, ext, j)
	        data[i] = data[i] * 10.0 ** (0.4 * AIRMASS(ids) * ext)
	    }
	else
	    do i = 1, npts {
	        w = W0(ids) + (i-1) * WPC(ids)
	        call intrp (EXTN_LOOKUP, Memr[extwaves], Memr[extmags],
		    extnwaves, 10. ** w, ext, j)
	        data[i] = data[i] * 10.0 ** (0.4 * AIRMASS(ids) * ext)
	    }

	EX_FLAG(ids) = 0
	call printf ("  Extinction correction applied\n")
end


# CALIBRATE -- Perform the arithemtic calibration

procedure cal_flux (ids, sens, fnu, beam, beams, nbeams, data, npts)

pointer	ids		# Image header parameters
char	sens[ARB]	# Sensitivity image or root name
bool	fnu		# Fnu units?
int	beam		# Beam to use for calibration
pointer	beams		# Pointer to beam calibration structures
int	nbeams		# Number of active beams
real	data[npts]	# Data to be calibrated
int	npts		# Number of data points

int	i, nout
real	expo, w0, wpc, x, w, d1, d2, asieval()
pointer	cal, asi
errchk	asifit

begin
	# Get flux calibration.
	call cal_getflux (sens, fnu, beam, beams, nbeams, cal)

	# Compute correction for dispersion and exposure time
	if (IS_INDEF(ITM(ids)) || (ITM(ids) <= 0.))
	    expo = 1.0
	else
	    expo = ITM(ids)
	w0 = W0(ids)
	wpc = WPC(ids)

	# Correct to wavelength interval.
	x = 1.0 / wpc / expo
	call amulkr (data, x, data, npts)

	# Calibrate.
	d1 = abs ((w0 - CAL_W0(cal)) / w0)
	d2 = abs ((wpc - CAL_WPC(cal)) / wpc)
	if ((d1 < 0.001) && (d2 < 0.001) && (npts == CAL_NPTS(cal)))
	    call amulr (data, Memr[CAL_DATA(cal)], data, npts)
	else {
	    call asiinit (asi, II_SPLINE3)
	    call asifit (asi, Memr[CAL_DATA(cal)], CAL_NPTS(cal))

	    nout = 0
	    w = w0 - wpc
	    do i = 1, npts {
		w = w + wpc
		x = (w - CAL_W0(cal)) / CAL_WPC(cal) + 1
		if (x < 1. || x > CAL_NPTS(cal)) {
		    x = max (1., min (real (CAL_NPTS(cal)), x))
		    nout = nout + 1
		}
		data[i] = data[i] * asieval (asi, x)
	    }
	    call asifree (asi)

	    if (nout > 0) {
	        call eprintf (
                "  WARNING: %d pixels lie outside of flux calibration limits\n")
		    call pargi (nout)
	    }
	}

	CA_FLAG(ids) = 0
	call printf ("  Flux calibration applied\n")
end



procedure cal_getflux (sens, fnu, beam, beams, nbeams, cal)

char	sens[ARB]		# Sensitivity function image or rootname
bool	fnu			# Fnu units?
int	beam			# Beam number to use for calibration
pointer	beams			# Pointer to beam data
int	nbeams			# Number of active beams
pointer	cal			# Calibration data structure (returned)

int	i, j
real	w
pointer	sp, fname, ids, data, im, immap(), imgl1r()
errchk	immap

begin
	for (i=0; i<nbeams; i=i+1)
	    if (CAL_BEAM(Memi[beams+i]) == beam)
		break

	if (i >= nbeams) {
	    call smark (sp)
	    call salloc (fname, SZ_FNAME, TY_CHAR)
	    call salloc (ids, LEN_IDS, TY_STRUCT)
	    call salloc (POINT(ids), MAX_NCOEFF, TY_REAL)

	    if (nbeams == 0)
		call malloc (beams, 10, TY_INT)
	    else if (mod (nbeams, 10) == 0)
		call realloc (beams, nbeams+10, TY_INT)
	    call malloc (Memi[beams+i], CAL_LEN, TY_STRUCT)

	    if (IS_INDEFI(beam))
	        call strcpy (sens, Memc[fname], SZ_FNAME)
	    else {
		call sprintf (Memc[fname], SZ_FNAME, "%s.%04d")
		    call pargstr (sens)
		    call pargi (beam)
	    }

	    im = immap (Memc[fname], READ_ONLY, 0)
	    call load_ids_hdr (ids, im, 1)

	    cal = Memi[beams+i]
	    CAL_BEAM(cal) = beam
	    CAL_W0(cal) = W0(ids)
	    CAL_WPC(cal) = WPC(ids)
	    CAL_NPTS(cal) = IM_LEN(im,1)
	    call malloc (CAL_DATA(cal), IM_LEN(im,1), TY_REAL)

	    # The sensitivity spectrum is in peculiar magnitudish units of
	    # 2.5 log10 [counts/sec/A / ergs/cm2/s/A].  This must be converted
	    # back to reasonable numbers and then divided into the rawish
	    # spectrum.  Note that the inverse is calculated to avoid divides.

	    data = CAL_DATA(cal)
	    call amovr (Memr[imgl1r(im)], Memr[data], IM_LEN(im,1))
	    do j = 1, IM_LEN(im,1) {
		Memr[data] = 10.0 ** (-0.4 * Memr[data])
		if (fnu) {
		    w = W0(ids) + (j-1) * WPC(ids)
		    Memr[data] = Memr[data] * w ** 2 / VLIGHT
		}
		data = data + 1
	    }

	    call imunmap (im)
	    call sfree (sp)
	}

	cal = Memi[beams+i]
end
