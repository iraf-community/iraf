include	<error.h>
include	<imset.h>
include	<imhdr.h>
include	<math/iminterp.h>
include	<smw.h>

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
pointer	ob		# Observatory
bool	ignoreaps	# Ignore aperture numbers?
bool	extinct		# Apply extinction correction?
bool	flux		# Apply flux calibration?
bool	fnu		# Calibration flux in FNU?

bool	doextinct, doflux, newobs, obshead
int	i, j, k, l, n, enwaves, nout, ncal
real	a, latitude, time, ext, fcor, ical, w, dw
pointer	sp, input, output, temp
pointer	obs, in, smw, sh, out, ewaves, emags, pcal, cal, asi, x, y, data

int	imtgetim(), imtlen()
bool	clgetb(), streq()
real	clgetr(), obsgetr(), asieval()
double	shdr_lw(), shdr_wl()
pointer	imtopenp(), immap(), smw_openim(), imgl3r(), impl3r()
errchk	immap, smw_openim, shdr_open, imgl3r, impl3r
errchk	obsimopen, get_airm, ext_load, cal_getflux, cal_extn, cal_flux

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (sens, SZ_FNAME, TY_CHAR)
	call salloc (ob, SZ_FNAME, TY_CHAR)
	call salloc (temp, SZ_LINE, TY_CHAR)

	# Get task parameters.
	inlist = imtopenp ("input")
	outlist = imtopenp ("output")
	call clgstr ("records", Memc[temp], SZ_LINE)
	call odr_openp (inlist, Memc[temp])
	call odr_openp (outlist, Memc[temp])
	call clgstr ("sensitivity", Memc[sens], SZ_FNAME)
	call clgstr ("observatory", Memc[ob], SZ_FNAME)
	extinct = clgetb ("extinct")
	flux = clgetb ("flux")
	fnu = clgetb ("fnu")
	ignoreaps = clgetb ("ignoreaps")

	if (!extinct && !flux)
	    call error (0, "No calibration correction specified")

	# Loop over all input images.
	sh = NULL
	obs = NULL
	enwaves = 0
	ncal = 0
	while (imtgetim (inlist, Memc[input], SZ_FNAME) != EOF) {

	    # Set output image.  Use a temporary image when output=input.
	    if (imtlen (outlist) > 0) {
		if (imtgetim (outlist, Memc[output], SZ_FNAME) == EOF)
		    break
	    } else
		call strcpy (Memc[input], Memc[output], SZ_FNAME)

	    # Map the input image.
	    iferr (in = immap (Memc[input], READ_ONLY, 0)) {
		call erract (EA_WARN)
		next
	    }
	    smw = smw_openim (in)

	    # Check the input image calibration status.
	    call shdr_open (in, smw, 1, 1, INDEFI, SHHDR, sh)
	    if (DC(sh) == DCNO) {
		call eprintf ("WARNING: [%s] has no dispersion function\n")
		    call pargstr (Memc[input])
		call smw_close (MW(sh))
		call imunmap (in)
		next
	    }
	    call shdr_units (sh, "angstroms")

	    doextinct = extinct && (EC(sh) == ECNO)
	    doflux = flux && (FC(sh) == FCNO)
	    if (!(doextinct || doflux)) {
		call eprintf ("WARNING: [%s] is already calibrated\n")
		    call pargstr (Memc[input])
		call smw_close  (MW(sh))
		call imunmap (in)
		next
	    }

	    # Map the output image.
	    if (streq (Memc[input], Memc[output]))
		call mktemp ("temp", Memc[temp], SZ_LINE)
	    else
		call strcpy (Memc[output], Memc[temp], SZ_LINE)
	    out = immap (Memc[temp], NEW_COPY, in)
	    if (IM_PIXTYPE(out) != TY_DOUBLE)
		IM_PIXTYPE(out) = TY_REAL

	    # Log the operation.
	    call printf ("%s: %s\n")
		call pargstr (Memc[output])
		call pargstr (IM_TITLE(out))
	    call flush (STDOUT)

	    # Initialize the extinction correction.
	    if (doextinct) {
		EC(sh) = ECYES

		# Load extinction function.
		if (enwaves == 0) {
		    call ext_load (ewaves, emags, enwaves)
		    call intrp0 (EXTN_LOOKUP)
		}

		# Determine airmass if needed.
		if (IS_INDEF(AM(sh))) {
		    call obsimopen (obs, in, Memc[ob], NO, newobs, obshead)
		    if (newobs)
			call obslog (obs, "CALIBRATE", "latitude", STDOUT)
		    latitude = obsgetr (obs, "latitude")
		    iferr (call get_airm (RA(sh), DEC(sh), HA(sh), ST(sh),
			latitude, AM(sh))) {
			call printf ("%s: ")
			    call pargstr (Memc[input])
			call flush (STDOUT)
			AM(sh) = clgetr ("airmass")
			call imunmap (in)
			ifnoerr (in = immap (Memc[input], READ_WRITE, 0)) {
			    IM(sh) = in
			    call imseti (IM(sh), IM_WHEADER, YES)
			    call imaddr (IM(sh), "airmass", AM(sh))
			} else {
			    in = immap (Memc[input], READ_ONLY, 0)
			    IM(sh) = in
			}
		    }
		}
		a = AM(sh)
	    } else
		ext = 1.

	    # Initialize the flux correction.
	    nout = 0
	    if (doflux) {
		FC(sh) = FCYES

		if (IS_INDEF (IT(sh)) || IT(sh) <= 0.) {
		    call printf ("%s: ")
			call pargstr (Memc[input])
		    call flush (STDOUT)
		    IT(sh) = clgetr ("exptime")
		    call imunmap (in)
		    ifnoerr (in = immap (Memc[input], READ_WRITE, 0)) {
			IM(sh) = in
			call imseti (IM(sh), IM_WHEADER, YES)
			call imaddr (IM(sh), "exptime", IT(sh))
			call imaddr (out, "exptime", IT(sh))
		    } else {
		        in = immap (Memc[input], READ_ONLY, 0)
			IM(sh) = in
		    }
		}
		time = IT(sh)
	    } else
		fcor = 1.

	    # Calibrate.
	    do j = 1, IM_LEN(in,3) {
		do i = 1, IM_LEN(in,2) {
		    data = impl3r (out, i, j)
		    switch (SMW_FORMAT(smw)) {
		    case SMW_ND:
			if (doflux) {
			    call cal_getflux (Memc[sens], INDEFI, fnu,
				pcal, ncal, cal)

			    asi = IM(cal)
			    n = SN(cal)
			}
			y = imgl3r (in, i, j)
			switch (SMW_LAXIS(smw,1)) {
			case 1:
			    do k = 1, IM_LEN(out,1) {
				w = shdr_lw (sh, double(k))
				if (doextinct) {
				    call intrp (EXTN_LOOKUP, Memr[ewaves],
					Memr[emags], enwaves, w, ext, l)
				    ext = 10.0 ** (0.4 * a * ext)
				}
				if (doflux) {
				    ical = shdr_wl (cal, double(w))
				    if (ical < 1. || ical > n) {
					if (ical < 0.5 || ical > n + 0.5)
					    nout = nout + 1
					ical = max (1., min (real(n), ical))
				    }
				    dw = abs (shdr_lw (sh, double(k+0.5)) -
					shdr_lw (sh, double(k-0.5)))
				    fcor = asieval (asi, ical) / dw / time
				}
				Memr[data] = Memr[y] * ext * fcor
				y = y + 1
				data = data + 1
			    }
			case 2, 3:
			    if (SMW_LAXIS(smw,1) == 2)
				k = i
			    else
				k = j
			    w = shdr_lw (sh, double(k))
			    if (doextinct) {
				call intrp (EXTN_LOOKUP, Memr[ewaves],
				    Memr[emags], enwaves, w, ext, l)
				ext = 10.0 ** (0.4 * a * ext)
			    }
			    if (doflux) {
				ical = shdr_wl (cal, double(w))
				if (ical < 1. || ical > n) {
				    if (ical < 0.5 || ical > n + 0.5)
					nout = nout + 1
				    ical = max (1., min (real(n), ical))
				}
				dw = abs (shdr_lw (sh, double(k+0.5)) -
				    shdr_lw (sh, double(k-0.5)))
				fcor = asieval (asi, ical) / dw / time
			    }
			    call amulkr (Memr[y], ext * fcor, Memr[data],
				IM_LEN(out,1))
			}
		    case SMW_ES, SMW_MS:
			call shdr_open (in, smw, i, j, INDEFI, SHDATA, sh)
			call shdr_units (sh, "angstroms")
			if (doflux) {
			    if (ignoreaps)
				call cal_getflux (Memc[sens], INDEFI, fnu,
				    pcal, ncal, cal)
			    else
				call cal_getflux (Memc[sens], AP(sh), fnu,
				    pcal, ncal, cal)

			    asi = IM(cal)
			    n = SN(cal)
			}
			x = SX(sh)
			y = SY(sh)
			do k = 1, SN(sh) {
			    w = Memr[x]
			    if (doextinct) {
				call intrp (EXTN_LOOKUP, Memr[ewaves],
				    Memr[emags], enwaves, w, ext, l)
				ext = 10.0 ** (0.4 * a * ext)
			    }
			    if (doflux) {
				ical = shdr_wl (cal, double(w))
				if (ical < 1. || ical > n) {
				    if (ical < 0.5 || ical > n + 0.5)
					nout = nout + 1
				    ical = max (1., min (real(n), ical))
				}
				dw = abs (shdr_lw (sh, double(k+0.5)) -
				    shdr_lw (sh, double(k-0.5)))
				fcor = asieval (asi, ical) / dw / time
			    }
			    Memr[data] = Memr[y] * ext * fcor
			    x = x + 1
			    y = y + 1
			    data = data + 1
			}
			do k = SN(sh)+1, IM_LEN(out,1) {
			    Memr[data] = 0
			    data = data + 1
			}
		    }
		}
	    }

	    # Log the results.
	    if (doflux && (IS_INDEF (IT(sh)) || IT(sh) <= 0.)) {
		call printf (
		    "  WARNING: No exposure time found.  Using a time of %g.\n")
		    call pargr (time)
	    }
	    if (nout > 0) {
		call printf (
		    "  WARNING: %d pixels outside of flux calibration limits\n")
		    call pargi (nout)
	    }
	    if (doextinct)
		call printf ("  Extinction correction applied\n")
	    if (doflux)
		call printf ("  Flux calibration applied\n")
	    call flush (STDOUT)

	    call imaddr (out, "AIRMASS", AM(sh))
	    call imaddi (out, "EX-FLAG", EC(sh))
	    call imaddi (out, "CA-FLAG", FC(sh))
	    if (doflux) {
		if (fnu)
		    call imastr (out, "BUNIT", "erg/cm2/s/Hz")
		else
		    call imastr (out, "BUNIT", "erg/cm2/s/A")
	    }

	    # Close the input and output images.
	    call smw_close (MW(sh))
	    call imunmap (in)
	    call imunmap (out)
	    if (streq (Memc[input], Memc[output])) {
		call imdelete (Memc[input])
		call imrename (Memc[temp], Memc[output])
	    }
	}

	# Finish up.
	if (enwaves > 0) {
	    call mfree (ewaves, TY_REAL)
	    call mfree (emags, TY_REAL)
	}
	if (ncal > 0) {
	    do i = 0, ncal-1 {
		cal = Memi[pcal+i]
		call asifree (IM(cal))
		call smw_close (MW(cal))
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

int	i, j, n, clgwrd()
pointer	sp, fname, im, smw, x, y, immap(), smw_openim()
errchk	immap, smw_openim, shdr_open, asifit

begin
	# Check for previously saved calibration
	for (i=0; i<ncal; i=i+1) {
	    cal = Memi[pcal+i]
	    if (AP(cal) == ap)
		return
	}

	# Allocate space for a new data pointer, get the calibration data,
	# and convert to calibration array.

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
	smw = smw_openim (im)
	cal = NULL
	call shdr_open (im, smw, 1, 1, ap, SHDATA, cal)
	call shdr_units (cal, "angstroms")
	AP(cal) = ap
	Memi[pcal+ncal] = cal
	ncal = ncal + 1
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

	call asiinit (im, clgwrd ("interp", Memc[fname], SZ_FNAME,II_FUNCTIONS))
	call asifit (im, Memr[SY(cal)], n)
	IM(cal) = im

	call mfree (SX(cal), TY_REAL)
	call mfree (SY(cal), TY_REAL)

	call sfree (sp)
end
