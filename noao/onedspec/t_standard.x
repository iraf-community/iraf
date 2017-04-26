include	<error.h>
include	<imhdr.h>
include	<imset.h>
include	<gset.h>
include	<mach.h>
include	<pkg/gtools.h>
include	<smw.h>

define	KEY	"noao$onedspec/standard.key"
define	PROMPT	"STANDARD options"

define	VLIGHT		2.997925e18	# Velocity of light in Angstroms/sec
define	EXT_LOOKUP	1		# Interp entry ID for extinction table
define	MAG_LOOKUP	2		# Interp entry ID for magnitude table

define	STD_LEN		13		# Length of standard structure
define	STD_AP		Memi[$1]	# Aperture number
define	STD_TYPE	Memi[$1+1]	# Spectrum type	
define	STD_SH		Memi[$1+2]	# Pointer to spectrum parameters
define	STD_IFLAG	Memi[$1+3]	# Interactive flag
define	STD_NWAVES	Memi[$1+4]	# Number of calibration points
define	STD_WAVES	Memi[$1+5]	# Pointer to standard star wavelengths
define	STD_DWAVES	Memi[$1+6]	# Pointer to standard star bandpasses
define	STD_MAGS	Memi[$1+7]	# Pointer to standard star magnitudes
define	STD_FLUXES	Memi[$1+8]	# Pointer to standard star fluxes
define	CAL_NWAVES	Memi[$1+9]	# Number of calibration points
define	CAL_WAVES	Memi[$1+10]	# Pointer to calibration wavelengths
define	CAL_DWAVES	Memi[$1+11]	# Pointer to calibration bandpasses
define	CAL_MAGS	Memi[$1+12]	# Pointer to calibration magnitudes

# Object flags
define	NONE	-1	# No object flag
define	SKY	0	# Sky
define	OBJ	1	# Object

# Interactive flags
define	ANSWERS	"|no|yes|N|Y|NO|YES|NO!|YES!|"
define	NO1	1	# No for a single spectrum
define	YES1	2	# Yes for a single spectrum
define	N2	3	# No for all spectra of the same aperture
define	Y2	4	# Yes for all spectra of the same aperture
define	NO2	5	# No for all spectra of the same aperture
define	YES2	6	# Yes for all spectra of the same aperture
define	NO3	7	# No for all spectra
define	YES3	8	# Yes for all spectra

# T_STANDARD -- Read standard star spectrum and compare with tabulated
#               fluxes for given star to ascertain the system sensitivity
#               across the spectrum. The user may optionally define
#               new and arbitrary bandpasses
#
#               The sensitivity function is stored in tabular form in a file
#               containing the wavelength, sensitivity factor, and counts per
#               bandpass at each required position along the spectrum.
#               The file is appended to for each new measurement from either
#               same or different standard stars.

procedure t_standard()

int	list			# List of input spectra
pointer	output			# Output standard file
pointer	observatory		# Observatory
pointer	aps			# Aperture list
real	bandwidth		# Width of bandpass
real	bandsep			# Separation of bandpass
bool	bswitch			# Beam switch?
bool	samestar		# Same star in all apertures?
int	interactive		# Interactive bandpass definition

bool	newobs, obshead
int	i, j, line, enwaves, nstds
real	wave, dwave, latitude
pointer	sp, units, errstr, str, image, ewaves, emags
pointer	im, mw, un, unang, sh, obj, sky, std, stds, obs, gp

int	imtgetim(), errget()
real	clgetr(), obsgetr()
bool	clgetb(), rng_elementi(), streq()
pointer	imtopenp(), rng_open(), immap(), smw_openim(), un_open()
errchk	immap, smw_openim, shdr_open, std_calib, get_airm, ext_load, obsimopen
errchk	un_open, std_gcalib

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (observatory, SZ_FNAME, TY_CHAR)
	call salloc (units, SZ_FNAME, TY_CHAR)
	call salloc (errstr, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Get task parameters.
	list = imtopenp ("input")
	call clgstr ("records", Memc[image], SZ_FNAME)
	call odr_openp (list, Memc[image])
	call clgstr ("output", Memc[output], SZ_FNAME)
	call clgstr ("apertures", Memc[image], SZ_FNAME)
	bandwidth = clgetr ("bandwidth")
	bandsep = clgetr ("bandsep")
	bswitch = clgetb ("beam_switch")
	if (bswitch)
	    samestar = true
	else
	    samestar = clgetb ("samestar")
	if (clgetb ("interact"))
	    interactive = YES1
	else
	    interactive = NO3

	# Expand the aperture list.
	iferr (aps = rng_open (Memc[image], INDEF, INDEF, INDEF))
	    call error (0, "Bad aperture list")

	call ext_load (ewaves, emags, enwaves)

	un = NULL
	sh = NULL
	obj = NULL
	sky = NULL
	obs = NULL
	gp = NULL
	nstds = 0
	while (imtgetim (list, Memc[image], SZ_FNAME) != EOF) {
	    iferr (im = immap (Memc[image], READ_ONLY, 0)) {
		call erract (EA_WARN)
		next
	    }
	    mw = smw_openim (im)
	    call shdr_open (im, mw, 1, 1, INDEFI, SHHDR, sh)

	    if (DC(sh) == DCNO) {
		call eprintf ("%s: No dispersion function\n")
		    call pargstr (Memc[image])
		call smw_close (MW(sh))
		call imunmap (IM(sh))
		next
	    }

	    # Work in units of first spectrum.
	    if (un == NULL) {
		call strcpy (UNITS(sh), Memc[units], SZ_FNAME)
		un = un_open (Memc[units])
		unang = un_open ("Angstroms")
		call un_ctranr (unang, un, Memr[ewaves], Memr[ewaves], enwaves)
	    }
	    if (IS_INDEF (IT(sh))) {
		call printf ("%s: ")
		    call pargstr (Memc[image])
		call flush (STDOUT)
		IT(sh) = clgetr ("exptime")
		call imunmap (IM(sh))
		ifnoerr (im = immap (Memc[image], READ_WRITE, 0)) {
		    IM(sh) = im
		    call imseti (IM(sh), IM_WHEADER, YES)
		    call imaddr (IM(sh), "exptime", IT(sh))
		} else {
		    im = immap (Memc[image], READ_ONLY, 0)
		    IM(sh) = im
		}
	    }

	    do line = 1, SMW_NSPEC(mw) {
		call shdr_open (im, mw, line, 1, INDEFI, SHDATA, sh)
		if (!rng_elementi (aps, AP(sh)))
		    next
		call shdr_units (sh, Memc[units])

		if (!bswitch || OFLAG(sh) == OBJ) {
		    call printf ("%s%s(%d): %s\n")
			call pargstr (IMNAME(sh))
			call pargstr (IMSEC(sh))
			call pargi (AP(sh))
			call pargstr (TITLE(sh))
		    call flush (STDOUT)
		}

	        if (IS_INDEF (AM(sh))) {
		    call clgstr ("observatory", Memc[observatory], SZ_FNAME)
		    call obsimopen (obs, im, Memc[observatory], NO, newobs,
			obshead)
		    if (newobs)
			call obslog (obs, "STANDARD", "latitude", STDOUT)
		    latitude = obsgetr (obs, "latitude")
		    iferr (call get_airm (RA(sh), DEC(sh), HA(sh), ST(sh),
			latitude, AM(sh))) {
			call printf ("%s: ")
			    call pargstr (Memc[image])
			call flush (STDOUT)
			AM(sh) = clgetr ("airmass")
			call imunmap (IM(sh))
			ifnoerr (im = immap (Memc[image], READ_WRITE, 0)) {
			    IM(sh) = im
			    call imseti (IM(sh), IM_WHEADER, YES)
			    call imaddr (IM(sh), "airmass", AM(sh))
			} else {
			    im = immap (Memc[image], READ_ONLY, 0)
			    IM(sh) = im
			}
		    }
		}

		for (i=0; i<nstds; i=i+1) {
		    std = Memi[stds+i]
		    if (STD_AP(std) == AP(sh))
			break
		}

		# Allocate space for this aperture if not already done.
		if (i >= nstds) {
		    if (nstds == 0)
			call malloc (stds, 10, TY_INT)
		    else if (mod (nstds, 10) == 0)
			call realloc (stds, nstds+10, TY_INT)
		    call salloc (std, STD_LEN, TY_STRUCT)
		    Memi[stds+i] = std
		    nstds = nstds + 1

		    STD_AP(std) = AP(sh)
		    STD_TYPE(std) = NONE
		    STD_SH(std) = NULL
		    STD_IFLAG(std) = interactive
		    STD_NWAVES(std) = 0

		    if (!samestar || i == 0) {
	 	        # Read calibration data
			Memc[str] = EOS
		        repeat {
		            iferr (call std_gcalib (std, un)) {
				j = errget (Memc[errstr], SZ_FNAME)
				if (streq (Memc[errstr], Memc[str]))
				    call erract (EA_ERROR)
				call strcpy (Memc[errstr], Memc[str], SZ_LINE)
		                call erract (EA_WARN)
			        next
			    }
		            break
			}
		    } else {
			CAL_NWAVES(std) = CAL_NWAVES(Memi[stds])
			CAL_WAVES(std) = CAL_WAVES(Memi[stds])
			CAL_DWAVES(std) = CAL_DWAVES(Memi[stds])
			CAL_MAGS(std) = CAL_MAGS(Memi[stds])
		    }

		    if (IS_INDEF (bandwidth)) {
		        do j = 1, CAL_NWAVES(std) {
		            wave = Memr[CAL_WAVES(std)+j-1]
		            dwave = Memr[CAL_DWAVES(std)+j-1]
		            call std_addband (std, wave, dwave, 0.)
		        }
		    } else {
			wave = W0(sh) + bandwidth / 2
			dwave = W0(sh) + (SN(sh)-1) * WP(sh) - bandwidth / 2
			while (wave <= dwave) {
		            call std_addband (std, wave, bandwidth, 0.)
			    wave = wave + bandsep
			}
		    }
		}

		# The copying of SHDR structures and associated MWCS only
		# occurs with beam switched data.

		if (bswitch) {
		    switch (STD_TYPE(std)) {
		    case NONE:
			STD_TYPE(std) = OFLAG(sh)
			call shdr_copy (sh, STD_SH(std), YES)
			next
		    case SKY:
			obj = sh
			sky = STD_SH(std)
			if (OFLAG(sh) == SKY) {
			    call eprintf ("%s[%d]: Object spectrum not found\n")
				call pargstr (IMNAME(sky))
				call pargi (AP(sky))

			    call smw_close (MW(sky))
			    call shdr_copy (sh, STD_SH(std), YES)
			    next
			}
		    case OBJ:
			obj = STD_SH(std)
			sky = sh
			if (OFLAG(sh) == OBJ) {
			    obj = STD_SH(std)
			    call eprintf ("%s[%d]: Sky spectrum not found\n")
				call pargstr (IMNAME(obj))
				call pargi (AP(obj))

			    call smw_close (MW(obj))
			    call shdr_copy (sh, STD_SH(std), YES)
			    next
			}
		    }
		} else {
		    obj = sh
		    sky = NULL
		}

		# Generate a calibration table
		call std_calib (obj, sky, std, gp, Memr[ewaves], Memr[emags],
		    enwaves)
		call std_output (obj, sky, std, Memc[output])

		if (interactive == YES1) {
		    if (STD_IFLAG(std) == NO3 || STD_IFLAG(std) == YES3) {
		        interactive = STD_IFLAG(std)
		        do i = 0, nstds-1
			    STD_IFLAG(Memi[stds+i]) = interactive
		    }
		    if (interactive == NO3 && gp != NULL)
			call gclose (gp)
		}

		if (bswitch) {
		    call smw_close (MW(STD_SH(std)))
		    STD_TYPE(std) = NONE
		}
	    }

	    call smw_close (MW(sh))
	    call imunmap (IM(sh))
	}

	if (un != NULL) {
	    call un_close (un)
	    call un_close (unang)
	}
	if (obs != NULL)
	    call obsclose (obs)
	if (gp != NULL)
	    call gclose (gp)
	do i = 0, nstds-1 {
	    std = Memi[stds+i]
	    obj = STD_SH(std)
	    switch (STD_TYPE(std)) {
	    case SKY:
		call eprintf ("%s[%d]: Object spectrum not found\n")
		    call pargstr (IMNAME(obj))
		    call pargi (AP(obj))
	    case OBJ:
		call eprintf ("%s[%d]: Sky spectrum not found\n")
		    call pargstr (IMNAME(obj))
		    call pargi (AP(obj))
	    }
	    if (obj != NULL)
		call shdr_close (obj)
	    if (!samestar || i == 0) {
		call mfree (CAL_WAVES(std), TY_REAL)
		call mfree (CAL_DWAVES(std), TY_REAL)
		call mfree (CAL_MAGS(std), TY_REAL)
	    }
	    call mfree (STD_WAVES(std), TY_REAL)
	    call mfree (STD_DWAVES(std), TY_REAL)
	    call mfree (STD_MAGS(std), TY_REAL)
	    call mfree (STD_FLUXES(std), TY_REAL)
	}
	call mfree (stds, TY_INT)
	call mfree (ewaves, TY_REAL)
	call mfree (emags, TY_REAL)
	call shdr_close (sh)
	call rng_close (aps)
	call imtclose (list)
	call sfree (sp)
end


# STD_CALIB -- Compute standard star calibrations

procedure std_calib (obj, sky, std, gp, ewaves, emags, enwaves)

pointer	obj			# Object pointer
pointer	sky			# Sky pointer
pointer	std			# Standard pointer
pointer	gp			# Graphics pointer
real	ewaves[enwaves]		# Extinction wavelengths
real	emags[enwaves]		# Extinction magnitudes
int	enwaves			# Extinction points

int	i, j, n, nwaves, wcs, key, newgraph
real	wave, dwave, flux, wx1, wx2, wy
pointer	sp, cmd, gt, waves, dwaves, fluxes, x, y

real	std_flux()
double	shdr_wl()
int	clgcur(), strdic(), clgwrd()
pointer	gopen(), gt_init()
errchk	gopen, std_output

define	beep_	99

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Sky subtract
	if (sky != NULL) {
	    call shdr_rebin (sky, obj)
	    call asubr (Memr[SY(obj)], Memr[SY(sky)], Memr[SY(obj)], SN(obj))
	}

	# Remove extinction correction
	if (EC(obj) == ECYES) {
	    x = SX(obj)
	    y = SY(obj)
	    n = SN(obj)
	    do i = 1, n {
	        call intrp (EXT_LOOKUP, ewaves, emags, enwaves,
		    Memr[x], flux, j)
	        Memr[y] = Memr[y] * 10.0 ** (-0.4 * flux * AM(obj))
		x = x + 1
		y = y + 1
	    }
	}

	nwaves = STD_NWAVES(std)
	waves = STD_WAVES(std)
	dwaves = STD_DWAVES(std)
	fluxes = STD_FLUXES(std)
	do i = 0, nwaves-1 {
	    wave = Memr[waves+i]
	    dwave = Memr[dwaves+i]
	    Memr[fluxes+i] = std_flux (obj, wave, dwave, ewaves, emags, enwaves)
	}

	# Plot spectrum if user wants to see whats happening
	if (STD_IFLAG(std) == NO1 || STD_IFLAG(std) == YES1) {
	    call printf ("%s[%d]: Edit bandpasses? ")
		call pargstr (IMNAME(obj))
		call pargi (AP(obj))
	    STD_IFLAG(std) = clgwrd ("answer", Memc[cmd], SZ_FNAME, ANSWERS)
	}

	i = STD_IFLAG(std)
	if (i==YES1||i==Y2||i==YES2||i==YES3) {
	    if (gp == NULL) {
	        call clgstr ("graphics", Memc[cmd], SZ_FNAME)
	        gp = gopen (Memc[cmd], NEW_FILE, STDGRAPH)
	    }
	    gt = gt_init()
	    call gt_sets (gt, GTTITLE, TITLE(obj))
	    call gt_sets (gt, GTPARAMS, IMNAME(obj))
	    call gt_sets (gt, GTXLABEL, LABEL(obj))
	    call gt_sets (gt, GTXUNITS, UNITS(obj))
	    call gt_sets (gt, GTYLABEL, "instrumental flux")
	    call gt_sets (gt, GTTYPE, "line")

	    key = 'r'
	    repeat {
	        switch (key) {
	        case '?':
		    call gpagefile (gp, KEY, PROMPT)
	        case ':':
		    if (Memc[cmd] == '/')
		        call gt_colon (Memc[cmd], gp, gt, newgraph)
		    else {
			switch (strdic (Memc[cmd],Memc[cmd],SZ_LINE,"|show|")) {
			case 1:
			    call mktemp ("std", Memc[cmd], SZ_LINE)
			    call std_output (obj, sky, std, Memc[cmd])
		            call gpagefile (gp, Memc[cmd], "standard star data")
			    call delete (Memc[cmd])
			default:
			    goto beep_
			}
		    }
	        case 'a':
		    call printf ("a again:\n")
		    i = clgcur ("cursor", wx2, wy, wcs, key, Memc[cmd], SZ_LINE)
		    call printf ("\n")
		    if (wx1 == wx2) {
		        call printf ("\07Two cursor positions required")
			goto beep_
		    }

		    # Create artificial standard wavelength and bandpass
		    wave = (wx1 + wx2) / 2.0
		    dwave = wx2 - wx1
	    	    flux = std_flux (obj, wave, dwave, ewaves, emags, enwaves)
		    call std_addband (std, wave, dwave, flux)
		    flux = flux / abs (shdr_wl (obj, double(wx1)) -
			shdr_wl (obj, double (wx2)))
		    call gmark (gp, wave, flux, GM_BOX, -dwave, 3.)

		    nwaves = STD_NWAVES(std)
		    waves = STD_WAVES(std)
		    dwaves = STD_DWAVES(std)
		    fluxes = STD_FLUXES(std)
		case 'd':
		    dwave = MAX_REAL
		    do i = 0, nwaves-1 {
			wave = Memr[waves+i]
			if (abs (wx1 - wave) < dwave) {
			    dwave = abs (wx1 - wave)
			    j = i
			}
		    }
		    wave = Memr[waves+j]
		    dwave = Memr[dwaves+j]
		    flux = Memr[fluxes+j]
		    flux = flux / abs (shdr_wl (obj, double(wave-dwave/2)) -
			shdr_wl (obj, double (wave+dwave/2)))
		    call gseti (gp, G_PMLTYPE, 0)
		    call gmark (gp, wave, flux, GM_BOX, -dwave, 3.)
		    call gseti (gp, G_PMLTYPE, 1)
		    call gscur (gp, wave, flux)
		    call std_delband (std, j)
		case 'q':
		    break
		case 'I':
		    call fatal (0, "Interrupt")
		case 'r':
		    newgraph = YES
	        case 'w':
		    call gt_window (gt, gp, "cursor", newgraph)
	        default: # Invalid keystroke
beep_	            call printf ("\007")
	        }

		if (newgraph == YES) {
		    call std_graph (obj, std, gp, gt, YES)
		    newgraph = NO
		}
	    } until (clgcur ("cursor",wx1,wy,wcs,key,Memc[cmd],SZ_LINE) == EOF)
	    call gt_free (gt)
	}

	call sfree (sp)
end


# STD_OUTPUT -- Output standard  star data.
# For now we do this in Angstroms.

procedure std_output (obj, sky, std, output)

pointer	obj			# Object pointer
pointer	sky			# Sky pointer
pointer	std			# Standard pointer
char	output[ARB]		# Output file name

int	i, fd, open()
real	wave, dwave, mag, flux, fnuzero, flambda, clgetr()
pointer	unang, un_open()
errchk	open, un_open, un_ctranr, std_units

begin
	fd = open (output, APPEND, TEXT_FILE)
	call fprintf (fd, "[%s]")
	    call pargstr (IMNAME(obj))
	if (sky != NULL) {
	    call fprintf (fd, "-[%s]")
	        call pargstr (IMNAME(sky))
	}

	unang = un_open ("Angstroms")
	call un_ctranr (UN(obj), unang, W0(obj), wave, 1)
	call un_ctranr (UN(obj), unang, W0(obj)+(SN(obj)-1)*WP(obj), dwave, 1)
	call fprintf (fd, " %d %d %.2f %5.3f %9.3f %9.3f %s\n")
	    call pargi (AP(obj))
	    call pargi (SN(obj))
	    call pargr (IT(obj))
	    call pargr (AM(obj))
	    #call pargr (W0(obj))
	    #call pargr (W0(obj) + (SN(obj)-1) * WP(obj))
	    call pargr (wave)
	    call pargr (dwave)
	    call pargstr (TITLE(obj))

	fnuzero = clgetr ("fnuzero")
	do i = 0, STD_NWAVES(std)-1 {
	    wave = Memr[STD_WAVES(std)+i]
	    dwave = Memr[STD_DWAVES(std)+i]
	    mag = Memr[STD_MAGS(std)+i]
	    flux = Memr[STD_FLUXES(std)+i]
	    if (flux == 0.)
		next

	    call std_units (UN(obj), unang, wave, dwave, 1)
	    flambda = fnuzero * 10. ** (-0.4 * mag) * VLIGHT / wave**2
	    call fprintf (fd, "%8.2f %12.5g %8.3f %12.5g\n")
		call pargr (wave)
		call pargr (flambda)
		call pargr (dwave)
		call pargr (flux)
	}
	call close (fd)

	call un_close (unang)
end


# STD_FLUX -- Add up the flux in a given bandpass centered on a given
# wavelength.  The bandpass must be entirely within the data.
# A correction for differential extinction across the bandpass is made
# by applying the extinction correction and then removing the correction
# at the bandpass center

real procedure std_flux (sh, wave, dwave, ewaves, emags, enwaves)

pointer	sh			# Spectrum
real	wave			# Bandpass wavelength
real	dwave			# Bandpass width
real	ewaves[enwaves]		# Extinction wavelengths
real	emags[enwaves]		# Extinction magnitudes
int	enwaves			# Extinction points

real	flux			# Bandpass flux

int	i, i1, i2, ierr
real	a, e, ec, x1, x2
double	w1, w2, w3, w4, shdr_lw(), shdr_wl()
pointer	x, y

begin
	# Determine bandpass limits in pixel and return if out of bounds.
	w1 = wave - dwave / 2.
	w2 = wave + dwave / 2.
	w3 = shdr_lw (sh, 0.5D0)
	w4 = shdr_lw (sh, double (SN(sh)+0.5))
	if (w1 < min (w3, w4) || w2 > max (w3, w4)) 
	    return (0.)

	a = shdr_wl (sh, w1)
	x2 = shdr_wl (sh, w2)
	x1 = min (a, x2)
	x2 = max (a, x2)
	i1 = nint (x1)
	i2 = nint (x2 - 0.00001)
	if (x1 == x2 || i1 < 1 || i2 > SN(sh))
	    return (0.)

	a = AM(sh)
	x = SX(sh) + i1 - 1
	y = SY(sh) + i1 - 1

	call intrp (EXT_LOOKUP, ewaves, emags, enwaves, wave, ec, ierr)
	call intrp (EXT_LOOKUP, ewaves, emags, enwaves, Memr[x], e, ierr)

	if (i1 == i2) {
	    flux = (x2-x1) * Memr[y] * 10.0 ** (0.4 * a * (e - ec))
	    return (flux)
	}

	flux = (i1+0.5-x1) * Memr[y] * 10.0 ** (0.4 * a * (e - ec))
	x = x + 1
	y = y + 1

	for (i=i1+1; i<=i2-1; i=i+1) {
	    call intrp (EXT_LOOKUP, ewaves, emags, enwaves, Memr[x], e, ierr)
	    flux = flux + Memr[y] * 10.0 ** (0.4 * a * (e - ec))
	    x = x + 1
	    y = y + 1
	}

	call intrp (EXT_LOOKUP, ewaves, emags, enwaves, Memr[x], e, ierr)
	flux = flux + (x2-i2+0.5) * Memr[y] * 10.0 ** (0.4 * a * (e - ec))

	return (flux)
end


# STD_ADDBAND -- Add a standard bandpass

procedure std_addband (std, wave, dwave, flux)

pointer	std		# Pointer to standard star data
real	wave		# Wavelength to be added
real	dwave		# Bandpass to be added
real	flux		# Flux to be added

int	i, nwaves
real	mag
pointer	waves, dwaves, mags, fluxes

begin
	nwaves = STD_NWAVES(std)
	if (nwaves == 0) {
	    call malloc (STD_WAVES(std), 10, TY_REAL)
	    call malloc (STD_DWAVES(std), 10, TY_REAL)
	    call malloc (STD_MAGS(std), 10, TY_REAL)
	    call malloc (STD_FLUXES(std), 10, TY_REAL)
	} else if (mod (nwaves, 10) == 0) {
	    call realloc (STD_WAVES(std), nwaves+10, TY_REAL)
	    call realloc (STD_DWAVES(std), nwaves+10, TY_REAL)
	    call realloc (STD_MAGS(std), nwaves+10, TY_REAL)
	    call realloc (STD_FLUXES(std), nwaves+10, TY_REAL)
	}

        call intrp (MAG_LOOKUP, Memr[CAL_WAVES(std)], Memr[CAL_MAGS(std)],
	    CAL_NWAVES(std), wave, mag, i)

	waves = STD_WAVES(std)
	dwaves = STD_DWAVES(std)
	mags = STD_MAGS(std)
	fluxes = STD_FLUXES(std)
	for (i=nwaves; (i>0)&&(Memr[waves+i-1]>wave); i=i-1) {
            Memr[waves+i] = Memr[waves+i-1]
            Memr[dwaves+i] = Memr[dwaves+i-1]
            Memr[mags+i] = Memr[mags+i-1]
            Memr[fluxes+i] = Memr[fluxes+i-1]
	}
        Memr[waves+i] = wave
        Memr[dwaves+i] = dwave
        Memr[mags+i] = mag
        Memr[fluxes+i] = flux
	STD_NWAVES(std) = nwaves + 1
end


# STD_DELBAND -- Delete a bandpass

procedure std_delband (std, band)

pointer	std		# Pointer to standard star data
int	band		# Band to be deleted

int	i, nwaves
pointer	waves, dwaves, mags, fluxes

begin
	nwaves = STD_NWAVES(std)
	waves = STD_WAVES(std)
	dwaves = STD_DWAVES(std)
	mags = STD_MAGS(std)
	fluxes = STD_FLUXES(std)
	for (i=band+1; i<nwaves; i=i+1) {
	    Memr[waves+i-1] = Memr[waves+i]
	    Memr[dwaves+i-1] = Memr[dwaves+i]
	    Memr[mags+i-1] = Memr[mags+i]
	    Memr[fluxes+i-1] = Memr[fluxes+i]
	}
	nwaves = nwaves - 1

	STD_NWAVES(std) = nwaves
	if (nwaves == 0) {
	    call mfree (STD_WAVES(std), TY_REAL)
	    call mfree (STD_DWAVES(std), TY_REAL)
	    call mfree (STD_MAGS(std), TY_REAL)
	    call mfree (STD_FLUXES(std), TY_REAL)
	}
end


# STD_GRAPH -- Graph the spectrum and standard star calibration points.

procedure std_graph (sh, std, gp, gt, clear)

pointer	sh			# Spectrum pointer
pointer	std			# Standard star data
pointer	gp			# GIO pointer
pointer	gt			# GTOOLS pointer
int	clear			# Clear flag

int	i
real	dw, wave, dwave, flux
double	shdr_wl()

begin
	if (clear == YES) {
	    call gclear (gp)
	    call gascale (gp, Memr[SX(sh)], SN(sh), 1)
	    call gascale (gp, Memr[SY(sh)], SN(sh), 2)
	    call gt_swind (gp, gt)
	    call gt_labax (gp, gt)
	}

	call gt_plot (gp, gt, Memr[SX(sh)], Memr[SY(sh)], SN(sh))

	do i = 0, STD_NWAVES(std)-1 {
	    wave = Memr[STD_WAVES(std)+i]
	    dwave = Memr[STD_DWAVES(std)+i]
	    flux = Memr[STD_FLUXES(std)+i]
	    if (flux == 0.)
		next
	    dw = abs (shdr_wl (sh, double(wave-dwave/2)) -
		shdr_wl (sh, double (wave+dwave/2)))
	    flux = flux / dw
	    call gmark (gp, wave, flux, GM_BOX, -dwave, 3.)
	}
end


# STD_GCALIB -- Get calibration data in desired units.

procedure std_gcalib (std, un)

pointer	std		#I Standard pointer
pointer	un		#I Desired units pointer

pointer	unang, un_open()
errchk	getcalib, std_units

begin
	call getcalib (CAL_WAVES(std), CAL_DWAVES(std), CAL_MAGS(std),
	    CAL_NWAVES(std))

	# Cnvert to desired units.
	unang = un_open ("Angstroms")
	call std_units (unang, un,
	    Memr[CAL_WAVES(std)], Memr[CAL_DWAVES(std)], CAL_NWAVES(std))
	call un_close (unang)
end


# STD_UNITS -- Convert bandpass information to different units.

procedure std_units (unin, unout, center, width, n)

pointer	unin		#I Input units
pointer	unout		#I Output units
real	center[ARB]	#U Bandpass centers
real	width[ARB]	#U Bandpass widths
int	n		#I Number of bandpasses

int	i
real	x1, x2
bool	un_compare()
errchk	un_ctranr


begin
	if (un_compare (unin, unout))
	    return

	do i = 1, n {
	    x1 = center[i] - width[i] / 2
	    x2 = center[i] + width[i] / 2
	    call un_ctranr (unin, unout, x1, x1, 1)
	    call un_ctranr (unin, unout, x2, x2, 1)
	    center[i] = (x1 + x2) / 2.
	    width[i] = abs (x1 - x2)
	}
end
