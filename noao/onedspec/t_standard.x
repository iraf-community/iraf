include	<error.h>
include	<imhdr.h>
include	<gset.h>
include	<mach.h>
include	<pkg/gtools.h>
include "idsmtn.h"

define	KEY	"noao$lib/scr/standard.key"
define	PROMPT	"STANDARD options"

define	VLIGHT		2.997925e18	# Velocity of light in Angstroms/sec
define	EXT_LOOKUP	1		# Interp entry ID for extinction table
define	MAG_LOOKUP	2		# Interp entry ID for magnitude table
define	NRANGES		100		# Maximum number of aperture ranges

define	STD_LEN		19		# Length of standard structure
define	STD_BEAM	Memi[$1]	# Beam number
define	STD_OBJ		Memi[$1+1]	# Pointer to image name
define	STD_SKY		Memi[$1+2]	# Pointer to sky name
define	STD_IDS		Memi[$1+3]	# Pointer to header parameters
define	STD_SPEC	Memi[$1+4]	# Pointer to spectrum data
define	STD_BSFLAG	Memi[$1+5]	# Beam switch flag
define	STD_IFLAG	Memi[$1+6]	# Interactive flag
define	STD_NWAVES	Memi[$1+7]	# Number of calibration points
define	STD_WAVES	Memi[$1+8]	# Pointer to standard star wavelengths
define	STD_DWAVES	Memi[$1+9]	# Pointer to standard star bandpasses
define	STD_MAGS	Memi[$1+10]	# Pointer to standard star magnitudes
define	STD_FLUXES	Memi[$1+11]	# Pointer to standard star fluxes
define	CAL_NWAVES	Memi[$1+12]	# Number of calibration points
define	CAL_WAVES	Memi[$1+13]	# Pointer to calibration wavelengths
define	CAL_DWAVES	Memi[$1+14]	# Pointer to calibration bandpasses
define	CAL_MAGS	Memi[$1+15]	# Pointer to calibration magnitudes
define	EXT_NWAVES	Memi[$1+16]	# Number of extinction points
define	EXT_WAVES	Memi[$1+17]	# Extinction wavelength points
define	EXT_EXTNS	Memi[$1+18]	# Pointer to extinctions points

# Interactive flags
define	ANSWERS	"|no|yes|NO|YES|NO!|YES!|"
define	NO1	1	# No for a single spectrum
define	YES1	2	# Yes for a single spectrum
define	NO2	3	# No for all spectra of the same aperture
define	YES2	4	# Yes for all spectra of the same aperture
define	NO3	5	# No for all spectra
define	YES3	6	# Yes for all spectra

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

int	odr			# List of input spectra
pointer	output			# Output standard file
pointer	aps			# Aperture list
real	bandwidth		# Width of bandpass
real	bandsep			# Separation of bandpass
real	latitude		# Observation latitude
int	interactive		# Interactive bandpass definition
bool	samestar		# Same star in all apertures?

int	i, line, beam, nbeams
real	wave, dwave
pointer	sp, image, hdr, gp, im, data, stds, std

int	odr_getim(), decode_ranges()
real	clgetr()
bool	clgetb(), is_in_range()
pointer	immap(), imgl1r(), imgl2r()
errchk	gen_calib, get_airm, ext_load()

begin
	# If beam switching call a separate procedure.
	if (clgetb ("beam_switch")) {
	    call bs_standard ()
	    return
	}

	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (hdr, LEN_IDS, TY_STRUCT)
	call salloc (POINT(hdr), MAX_NCOEFF, TY_REAL)
	call salloc (aps, 3*NRANGES, TY_INT)

	# Get task parameters.
	if (clgetb ("recformat"))
	    call odr_open1 ("input", "records", odr)
	else
	    call odr_open1 ("input", "", odr)
	call clgstr ("output", Memc[output], SZ_FNAME)
	call clgstr ("apertures", Memc[image], SZ_FNAME)
	bandwidth = clgetr ("bandwidth")
	bandsep = clgetr ("bandsep")
	latitude = clgetr ("latitude")
	samestar = clgetb ("samestar")
	if (clgetb ("interact"))
	    interactive = YES1
	else
	    interactive = NO3

	# Expand the aperture list.
	if (decode_ranges (Memc[image], Memi[aps], NRANGES, i) == ERR)
	    call error (0, "Bad aperture list")

	gp = NULL
	nbeams = 0
	while (odr_getim (odr, Memc[image], SZ_FNAME) != EOF) {
	    iferr (im = immap (Memc[image], READ_ONLY, 0)) {
		call erract (EA_WARN)
		next
	    }

	    call load_ids_hdr (hdr, im, 1)
	    if (DC_FLAG(hdr) == -1) {
		call eprintf ("%s: Not dispersion corrected\n")
		    call pargstr (Memc[image])
		call imunmap (im)
		next
	    }
	    if (IS_INDEF (ITM(hdr))) {
		call eprintf ("%s: Warning - exposure time missing\n")
		    call pargstr (Memc[image])
	    }


	    do line = 1, IM_LEN(im,2) {
		if (line > 1)
	            call load_ids_hdr (hdr, im, line)
		if (!is_in_range (Memi[aps], BEAM(hdr)))
		    next

		call printf ("[%s][%d]: %s\n")
		    call pargstr (Memc[image])
		    call pargi (BEAM(hdr))
		    call pargstr (IM_TITLE(im))
		call flush (STDOUT)

	        if (IS_INDEF (AIRMASS(hdr)))
		    call get_airm (RA(hdr), DEC(hdr), HA(hdr), ST(hdr),
			latitude, AIRMASS(hdr))

	        # Access pixels
	        if (IM_NDIM(im) == 1)
		    data = imgl1r (im, 1)
	        else
		    data = imgl2r (im, line)

		for (beam=0; beam<nbeams; beam=beam+1) {
		    std = Memi[stds+beam]
		    if (STD_BEAM(std) == BEAM(hdr))
			break
		}

		# Allocate space for this beam if not already done.
		if (beam >= nbeams) {
		    if (nbeams == 0)
			call malloc (stds, 10, TY_INT)
		    else if (mod (nbeams, 10) == 0)
			call realloc (stds, nbeams+10, TY_INT)

		    call salloc (std, STD_LEN, TY_STRUCT)
		    Memi[stds+beam] = std
		    nbeams = nbeams + 1

		    STD_BEAM(std) = BEAM(hdr)
		    STD_IFLAG(std) = interactive
		    STD_NWAVES(std) = 0

		    if (!samestar || beam == 0) {
	 	        # Read calibration data
		        repeat {
		            iferr (call getcalib (CAL_WAVES(std),
				CAL_DWAVES(std), CAL_MAGS(std),
				CAL_NWAVES(std))) {
		                call erract (EA_WARN)
			        next
			    }
			    call ext_load (EXT_WAVES(std), EXT_EXTNS(std),
				EXT_NWAVES(std))
		            break
			}
		    } else {
			CAL_NWAVES(std) = CAL_NWAVES(Memi[stds])
			CAL_WAVES(std) = CAL_WAVES(Memi[stds])
			CAL_DWAVES(std) = CAL_DWAVES(Memi[stds])
			CAL_MAGS(std) = CAL_MAGS(Memi[stds])
			EXT_NWAVES(std) = EXT_NWAVES(Memi[stds])
			EXT_WAVES(std) = EXT_WAVES(Memi[stds])
			EXT_EXTNS(std) = EXT_EXTNS(Memi[stds])
		    }

		    if (IS_INDEF (bandwidth)) {
		        do i = 1, CAL_NWAVES(std) {
		            wave = Memr[CAL_WAVES(std)+i-1]
		            dwave = Memr[CAL_DWAVES(std)+i-1]
		            call add_band (std, wave, dwave, 0.)
		        }
		    } else {
			wave = W0(hdr) + bandwidth / 2
			dwave = W0(hdr) + (NP2(hdr) - 1) * WPC(hdr) -
			    bandwidth / 2
			while (wave <= dwave) {
		            call add_band (std, wave, bandwidth, 0.)
			    wave = wave + bandsep
			}
		    }
		}
		std = Memi[stds+beam]

		# Generate a calibration table
		call gen_calib (gp, im, hdr, Memc[image], "", Memc[output],
		    std, Memr[data])

		if (interactive == YES1) {
		    if (STD_IFLAG(std) == NO3 || STD_IFLAG(std) == YES3) {
		        interactive = STD_IFLAG(std)
		        do beam = 0, nbeams-1
			    STD_IFLAG(Memi[stds+beam]) = interactive
		    }
		    if (interactive == NO3 && gp != NULL)
			call gclose (gp)
		}
	    }
	    call imunmap (im)
	}

	if (gp != NULL)
	    call gclose (gp)
	call odr_close (odr)
	do beam = 0, nbeams-1 {
	    std = Memi[stds+beam]
	    if (!samestar || beam == 0) {
		call mfree (CAL_WAVES(std), TY_REAL)
		call mfree (CAL_DWAVES(std), TY_REAL)
		call mfree (CAL_MAGS(std), TY_REAL)
		call mfree (EXT_WAVES(std), TY_REAL)
		call mfree (EXT_EXTNS(std), TY_REAL)
	    }
	    call mfree (STD_WAVES(std), TY_REAL)
	    call mfree (STD_DWAVES(std), TY_REAL)
	    call mfree (STD_MAGS(std), TY_REAL)
	    call mfree (STD_FLUXES(std), TY_REAL)
	}
	call mfree (stds, TY_INT)
	call sfree (sp)
end


# BS_STANDARD -- Special version for beam-switched data.

procedure bs_standard()

int	odr			# List of input spectra
pointer	output			# Output standard file
pointer	aps			# Aperture list
real	bandwidth		# Width of bandpass
real	bandsep			# Separation of bandpass
real	latitude		# Observation latitude
int	interactive		# Interactive bandpass definition

int	i, line, ifile, nbeams, beam, flag
real	wave, dwave
pointer	sp, image, ids, gp, im, data, bands, stds, std
pointer	obj, sky, hdr, spec

pointer	immap(), imgl1r(), imgl2r()
int	odr_getim(), decode_ranges()
real	clgetr()
bool	clgetb(), is_in_range()
errchk	gen_calib, get_airm, ext_load()

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (hdr, LEN_IDS, TY_STRUCT)
	call salloc (POINT(hdr), MAX_NCOEFF, TY_REAL)
	call salloc (aps, 3*NRANGES, TY_INT)
	call salloc (bands, STD_LEN, TY_STRUCT)
	STD_NWAVES(bands) = 0
	CAL_NWAVES(bands) = 0

	# Get task parameters.
	if (clgetb ("recformat"))
	    call odr_open1 ("input", "records", odr)
	else
	    call odr_open1 ("input", "", odr)
	call clgstr ("output", Memc[output], SZ_FNAME)
	call clgstr ("apertures", Memc[image], SZ_FNAME)
	bandwidth = clgetr ("bandwidth")
	bandsep = clgetr ("bandsep")
	latitude = clgetr ("latitude")
	if (clgetb ("interact"))
	    interactive = YES1
	else
	    interactive = NO3

	# Expand the aperture list.
	if (decode_ranges (Memc[image], Memi[aps], NRANGES, i) == ERR)
	    call error (0, "Bad aperture list")

	gp = NULL
	ifile = 0
	nbeams = 0
	while (odr_getim (odr, Memc[image], SZ_FNAME) != EOF){
	    iferr (im = immap (Memc[image], READ_ONLY, 0)) {
		call erract (EA_WARN)
		next
	    }

	    call load_ids_hdr (hdr, im, 1)
	    if (DC_FLAG(hdr) == -1) {
		call eprintf ("%s: Not dispersion corrected\n")
		    call pargstr (Memc[image])
		call imunmap (im)
		next
	    }
	    if (IS_INDEF (ITM(hdr))) {
		call eprintf ("%s: Warning - exposure time missing\n")
		    call pargstr (Memc[image])
	    }

	    # Read in structure elements
	    do line = 1, IM_LEN(im,2) {
		if (line != 1)
	            call load_ids_hdr (hdr, im, line)
		if (!is_in_range (Memi[aps], BEAM(hdr)))
		    next

		if (OFLAG(hdr) == 1) {
		    call printf ("[%s][%d]: %s\n")
		        call pargstr (Memc[image])
		        call pargi (BEAM(hdr))
		        call pargstr (IM_TITLE(im))
		    call flush (STDOUT)
		}

	        if (IS_INDEF (AIRMASS(hdr)))
		    call get_airm (RA(hdr), DEC(hdr), HA(hdr), ST(hdr),
			latitude, AIRMASS(hdr))

	        # Access pixels
	        if (IM_NDIM(im) == 1)
		    data = imgl1r (im, 1)
	        else
		    data = imgl2r (im, line)

	        # Beam-switch (i.e. accumulate object-sky sums)
		for (beam=0; beam<nbeams; beam=beam+1) {
		    std = Memi[stds+beam]
		    if (STD_BEAM(std) == BEAM(hdr))
			break
		}

		# Allocate space for this beam if not already done
		if (beam >= nbeams) {
		    if (nbeams == 0)
			call malloc (stds, 10, TY_INT)
		    else if (mod (nbeams, 10) == 0)
			call realloc (stds, nbeams+10, TY_INT)

		    call salloc (std, STD_LEN, TY_STRUCT)
		    call salloc (obj, SZ_FNAME, TY_CHAR)
		    call salloc (sky, SZ_FNAME, TY_CHAR)
		    call salloc (ids, LEN_IDS, TY_STRUCT)
		    call salloc (spec, IM_LEN(im,1), TY_REAL)

		    STD_BEAM(std) = BEAM(hdr)
		    STD_OBJ(std) = obj
		    STD_SKY(std) = sky
		    STD_IDS(std) = ids
		    STD_SPEC(std) = spec
		    STD_BSFLAG(std) = INDEFI

		    Memi[stds+beam] = std
		    nbeams = nbeams + 1

		    if (CAL_NWAVES(bands) == 0) {
		 	STD_IFLAG(bands) = interactive

	 	        # Read calibration data
		        repeat {
		            iferr (call getcalib (CAL_WAVES(bands),
			        CAL_DWAVES(bands), CAL_MAGS(bands),
				CAL_NWAVES(bands))) {
		                call erract (EA_WARN)
			        next
		            }
			    call ext_load (EXT_WAVES(bands), EXT_EXTNS(bands),
				EXT_NWAVES(bands))
		            break
		        }

		        if (IS_INDEF (bandwidth)) {
		            do i = 1, CAL_NWAVES(bands) {
		                wave = Memr[CAL_WAVES(bands)+i-1]
		                dwave = Memr[CAL_DWAVES(bands)+i-1]
		                call add_band (bands, wave, dwave, 0.)
		            }
		        } else {
			    wave = W0(hdr) + bandwidth / 2
			    dwave = W0(hdr) + (NP2(hdr) - 1) * WPC(hdr) -
			        bandwidth / 2
			    while (wave <= dwave) {
		                call add_band (bands, wave, bandwidth, 0.)
			        wave = wave + bandsep
			    }
			}
		    }
		}

		obj = STD_OBJ(std)
		sky = STD_SKY(std)
		ids = STD_IDS(std)
		spec = STD_SPEC(std)
		flag = STD_BSFLAG(std)
		
		# Save header elements and image name
		if (OFLAG(hdr) == 1) {
		    BEAM(ids) = BEAM(hdr)
		    EX_FLAG(ids) = EX_FLAG(hdr)
		    W0(ids) = W0(hdr)
		    WPC(ids) = WPC(hdr)
		    NP2(ids) = NP2(hdr)
		    ITM(ids) = ITM(hdr)
		    AIRMASS(ids) = AIRMASS(hdr)
		    call strcpy (Memc[image], Memc[obj], SZ_FNAME)
		} else
		    call strcpy (Memc[image], Memc[sky], SZ_FNAME)

		# Accumulate object or sky
		call add_spec (Memr[data], Memr[spec], flag, OFLAG(hdr),
		    NP2(hdr))
		STD_BSFLAG(std) = flag

		# Add to calibration table.
	        if (flag == 0) {
		    call gen_calib (gp, im, ids, Memc[obj], Memc[sky],
			Memc[output], bands, Memr[spec])

		    if (STD_IFLAG(bands)==NO2 || STD_IFLAG(bands)==NO3)
			if (gp != NULL)
			    call gclose (gp)
		}

	        # Check for unfulfilled sums
	        ifile = ifile+1
	        if (mod (ifile, nbeams*2) == 0) {
		    do beam = 0, nbeams-1 {
			std = Memi[stds+beam]
		        if (STD_BSFLAG(std) != 0) {
			    call eprintf (
				"Warning: number of skys and objects ")
			    call eprintf ("don't match for aperture: %2d\n")
			        call pargi (STD_BEAM(std))
		        }
		    }
		}
	    }
	    call imunmap (im)
	}

	if (gp != NULL)
	    call gclose (gp)

	# Final check for unfulfilled sums
	do beam = 0, nbeams-1 {
	    std = Memi[stds+beam]
	    if (STD_BSFLAG(std) != 0) {
		call eprintf ("Warning: number of skys and objects ")
		call eprintf ("don't match for aperture: %2d\n")
		    call pargi (STD_BEAM(std))
	    }
	}

	call odr_close (odr)
	call mfree (CAL_WAVES(bands), TY_REAL)
	call mfree (CAL_DWAVES(bands), TY_REAL)
	call mfree (CAL_MAGS(bands), TY_REAL)
	call mfree (EXT_WAVES(bands), TY_REAL)
	call mfree (EXT_EXTNS(bands), TY_REAL)
	call mfree (STD_WAVES(bands), TY_REAL)
	call mfree (STD_DWAVES(bands), TY_REAL)
	call mfree (STD_MAGS(bands), TY_REAL)
	call mfree (STD_FLUXES(bands), TY_REAL)
	call mfree (stds, TY_INT)
	call sfree (sp)
end


# GEN_CALIB -- Read spectrum and header values.
#              Correct for extinction and generate a table of
#              calibration values

procedure gen_calib (gp, im, ids, image, sky, output, std, spec)

pointer	gp				# GIO pointer
pointer	im				# Image pointer
pointer	ids				# Header pointer
char	image[ARB]			# Image name
char	sky[ARB]			# Sky name
char	output[ARB]			# Output standard file name
pointer	std				# Standard star data
real	spec[ARB]			# Spectrum data

int	i, j, npts, nwaves, wcs, key, newgraph
real	wave, dwave, flux, wx1, wx2, wy
pointer	sp, cmd, gt, waves, dwaves, fluxes

int	clgcur(), strdic(), clgwrd()
pointer	gopen(), gt_init()
errchk	gopen, std_output

define	beep_	99

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# De-extinction correct if needed.
	npts = NP2(ids)
	if (EX_FLAG(ids) == 0) {
	    do i = 1, npts {
		wave = W0(ids) + (i-1) * WPC(ids)
	        call intrp (EXT_LOOKUP, Memr[EXT_WAVES(std)],
		    Memr[EXT_EXTNS(std)], EXT_NWAVES(std), wave, flux, j)
	        spec[i] = spec[i] * 10.0 ** (-0.4 * flux * AIRMASS(ids))

	    }
	}

	nwaves = STD_NWAVES(std)
	waves = STD_WAVES(std)
	dwaves = STD_DWAVES(std)
	fluxes = STD_FLUXES(std)
	do i = 0, nwaves-1 {
	    wave = Memr[waves+i]
	    dwave = Memr[dwaves+i]
	    call add_flux (wave, dwave, std, W0(ids), WPC(ids), AIRMASS(ids),
		spec, npts, Memr[fluxes+i])
	}


	# Plot spectrum if user wants to see whats happening
	if (STD_IFLAG(std) == NO1 || STD_IFLAG(std) == YES1) {
	    call printf ("[%s][%d]: Edit bandpasses? ")
		call pargstr (image)
		call pargi (BEAM(ids))
	    STD_IFLAG(std) = clgwrd ("answer", Memc[cmd], SZ_FNAME, ANSWERS)
	}

	if (STD_IFLAG(std)==YES1||STD_IFLAG(std)==YES2||STD_IFLAG(std)==YES3) {
	    if (gp == NULL) {
	        call clgstr ("graphics", Memc[cmd], SZ_FNAME)
	        gp = gopen (Memc[cmd], NEW_FILE, STDGRAPH)
	    }
	    gt = gt_init()
	    call gt_sets (gt, GTTITLE, IM_TITLE(im))
	    call gt_sets (gt, GTPARAMS, image)
	    call gt_sets (gt, GTXLABEL, "wavelength")
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
			    call std_output (Memc[cmd], image, sky, im, ids,
				std)
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
	    	    call add_flux (wave, dwave, std, W0(ids), WPC(ids),
			AIRMASS(ids), spec, npts, flux)
		    call add_band (std, wave, dwave, flux)
		    flux = flux * WPC(ids) / dwave
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
		    flux = Memr[fluxes+j] * WPC(ids) / dwave
		    call gseti (gp, G_PMLTYPE, 0)
		    call gmark (gp, wave, flux, GM_BOX, -dwave, 3.)
		    call gseti (gp, G_PMLTYPE, 1)
		    call gscur (gp, wave, flux)
		    call del_band (std, j)
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
		    call std_graph (gp, gt, YES, spec, npts, W0(ids),
			WPC(ids), std)
		    newgraph = NO
		}
	    } until (clgcur ("cursor",wx1,wy,wcs,key,Memc[cmd],SZ_LINE) == EOF)
	    call gt_free (gt)
	}

	call std_output (output, image, sky, im, ids, std)
	call sfree (sp)
end


procedure std_output (output, image, sky, im, ids, std)

char	output[ARB]		# Output file name
char	image[ARB]		# Spectrum image name
char	sky[ARB]		# Sky image name
pointer	im			# IMIO pointer
pointer	ids			# Header parameters
pointer	std			# Standard star data

int	i, fd, open()
real	wave, dwave, mag, flux, fnuzero, flambda, clgetr()
errchk	open()

begin
	# Open output file and write standard star data.
	fd = open (output, APPEND, TEXT_FILE)
	call fprintf (fd, "[%s]")
	    call pargstr (image)
	if (sky[1] != EOS) {
	    call fprintf (fd, "-[%s]")
	        call pargstr (sky)
	}
	call fprintf (fd, " %d %d %.2f %5.3f %9.3f %9.3f %s\n")
	    call pargi (BEAM(ids))
	    call pargi (NP2(ids))
	    call pargr (ITM(ids))
	    call pargr (AIRMASS(ids))
	    call pargr (W0(ids))
	    call pargr (W0(ids) + (NP2(ids)-1) * WPC(ids))
	    call pargstr (IM_TITLE(im))

	do i = 0, STD_NWAVES(std)-1 {
	    wave = Memr[STD_WAVES(std)+i]
	    dwave = Memr[STD_DWAVES(std)+i]
	    mag = Memr[STD_MAGS(std)+i]
	    flux = Memr[STD_FLUXES(std)+i]
	    if (flux == 0.)
		next

	    fnuzero = clgetr ("fnuzero")
	    flambda = fnuzero * 10. ** (-0.4 * mag) * VLIGHT / wave**2
	    call fprintf (fd, "%8.2f %12.5g %8.3f %12.5g\n")
		call pargr (wave)
		call pargr (flambda)
		call pargr (dwave)
		call pargr (flux)
	}
	call close (fd)
end

# ADD_FLUX -- Add up the flux in a given bandpass centered on a given
# wavelength, correcting for extinction.

procedure add_flux (wave, dwave, std, w0, wpc, airmass, spec, npts, flux)

real	wave			# Wavelength at center of bandpass
real	dwave			# Bandpass width
pointer	std			# Calibration star data
real	w0, wpc			# Wavelength coordinates
real	airmass			# Observation airmass
real	spec[npts]		# Spectrum data
int	npts			# Number of points
real	flux			# Returned bandpass flux

int	i1, i2, j, ierr
real	wstart, w, x1, x2, extn

begin
	# Check that bandpass is entirely within data.
	flux = 0.
	x2 = w0 + (npts - 1) * wpc
	if ((wave-dwave/2 < w0) || (wave+dwave/2 > x2))
	    return

	# Note that a pixel coordinate refers to the center of the pixel.

	wstart = w0 - wpc/2.0
	x1 = ((wave - dwave/2.0) - wstart) / wpc
	x2 = ((wave + dwave/2.0) - wstart) / wpc
	i1  = aint(x1) + 1
	i2  = aint(x2) + 1

	# Correct for extinction when summing.
	for (j = i1+1; j <= i2-1; j = j+1) {
	    w = w0 + (j-1) * wpc
	    call intrp (EXT_LOOKUP, Memr[EXT_WAVES(std)],
		Memr[EXT_EXTNS(std)], EXT_NWAVES(std), w, extn, ierr)
	    flux = flux + spec[j] * 10.0 ** (0.4 * extn * airmass)
	}
	w = wstart + x1 * wpc
	call intrp (EXT_LOOKUP, Memr[EXT_WAVES(std)], Memr[EXT_EXTNS(std)],
	    EXT_NWAVES(std), w, extn, ierr)
	flux = flux + (i1-x1) * spec[i1] * 10.0 ** (0.4 * extn * airmass)

	w = wstart + x2 * wpc
	call intrp (EXT_LOOKUP, Memr[EXT_WAVES(std)], Memr[EXT_EXTNS(std)],
	    EXT_NWAVES(std), w, extn, ierr)
	flux = flux + (1-(i2-x2)) * spec[i2] * 10.0 ** (0.4*extn*airmass)

	# Uncorrect to center of bandpass.
	call intrp (EXT_LOOKUP, Memr[EXT_WAVES(std)], Memr[EXT_EXTNS(std)],
	    EXT_NWAVES(std), wave, extn, ierr)
	flux = flux * 10.0 ** (-0.4 * extn * airmass)
end


procedure add_band (std, wave, dwave, flux)

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


procedure del_band (std, band)

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

procedure std_graph (gp, gt, clear, spec, npts, w0, wpc, std)

pointer	gp			# GIO pointer
pointer	gt			# GTOOLS pointer
int	clear			# Clear flag
real	spec[npts]		# Spectrum
int	npts			# Number of points
real	w0			# Starting wavelength
real	wpc			# Wavelength interval per pixel
pointer	std			# Standard star data

int	i
real	wn, wave, dwave, flux

begin
	wn = w0 + (npts - 1) * wpc
	if (clear == YES) {
	    call gclear (gp)
	    call gswind (gp, w0, wn, INDEF, INDEF)
	    call gascale (gp, spec, npts, 2)
	    call gt_swind (gp, gt)
	    call gt_labax (gp, gt)
	}

	call gt_vplot (gp, gt, spec, npts, w0, wn)

	do i = 0, STD_NWAVES(std)-1 {
	    wave = Memr[STD_WAVES(std)+i]
	    dwave = Memr[STD_DWAVES(std)+i]
	    flux = Memr[STD_FLUXES(std)+i] * wpc / dwave
	    if (flux == 0.)
		next
	    call gmark (gp, wave, flux, GM_BOX, -dwave, 3.)
	}
end
