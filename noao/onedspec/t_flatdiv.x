include	<imhdr.h>
include	<error.h>
include	<imhdr.h>
include	<fset.h>
include "oned.h"
include "idsmtn.h"

# T_FLATDIV -- Divide by a flat field spectrum. This is basically
#  a simple division of two vectors but with the following
#  additional functions:
#
#	1. Check the processing flag of the input spectra to avoid
#	   double processing, and set the flag if the processing is
#	   performed.
#	2. Trap division by zero errors
#	3. Optionally apply coincidence corrections

procedure t_flatdiv ()

char	image[SZ_FNAME]
char	rec_numbers[SZ_LINE], ofile[SZ_FNAME], flat_file[SZ_FNAME]
char	ccoptions[SZ_LINE]
int	root, nfiles, start_rec
int	nrecs, records[3, MAX_RANGES]
int	beam_stat[MAX_NR_BEAMS]
int	len_flat
int	ccmode
real	dtime
real	power
bool	coincidence
pointer	ids, sp, flatsp, im

int	clpopni(), clplen(), clgeti(), clgwrd()
int	get_next_image(), decode_ranges()
real	clgetr()
bool	clgetb()
pointer	immap()

begin
	# Open input file name template
	root   = clpopni ("input")
	nfiles = clplen (root)

	# Get range specification if any
	call clgstr ("records", rec_numbers, SZ_LINE)
	if (decode_ranges (rec_numbers, records, MAX_RANGES, nrecs) == ERR)
	    call error (0, "Bad range specification")

	# Get rootname for output files and starting record
	# Subtract 1 from start_rec because 1 will be added later.
	call clgstr ("output", ofile, SZ_FNAME)
	start_rec = clgeti ("start_rec") - 1

	# Get flat field spectrum root name
	call clgstr ("flat_file", flat_file, SZ_FNAME)

	# Apply coincidence corrections?
	coincidence = clgetb ("coincor")
	if (coincidence) {
	    ccmode = clgwrd ("ccmode", ccoptions, SZ_LINE, ",photo,iids,")
	    dtime  = clgetr ("deadtime")
	    power = clgetr ("power")
	}

	# Force STDOUT flush
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Initialize beam number status
	call init_bs (beam_stat)

	# Initialize range decoder
	call reset_next_image ()

	# Mark dynamic space
	call smark (sp)

	# Allocate space for IDS header
	call salloc (ids, LEN_IDS, TY_STRUCT)
	call salloc (POINT(ids), MAX_NCOEFF, TY_REAL)

	# Loop over all input images - divide and make new image.
	# The output record number is incremented in all cases.
	while (get_next_image (root, records, nrecs, image, SZ_FNAME) != EOF) {
	    start_rec = start_rec + 1

	    # Open image
	    iferr (im = immap (image, READ_ONLY, 0)) {
		call eprintf ("[%s]")
		    call pargstr (image)
		call error (0, " Image not found or header info not available")
	    }

	    # Load header data into input and output header arrays
	    call load_ids_hdr (ids, im, 1)

	    # Verify divide flag
	    if (QD_FLAG(ids) != 0) {

		# Get flat field spectrum if needed
		call get_flatsp (im, ids, flatsp, flat_file, beam_stat, 
		    len_flat)

		# Calibrate the current spectrum and make a calibrated version
		call divide (im, ids, flatsp, len_flat, image, ofile,
		    start_rec, coincidence, ccmode, dtime, power)

	    } else {
		call eprintf ("[%s] already divided - ignored\n")
		    call pargstr (image)
	    }
	}

	# Update record number
	call clputi ("onedspec.next_rec", start_rec)

	# Free space
	call sfree (sp)
	call clpcls (root)
end

# GET_FLATSP -- Load flat field spectrum for the current beam number

procedure get_flatsp (im, ids, sp, flat_file, beam_stat, len_flat)

pointer	im, ids, sp
char	flat_file[SZ_FNAME]
int	beam_stat[ARB], len_flat

int	i
int	beam, len[MAX_NR_BEAMS]
char	sfname[SZ_FNAME]
pointer	flatsp[MAX_NR_BEAMS], imflat

int	strlen()
pointer	imgl1r(), immap()

begin
	# Determine beam number.

	beam = BEAM(ids) + 1

	# Validate beam number
	if (beam < 1 || beam > MAX_NR_BEAMS) {
	    call eprintf (" Beam number out of range: %d - using 0\n")
		call pargi (beam)
	    beam = 1
	}

	# Has this beam already been loaded?
	if (beam_stat[beam] == INDEFI) {

	    # Create file name
	    call strcpy (flat_file, sfname, SZ_FNAME)

	    # Flat field file names have beam number appended
	    call sprintf (sfname[strlen(sfname)+1], SZ_FNAME, ".%04d")
		call pargi (beam-1)

	    # Open spectrum
	    iferr (imflat = immap (sfname, READ_ONLY, 0)) {
		call eprintf ("[%s]")
		    call pargstr (sfname)
		call error (0, " Cannot find sensitivity spectrum")
	    }

	    # Allocate space for this beam's sensitivity spectrum
	    call salloc (flatsp[beam], IM_LEN(imflat,1), TY_REAL)

	    # Copy pixels into space
	    call amovr (Memr[imgl1r(imflat,1)], Memr[flatsp[beam]],
		IM_LEN(imflat,1))

	    # Must be careful that no division by zero occurs.
	    do i = 1, IM_LEN(imflat,1) {
		if (Memr[flatsp[beam]+i-1] == 0.0)
		    Memr[flatsp[beam]+i-1] = 1.0
	    }

	    # Mark this beam accounted for
	    beam_stat[beam] = 1
	    len[beam] = IM_LEN(imflat,1)

	    call imunmap (imflat)
	}

	# Point to the spectrum
	sp = flatsp[beam]
	len_flat = len[beam]

end

# DIVIDE -- Perform the division and create new spectrum

procedure divide (im, ids, flat, len_flat, ifile, ofile, rec, 
	coincidence, ccmode, dtime, power)

pointer	im, ids, flat
int	len_flat, rec, ccmode
real	dtime, power
char	ifile[ARB], ofile[ARB]
bool	coincidence

int	i
int	ncols, nlines
char	calfname[SZ_FNAME], original[SZ_FNAME]
pointer	imcal, rawpix, calpix

pointer	immap(), impl1r(), imgl1r()

begin
	# Find smallest length of the two possible spectra
	ncols = min (IM_LEN (im, 1), len_flat)

	# Create new spectrum.  Make up a name
	call sprintf (calfname, SZ_FNAME, "%s.%04d")
	    call pargstr (ofile)
	    call pargi (rec)

	call xt_mkimtemp (ifile, calfname, original, SZ_FNAME)
	imcal = immap (calfname, NEW_COPY, im)

	IM_NDIM(imcal) = IM_NDIM(im)
	IM_LEN (imcal,1) = ncols
	IM_PIXTYPE(imcal) = TY_REAL

	# Check for 2D spectrum
	if (IM_NDIM(im) > 1)
	    nlines = IM_LEN(im,2)
	else
	    nlines = 1

	# Copy across the image title
	call strcpy (IM_TITLE(im), IM_TITLE(imcal), SZ_LINE)

	# Operate on the pixels
	do i = 1, nlines {
	    rawpix = imgl1r (im   ,i)
	    calpix = impl1r (imcal,i)

	    # Apply coincidence correction if needed
	    if (coincidence)
	        if (CO_FLAG (ids) < 1)
		    call coincor (Memr[rawpix], Memr[rawpix], ncols, ids,
			ITM (ids), dtime, power, ccmode)

	    call adivr (Memr[rawpix], Memr[flat], Memr[calpix], ncols)
	}

	# Alter keyword flag
	QD_FLAG(ids) = 0

	# Write out keywords
	call store_keywords (ids, imcal)

	# Send user report
	call printf ("writing [%s]: %s\n")
	    call pargstr (original)
	    call pargstr (IM_TITLE(imcal))

	call imunmap (im)
	call imunmap (imcal)
	call xt_delimtemp (calfname, original)
end

# INIT_BS -- Initialize beam status flags

procedure init_bs (beam_stat)

int	beam_stat[ARB]

int	i

begin
	do i = 1, MAX_NR_BEAMS
	    beam_stat[i] = INDEFI
end
