include	<error.h>
include	<imhdr.h>
include	<mach.h>
include	<time.h>
include	<smw.h>

define	MAX_NR_BEAMS	100	# Max number of instrument apertures
define	MIN_RANGES	100	# Minimum spectra per beam if not given

# T_BSWITCH -- Beam switch a series of spectra to produce a single
#	     sky subtracted spectrum.
#
# The spectra may be extinction corrected if not already done.
#
# The summation may include an optional statistical weighting
# based on the total countrate summed over a user definable
# piece of the spectrum. If the countrate is <= 0, the
# spectrum is given zero weight.
#
# The data may be organized as data from the IIDS/IRS are usually
# obtained - where the telescope is beam-switched so that the
# object is first in one aperture while sky is observed in the other,
# and then the process is reversed.
#
# If the instrument offers many apertures, "nebular" mode can be used
# to obtain the same effect. Here all apertures observe the object(s)
# at one time; then the telescope is moved so all apertures are observing
# sky.
#
# Both these methods are considered "idsmode". But if there are a different
# number of sky observations than object, an imbalance exists. 
# To account for this possibility, all summations are performed by computing
# an average countrate over all observations. Sky countrates can then be
# subtracted from the object. Later the differential countrate is returned
# to an "equivalent" count by multiplying by the exposure time.
#
# Spectra must be dispersion corrected to employ either
# weighting or extinction correction.
#
# The series of spectra may be accumulated in subsets rather than
# over the entire series by specifying a subset rate. (E.g. for
# IIDS data a subset rate of 4 would produce a summed pair for
# every quadruple.)

# Revisions made for WCS support and change from idsmtn.h structure to shdr.h
# structure.  Because this program is an awful mess the changes were made a
# small as possible without altering the structure.  (5/1/91, Valdes)

procedure t_bswitch ()

char	image[SZ_FNAME,MAX_NR_BEAMS+1]
char	rec_numbers[SZ_LINE], title[SZ_LINE,MAX_NR_BEAMS]
char	ofile[SZ_FNAME], stat_fil[SZ_FNAME]
int	sfd, nrecsx
int	i, infile, nrecs, def_beam, start_rec, nimage, sub_rate
int	records[300], beam_stat[MAX_NR_BEAMS], ncols[MAX_NR_BEAMS]
bool	idsmode, extinct, stat, weight, eof_test
pointer	ids[MAX_NR_BEAMS+1] 
pointer	imnames[MAX_NR_BEAMS]	# Hold pointers to pointers of image names
pointer	imin, sp, obs

# The following arrays are suffixed by either 'o' for object or 's' for sky

int	ico   [MAX_NR_BEAMS],   ics   [MAX_NR_BEAMS]	# nr obs in beam
real	expo  [MAX_NR_BEAMS],   exps  [MAX_NR_BEAMS]	# exposure times
pointer	accumo[MAX_NR_BEAMS+1], accums[MAX_NR_BEAMS+1]	# beam accumulators
pointer	counto[MAX_NR_BEAMS],   counts[MAX_NR_BEAMS]	# counts in each obs

int	clpopni(), clgeti(), get_next_image(), decode_ranges()
int	open(), mod()
pointer	immap()
bool	clgetb(), streq()

begin
	call smark (sp)
	call aclri (ids, MAX_NR_BEAMS+1)

	# Open input filename template
	infile = clpopni ("input")

	# Get range specification
	call clgstr ("records", rec_numbers, SZ_LINE)
	if (decode_ranges (rec_numbers, records, 100, nrecs) == ERR)
	    call error (0, "Bad range specification")

	# If no ranges is given, filename expansion will occur, so
	# we must will need some indication of the number of spectra.
	if (nrecs == MAX_INT)
	    nrecsx = MIN_RANGES
	else
	    nrecsx = nrecs

	# Get root name for new records and starting record number
	call clgstr ("output", ofile, SZ_FNAME)
	start_rec = clgeti ("start_rec")

	# Get filename for statistics
	call clgstr ("stats", stat_fil, SZ_FNAME)

	# Assume spectra are in quadruples?
	idsmode = clgetb ("ids_mode")

	# Perform de-extinction?
	extinct = clgetb ("extinct")

	# Use weighting?
	weight = clgetb ("weighting")

	# Accumulate by subsets? - A very large number implies no subsetting
	sub_rate = clgeti ("subset")

	# Open statistics file if any
	if (streq (stat_fil, "")) {
	    sfd = NULL
	    stat = false
	} else {
	    stat = true
	    sfd = open (stat_fil, APPEND, TEXT_FILE)
	}

	# Initialize beam-switch status
	obs = NULL
	call init_file (extinct, def_beam, ico, ics, beam_stat)


	# Begin cycling through all images - accumulate if possible
	# by beam number

	# Initialize range decoder
	call reset_next_image ()

	# Set up for subsets
	nimage = 0
	eof_test = false

	repeat {

	while (get_next_image (infile, records, nrecs, image[1,def_beam],
	    SZ_FNAME) != EOF) {

	    # Attempt to open image with extended header -
	    iferr (imin = immap (image[1,def_beam], READ_ONLY, 0)) {
		call eprintf ("[%s]")
		    call pargstr (image[1,def_beam])
		call error (0, "Image not found or header info not available")
	    }

	    # Add in to accumlators
	    call accum_image (imin, ids, accumo, accums, counto, counts, 
		ico, ics, expo, exps, image, beam_stat, idsmode, extinct, 
		weight, nrecsx, ncols, title, imnames, sfd, obs)

	    call printf ("[%s] added\n")
		call pargstr (image[1,def_beam])
	    call flush (STDOUT)

	    # Close current image
	    call imunmap (imin)

	    # Test for subsets
	    nimage = nimage + 1
	    if (mod (nimage, sub_rate) == 0)
		go to 10
	}

	# Get here by running out of data
	eof_test = true

	# Must be careful not to write out the last sums if subsets are
	# in effect because the subset check would already have done so
	# We can check because "nimage" will not have been bumped
	# if EOF was encountered.

	if (mod (nimage, sub_rate) != 0) {
	
	    # All data has been summed - generate spectra of the accumlations
10	    call wrt_accum (ids, image, title, accumo, accums, ico, ics, 
		counto, counts, expo, exps, ncols, beam_stat, idsmode, weight, 
		extinct, ofile, start_rec, sub_rate)

	    # Generate statistics output for this beam
	    if (stat)
		call wrt_stats (sfd, accumo, accums, ico, ics, counto, counts, 
		    expo, exps, image, beam_stat, title, imnames, weight)

	    # Clear counters and accumulators
	    call reset_beams (accumo, accums, expo, exps, ico, ics, beam_stat, 
		ncols)
	}

	} until (eof_test)

	# Put current record counter back into the parameter file for
	# subsequent invocations
	call clputi ("next_rec", start_rec)

	# Close out inputs, outputs, and space
	do i = 1, MAX_NR_BEAMS+1
	    call shdr_close (ids[i])
	if (obs != NULL)
	    call obsclose (obs)
	call clpcls (infile)
	call close (sfd)
	call sfree (sp)
end

# ACCUM_IMAGE -- Opens current pixel file, loads header elements,
#                adds current spectrum to accumulator array(s),
#                and updates the accumulator status array.
#		 If not in IDSMODE, then returns both object and
#		 sky sums for further consideration.
#		 IDSMODE requires an equal number of each, object and sky,  in
#		 a sequence of OSSO-OSSO or OSSO-SOOS groups.

procedure accum_image (imin, ids, accumo, accums, counto, counts, ico, ics,
                expo, exps, image, beam_stat, idsmode, extinct, weight, nrecs,
		ncols, title, imnames, sfd, obs)

pointer	imin, ids[ARB]
pointer	imnames[ARB]			# Saved image names for stat printout
pointer	sfd				# Statistics file
pointer	obs				# Observatory

pointer	accumo[ARB], accums[ARB]	# Object and sky accumlators
pointer	counto[ARB], counts[ARB]	#                counting stats
real	expo  [ARB], exps  [ARB]	#                total exposure times
int	ico   [ARB], ics   [ARB]	#                number of observations

char	image[SZ_FNAME, MAX_NR_BEAMS+1], title[SZ_LINE,MAX_NR_BEAMS]
char	observatory[SZ_FNAME]
int	beam_stat[ARB], ncols[ARB]
int	dum_beam
bool	idsmode, extinct, weight, exflag, newobs, obshead
real	latitude

int	last_len[MAX_NR_BEAMS], name_nr[MAX_NR_BEAMS]
int	ifile, nr_beams, i, j, def_beam, beam_nr
int	nwaves, ic, nrecs
real	airm, wave1, wave2, wt
pointer	wave_tbl, extn_tbl, ipacc, ipc, mw

real	clgetr(), obsgetr()
pointer	smw_openim()
errchk	smw_openim, shdr_open, obsimopen

begin
	# Bump image file counter
	ifile = ifile + 1

	# Load header area
	mw = smw_openim (imin)
	call shdr_open (imin, mw, 1, 1, INDEFI, SHDATA, ids[def_beam])
	call smw_close (MW(ids[def_beam]))

	accumo[def_beam] = SY(ids[def_beam])

	# Check for proper flags
	call flag_chk (ids[def_beam], exflag)

	if (ifile == 1) {

	    # Get region for statistics to operate over -
	    # Currently only one set of wavelengths is available, but
	    # at some point, it may be desirable to extend this to
	    # provide a start and ending wavelength for each aperture
	    # since an aperture must be considered as an independent
	    # instrument.

	    # Insert defaults --> entire spectrum
	    # Now ask user for start and end - if =0.0, use defaults
	    wave1 = clgetr ("wave1")
	    wave2 = clgetr ("wave2")

	    if (wave1 == 0.0)
		wave1 = W0(ids[def_beam])
	    if (wave2 == 0.0)
		wave2 = W0(ids[def_beam]) + (IM_LEN(imin,1)-1) * 
		    WP(ids[def_beam])

	}

	# Determine beam number and add/sub in pixels
	# Remember that IIDS/IRS "beams" are 0-indexed

	beam_nr = BEAM(ids[def_beam]) + 1
	if (beam_nr > MAX_NR_BEAMS || beam_nr < 1)
	    call error (0, "Illegal beam number")

	# Allocate space for this aperture if not already done
	# Space must be allocated for 2 lines of spectra for
	# each aperture - Line 1 is used to sum up the most
	# recent object-sky spectra to maintain the local
	# statistics. Line 2 is used for the net accumulation
	# over the entire sequence. The statistics from Line 1
	# may be used to weigh the observations as they are
	# added into the Line 2 accumulation.
	#
	# For non-IDSMODE the two lines are used for separate
	# object and sky sums

	if (IS_INDEFI (beam_stat[beam_nr])) {
	    beam_stat[beam_nr] = 0

	    # Allocate space for the accumulators for this beam nr
	    call salloc (accumo[beam_nr], IM_LEN(imin,1), TY_REAL)
	    call salloc (accums[beam_nr], IM_LEN(imin,1), TY_REAL)

	    # Zero object and sky accumulators
	    call amovkr (0.0, Memr[accumo[beam_nr]], IM_LEN(imin,1))
	    call amovkr (0.0, Memr[accums[beam_nr]], IM_LEN(imin,1))


	    # Allocate space for statistics array - For each beam,
	    # a series of up to 'nrecs' spectra may be read, and we
	    # want to keep track of the stats (=countrates) for each
	    # observation. For non-idsmode, need sky rates too.
	    call salloc (counto[beam_nr], nrecs, TY_REAL)
	    if (!idsmode)
		call salloc (counts[beam_nr], nrecs, TY_REAL)

	    # Allocate space for the image names
	    call salloc (imnames[beam_nr], nrecs, TY_INT)
	    name_nr[beam_nr] = 1
	    do j = 1, nrecs
		call salloc (Memi[imnames[beam_nr]+j-1], SZ_LINE, TY_CHAR)

	    # Save number of points for checking purposes
	    last_len[beam_nr] = IM_LEN(imin,1)
	    ncols[beam_nr] = last_len[beam_nr]

	    # Initialize exposure time
	    expo[beam_nr] = 0.0
	    exps[beam_nr] = 0.0

	    nr_beams = nr_beams + 1
	}

	# If this is an object observation, save the image name
	if (OFLAG(ids[def_beam]) == 1) {
	    call strcpy (image[1,def_beam], Memc[Memi[imnames[beam_nr]+
		name_nr[beam_nr]-1]], SZ_LINE)
	    name_nr[beam_nr] = name_nr[beam_nr] + 1
	}

	# If an object observation, save the header elements --
	# NOTE that if we get >1 objects before getting a sky, only
	# the last  observation header is saved!

	# The pixel data will be the sum of all objects until the
	# |object-sky| count = 0 -- Thus, beam switching does not
	# necessarily accumulate by pairs, but depends on how the
	# sequence of observations are presented to the program.

	# The following test has been deleted so that headers
	# will be saved for sky frames as well. This is necessary
	# if BSWITCH is to perform the function of EXTINCTION
	# only when sky frames are to be written out as well.

	if (OFLAG(ids[def_beam]) == 1 || !idsmode) {
	    # Save headers - could probably be done faster by AMOV
	    call shdr_copy (ids[def_beam], ids[beam_nr], NO)

	    # Fix airmass if necessary
	    if (extinct && IS_INDEF (AM(ids[beam_nr]))) {
		call clgstr ("observatory", observatory, SZ_FNAME)
		call obsimopen (obs, imin, observatory, NO, newobs, obshead)
		if (newobs) {
		    call obslog (obs, "BSWITCH", "latitude", STDOUT)
		    if (sfd != NULL)
			call obslog (obs, "BSWITCH", "latitude", sfd)
		}
		latitude = obsgetr (obs, "latitude")
		call get_airm (RA(ids[beam_nr]), DEC(ids[beam_nr]),
		    HA(ids[beam_nr]), ST(ids[beam_nr]), latitude,
		    AM(ids[beam_nr]))
	    }

	    call strcpy (image[1,def_beam], image[1,beam_nr], SZ_FNAME)

	    # Save length - Each beam may be independent sizes
	    ncols[beam_nr] = IM_LEN(imin,1)

	    # Save title, too for same reason
	    call strcpy (IM_TITLE(imin), title[1,beam_nr], SZ_LINE)
	}

	# Verify length
	if (last_len[beam_nr] != ncols[beam_nr]) {
	    call eprintf ("[%s] -- Length not consistent %d\n")
		call pargstr (image[1,beam_nr])
		call pargi   (ncols[beam_nr])
	    ncols[beam_nr] = min (ncols[beam_nr], last_len[beam_nr])
	}
	last_len[beam_nr] = ncols[beam_nr]


	# Check to see if a pair is obtained - then perform statistics
	# and add into global accumulator

	if (idsmode) {

	    # Add spectrum to local accumulation buffer --> Use SKY buffer
	    # At this point of deriving a sequentially local sum, weighting
	    # is not used. 

	    call add_spec (Memr[accumo[def_beam]], Memr[accums[beam_nr]],
		beam_stat[beam_nr], OFLAG(ids[def_beam]), last_len[beam_nr])

	    # IDSMODE requires that every 2N observations produce an
	    # OBJECT-SKY pair
	    if (mod (ifile, 2*nr_beams) == 0)

		# Review all beams in use for non-zero pairings
		do i = 1, MAX_NR_BEAMS
		    if (!IS_INDEFI (beam_stat[i]) && beam_stat[i] != 0)
			call error (0, "Spectra are not in quadruples")


	    # Object and sky exposure times must be equal.
	    if (OFLAG(ids[def_beam]) == 1) {
		expo[beam_nr] = expo[beam_nr] + IT(ids[def_beam])

		# Increment number of object observations for this beam
		ico[beam_nr] = ico[beam_nr] + 1
	    }


	    if (beam_stat[beam_nr] == 0) {
		# Add up all counts within a region for statistics of objects
		# This must be kept separately for each beam number and for
		# each observation

		# First convert to counts per second (CPS)
		call adivkr (Memr[accums[beam_nr]], IT(ids[def_beam]),
		    Memr[accums[beam_nr]], last_len[beam_nr])

		# Sum CPS in statistics region
		call sum_spec (Memr[accums[beam_nr]], wave1, wave2, 
		    W0(ids[def_beam]), WP(ids[def_beam]), Memr[counto[beam_nr]+
		    ico[beam_nr]-1], last_len[beam_nr])

		# De-extinct spectrum
		if (extinct && !exflag) {
		    airm = AM(ids[beam_nr])
		    call de_ext_spec (Memr[accums[beam_nr]], airm, 
			W0(ids[def_beam]), WP(ids[def_beam]), Memr[wave_tbl], 
			Memr[extn_tbl], nwaves, last_len[beam_nr])
		}

		# Add to global accumulator
		# Use weights which are proportional to countrate, if desired
		if (weight) {
		    wt = Memr[counto[beam_nr]+ico[beam_nr]-1]
		    call amulkr (Memr[accums[beam_nr]], wt,
			Memr[accums[beam_nr]], last_len[beam_nr])
		}

		# And add into global sum
		call aaddr (Memr[accums[beam_nr]], Memr[accumo[beam_nr]],
		    Memr[accumo[beam_nr]], last_len[beam_nr])
	    }

	} else {
	    # Non IDSMODE -accumulate separate object and sky CPS sums

	    # Set pointers and update obj-sky parameters
	    if (OFLAG(ids[def_beam]) == 1) {
		beam_stat[beam_nr] = beam_stat[beam_nr] + 1
		ipacc = accumo[beam_nr]
		ipc   = counto[beam_nr]
		ico[beam_nr] = ico[beam_nr] + 1
		ic = ico[beam_nr]
		expo[beam_nr] = expo[beam_nr] + IT(ids[def_beam])
	    } else {
		beam_stat[beam_nr] = beam_stat[beam_nr] - 1
		ipacc = accums[beam_nr]
		ipc   = counts[beam_nr]
		ics[beam_nr] = ics[beam_nr] + 1
		ic = ics[beam_nr]
		exps[beam_nr] = exps[beam_nr] + IT(ids[def_beam])
	    }

	    # First convert to counts per second (CPS)
	    call adivkr (Memr[accumo[def_beam]], IT(ids[def_beam]),
		    Memr[accumo[def_beam]], last_len[beam_nr])

	    # Get counting stats
	    call sum_spec (Memr[accumo[def_beam]], wave1, wave2, 
		W0(ids[def_beam]), WP(ids[def_beam]), Memr[ipc+ic-1], 
		last_len[beam_nr])

	    # De-extinct spectrum
	    if (extinct && !exflag) {
		airm = AM(ids[beam_nr])
		call de_ext_spec (Memr[accumo[def_beam]], airm, 
		    W0(ids[def_beam]), WP(ids[def_beam]), Memr[wave_tbl], 
		    Memr[extn_tbl], nwaves, last_len[beam_nr])
	    }

	    if (weight) {
		wt = Memr[ipc+ic-1]
		call amulkr (Memr[accumo[def_beam]], wt, Memr[accumo[def_beam]],
		    last_len[beam_nr])
	    }

	    # Add into appropriate accumulator
	    call aaddr (Memr[accumo[def_beam]], Memr[ipacc], Memr[ipacc],
		last_len[beam_nr])
	}
	
	return

# INIT_FILE -- Zero the file initializer, the beam counter, beam stats
#	       and read the extinction data if necessary

entry init_file (extinct, dum_beam, ico, ics, beam_stat)

	ifile = 0
	nr_beams = 0
	def_beam = MAX_NR_BEAMS + 1
	dum_beam = def_beam

	do i = 1, MAX_NR_BEAMS {
	    beam_stat[i] = INDEFI
	    ico[i] = 0
	    ics[i] = 0
	}

	# If extinction required, read in extinction file, and sensitivity file
	if (extinct)
	    call get_extn (wave_tbl, extn_tbl, nwaves)

	return

# INIT_NAME -- Reset name index counter for a beam number

entry init_name (dum_beam)

	name_nr[dum_beam] = 1
	return
end

# ACCUM_OUT -- Checks accumulator flags and writes out a new summed
#              image if the count is zero

procedure accum_out (accum, image, ncols, title, root, rec, beam_nr,
	bsflag, itm, exflag)

real	accum[ARB], itm
char	image[SZ_FNAME], title[SZ_LINE], root[SZ_FNAME]
int	ncols, rec, beam_nr
int	bsflag, exflag

pointer	imin, imout, spec
char	bs_image[SZ_FNAME]

pointer	immap(), impl1r()

begin
	# Create new image with user area
	# Use ROOT for spectrum name and increment starting record number

	call sprintf (bs_image, SZ_FNAME, "%s.%04d")
	    call pargstr (root)
	    call pargi (rec)

	rec = rec + 1

	# Provide user info
	call printf ("writing: [%s] %s\n")
	    call pargstr (bs_image)
	    call pargstr (title)
	call flush (STDOUT)

	imin = immap (image, READ_ONLY, 0)
	imout = immap (bs_image, NEW_COPY, imin)

	# Add standard image header
	IM_NDIM(imout) = 1
	IM_LEN(imout,1) = ncols
	IM_PIXTYPE(imout) = TY_REAL
	call strcpy (title, IM_TITLE(imout), SZ_LINE)

	# Write out pixels
	spec = impl1r (imout)
	call amovr (accum, Memr[spec], ncols)

	# Update changed parameters
	if(bsflag == 1)
	    call imaddi (imout, "BS-FLAG", bsflag)
	call imaddr (imout, "EXPTIME", itm)
	call imaddi (imout, "EX-FLAG", exflag)

	call imunmap (imin)
	call imunmap (imout)

	# Store new image name back into image
	call strcpy (bs_image, image, SZ_FNAME)
end

# ACCUM_NORM - Normalize weighted rate and convert to counts

procedure accum_norm (accum, nr, counts, exp, ncols, weight)

real	accum[ARB], counts[ARB], exp
int	nr, ncols
bool	weight

real	sum_wt
int	i

begin
	# The accumulation is an array weighted by non-normalized weights
	# Normalize to total weight to produce a true weighted average
	# and multiply by the total exposure to produce
	# an equivalent sum

	# Add up all weighting factors
	if (weight) {
	    sum_wt = 0.0
	    do i = 1, nr
		sum_wt = sum_wt + counts[i]
	} else
	    sum_wt = real (nr)

	if (sum_wt == 0.0)
	    sum_wt = 1.0

	# Correct for exposure time
	sum_wt = exp / sum_wt

	call amulkr (accum, sum_wt, accum, ncols)
end

# WRT_ACCUM -- Write out accumulations as spectra

procedure wrt_accum (ids, image, title, accumo, accums, ico, ics, 
	counto, counts, expo, exps, ncols, beam_stat, idsmode, weight, 
	extinct, ofile, start_rec, sub_rate)

pointer	ids[ARB]
char	image[SZ_FNAME,MAX_NR_BEAMS+1], title[SZ_LINE,MAX_NR_BEAMS]

pointer	accumo[ARB], accums[ARB]
pointer	counto[ARB], counts[ARB]
int	ico   [ARB], ics   [ARB]
real	expo  [ARB], exps  [ARB]
int	ncols[ARB]
int	beam_stat[ARB]
bool	idsmode, weight, extinct
char	ofile[SZ_FNAME]
int	start_rec, sub_rate, bsflag

int	i, nr_beams
real	exp_ratio

begin
	# First compute number of beams
	nr_beams = 0
	do i = 1, MAX_NR_BEAMS
	    if (!IS_INDEFI (beam_stat[i]) && ((ico[i] > 0) || (ics[i] > 0)))
		nr_beams = nr_beams + 1

	# For all present apertures, write out a spectrum
	do i = 1, MAX_NR_BEAMS {

	    if (!IS_INDEFI (beam_stat[i]) && ((ico[i] > 0) || (ics[i] > 0))) {
		if (beam_stat[i] != 0 && idsmode) {
		    call eprintf ("Non-equal number of obj-sky observations")
		    call eprintf (" beam: %d - residual: %d\n")
			call pargi (i-1)
		        call pargi (beam_stat[i])

		    # Reset to 0 and force output
		    beam_stat[i] = 0
		}

		# The accumulator has a total CPS using non-normalized
		# weights - apply normalization and exposure time to
		# generate an equivalent COUNT sum.
		call accum_norm (Memr[accumo[i]], ico[i], Memr[counto[i]], 
			expo[i], ncols[i], weight)

		if (!idsmode) {
		    # Separate object and sky sums require sky info
		    call accum_norm (Memr[accums[i]], ics[i], Memr[counts[i]],
			exps[i], ncols[i], weight)

		    # Then normalize sky exposure time to that of object
		    if (exps[i] != 0.0)
			exp_ratio = expo[i]/exps[i]
		    else
			exp_ratio = 1.0

		    # Check that some object observtion was made
		    # If not, then we only have sky data so multiply by -1
		    # so that the subsequent subtraction will produce a
		    # positive sky
		    if (expo[i] == 0.0)
			exp_ratio = -1.0

		    if (exp_ratio != 1.0)
			call amulkr (Memr[accums[i]], exp_ratio,
			    Memr[accums[i]], ncols[i])

		    # Finally subtract sky from object equivalent counts
		    call asubr (Memr[accumo[i]], Memr[accums[i]], 
			Memr[accumo[i]], ncols[i])

		}
		# Set header flags
		# BS flag is not set if the subset rate equals the
		# number of apertures since each record in is copied out
		if (sub_rate > nr_beams)
		    bsflag = 1
		else
		    bsflag = -1

		if (OFLAG(ids[i]) == 1)
		    IT(ids[i]) = expo[i]
		else
		    IT(ids[i]) = exps[i]

		if (extinct)
		    EC(ids[i]) = 0

		# And write out spectrum, at last
		call accum_out (Memr[accumo[i]], image[1,i], 
		    ncols[i], title[1,i], ofile, start_rec, i,
		    bsflag, IT(ids[i]), EC(ids[i]))

		# Reset name entry counter
		call init_name (i)
	    }

	}
end

# RESET_BEAMS -- Zeroes the counters and accumulators for additional
#                cases

procedure reset_beams (accumo, accums, expo, exps, ico, ics, beam_stat, ncols)

pointer	accumo[ARB], accums[ARB]
real	expo  [ARB], exps  [ARB]
int	ico   [ARB], ics   [ARB]
int	beam_stat[ARB], ncols[ARB]

int	i

begin
	do i = 1, MAX_NR_BEAMS
	    if (!IS_INDEFI (beam_stat[i])) {

		expo[i] = 0.0
		exps[i] = 0.0
		ico[i]  = 0
		ics[i]  = 0
		call amovkr (0.0, Memr[accumo[i]], ncols[i])
		call amovkr (0.0, Memr[accums[i]], ncols[i])
	    }
end

# WRT_STATS -- Write out statistics file

procedure wrt_stats (fd, accumo, accums, ico, ics, counto, counts,
     expo, exps, image, beam_stat, title, imnames, weight)

int	fd
pointer	accumo[ARB], accums[ARB], counto[ARB], counts[ARB]
real	expo[ARB], exps[ARB]
int	ico[ARB], ics[ARB], beam_stat[ARB]
char	image[SZ_FNAME,MAX_NR_BEAMS+1]
char	title[ARB]
pointer	imnames[ARB]
bool	weight

int	i, j
real	cmaxo, cmaxs
char	ctime[SZ_TIME]

long	clktime()

begin
	# Issue time stamp
	call cnvtime (clktime (long(0)), ctime, SZ_TIME)
	call fprintf (fd, "%s\n\n")
	    call pargstr (ctime)

	# Issue message if weighted sums in effect
	if (weight)
	    call fprintf (fd, "--> Using weighted averages <--\n\n")

	# Cycle over beams
	do i = 1, MAX_NR_BEAMS {
	    if (!IS_INDEFI (beam_stat[i])) {

		# Write out Object stats if any
		if (ico[i] > 0) {
		    call fprintf (fd, "Object statistics for beam %d -->[%s]\n")
			call pargi (i-1)
			call pargstr (image[1,i])
		    call fprintf (fd, "Title: %s\n")
			call pargstr (title)

		    # Find maximum count value for this beam
		    cmaxo = Memr[counto[i]]

		    do j = 1, ico[i]
		        cmaxo = max (cmaxo, Memr[counto[i]+j-1])

		    call fprintf (fd,
			"Obs  Relative CPS   Image%12wPeak CPS = %10.3g\n")
			call pargr (cmaxo)

		    if (cmaxo == 0.0)
			cmaxo = 1.0

		    do j = 1, ico[i] {
			call fprintf (fd, "%3d    %8.2f    [%s]\n")
			    call pargi (j)
			    call pargr (Memr[counto[i]+j-1] / cmaxo)
			    call pargstr (Memc[Memi[imnames[i]+j-1]])
		    }
		}

		# Write out sky stats if any
		if (ics[i] > 0) {
		    call fprintf (fd, "Sky statistics for beam %d\n")
			call pargi (i-1)

		    cmaxs = Memr[counts[i]]

		    do j = 1, ics[i]
		        cmaxs = max (cmaxs, Memr[counts[i]+j-1])

		    call fprintf (fd, "Obs  Relative CPS   Peak CPS = %10.3g\n")
			call pargr (cmaxs)

		    if (cmaxs == 0.0)
			cmaxs = 1.0

		    do j = 1, ics[i] {
			call fprintf (fd, "%3d    %8.2f\n")
			    call pargi (j)
			    call pargr (Memr[counts[i]+j-1] / cmaxs)
		    }
		}

		call fprintf (fd, "\n\n")
	    }
	}
end


# ADD_SPEC -- Accumulate spectrum into array - either add or subtract
#             Returns status = net number of object - sky apectra
#                            = 0 for equal numbers to indicate further
#                              processing may take place

procedure add_spec (inspec, accum, stat, flag, len)

real	inspec[ARB], accum[ARB]
int	stat, flag, len

int	i, add_sub

begin
	add_sub = 0

	# Is this an Object or Sky?
	# If flag is neither 0 or 1, spectrum is ignored
	if (flag == 0)
	    add_sub = -1
	if (flag == 1)
	    add_sub = +1

	if (add_sub == 0) {
	    stat = INDEFI
	    return
	}

	# Is accumulator to be cleared?
	if (IS_INDEFI (stat) || stat == 0) {
	    call amulkr (inspec, real (add_sub), accum, len)
	    stat = add_sub

	} else {
	    # Add into accumulator
	    do i = 1, len
		accum[i] = accum[i] + add_sub * inspec[i]

	    stat = stat + add_sub
	}
end

# FLAG_CHK -- Check header flags prior to beam switching

procedure flag_chk (ids, exflag)

pointer	ids
bool	exflag

int	bsflag, imgeti()

begin
	# BS requires 
	# 1. dispersion corrected spectra
	# 2. non-beam switched
	# 3. may be either extinction corrected or not

	if (DC(ids) != DCLINEAR)
	    call error (0, "Spectrum not dispersion corrected")

	iferr (bsflag = imgeti (IM(ids), "BS-FLAG"))
	    bsflag = -1
	if (bsflag == 1)
	    call error (0, "Spectrum already beam-switched")

	if (EC(ids) == ECYES)
	    exflag = true
	else
	    exflag = false
end
