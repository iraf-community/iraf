include	<imhdr.h>
include	<fset.h>
include	"oned.h"
include	"idsmtn.h"

# T_SUMS -- Compute sums of strings of spectra according to
#	    Aperture number and object/sky flag. So for IIDS/IRS
#           type spectra, 4 sums will be generated.
#           In general, there will be 2N sums where N is the number
#           apertures.

procedure t_sums ()

char	image[SZ_FNAME]				# Image name to be added
char	images[SZ_FNAME,MAX_NR_BEAMS]		# Image name to be added
char	rec_numbers[SZ_LINE]			# Spectral records string
char	ofile[SZ_FNAME]				# Output image file name
int	records[3, MAX_RANGES]			# Spectral record numbers
int	root, nfiles, nrecs			# CL and ranges flags
int	ifile					# File counter
real	expo					# Exposure time
int	beam_stat[MAX_NR_BEAMS,2]		# Status of each aperture
int	npts[MAX_NR_BEAMS,2]			# Length of spectrum
real	expo_sum[MAX_NR_BEAMS,2]		# Accumulated exposure time
pointer	accum[MAX_NR_BEAMS,2]			# Pointers to beam accumulators
pointer	ids[MAX_NR_BEAMS,2]
pointer	title[MAX_NR_BEAMS,2]
int	beam, object
int	start_rec

int	i, j
pointer	sp, xids, im

int	clgeti(), clpopni(), clplen()
int	get_next_image(), decode_ranges()
pointer	immap()

begin
	# Get task parameters.
	root = clpopni ("input")
	nfiles = clplen (root)

	# Get input record numbers
	call clgstr ("records", rec_numbers, SZ_LINE)
	if (decode_ranges (rec_numbers, records, MAX_RANGES, nrecs) == ERR)
	    call error (0, "Bad range specification")

	call clgstr ("output", ofile, SZ_LINE)

	start_rec = clgeti ("start_rec")

	# Force STDOUT
	call fseti (STDOUT, F_FLUSHNL, YES)

	call reset_next_image ()

	call smark (sp)
	call salloc (xids, LEN_IDS, TY_STRUCT)
	call salloc (POINT(xids), MAX_NCOEFF, TY_REAL)

	ifile = 0

	# Clear all beam status flags
	call amovki (INDEFI, beam_stat, MAX_NR_BEAMS*2)
	call aclrr (expo_sum, MAX_NR_BEAMS*2)

	call printf ("Accumulating spectra --\n")

	while (get_next_image (root, records, nrecs, image, SZ_FNAME) != EOF) {
	    iferr (im = immap (image, READ_ONLY, 0)) {
		call eprintf ("Header info not available for [%s]")
		    call pargstr (image)
	    } else {

	    # Load header
	    call load_ids_hdr (xids, im, 1)
	    beam = BEAM(xids) + 1
	    if (beam < 1  || beam > MAX_NR_BEAMS)
		call error (0, "Invalid aperture number")

	    # Select array: Object = array 2; sky = array 1
	    if (OFLAG(xids) == 1)
		object = 2
	    else
		object = 1

	    expo = ITM(xids)

	    # Add spectrum into accumulator
	    if (beam_stat[beam, object] == INDEFI) {
	        npts[beam, object] = IM_LEN (im,1)
	        call salloc (accum[beam, object], npts[beam, object], TY_REAL)
	        call aclrr (Memr[accum[beam, object]], npts[beam, object])
	        beam_stat[beam, object] = 0

	        call salloc (title[beam, object], SZ_LINE, TY_CHAR)
	        call strcpy (IM_TITLE(im), Memc[title[beam, object]], SZ_LINE)

	        call salloc (ids[beam, object], LEN_IDS, TY_STRUCT)
	        call salloc (POINT(ids[beam, object]), MAX_NCOEFF, TY_REAL)
	    }

	    call su_accum_spec (im, npts, expo, beam_stat, beam, accum,
		expo_sum, title, ids, object)

	    call printf ("[%s] %s spectrum added to aperture %1d\n")
		call pargstr (image)
		if (object == 2)
		    call pargstr ("object")
		else
		    call pargstr ("sky   ")
		call pargi (beam-1)

	    call strcpy (image, images[1, beam], SZ_FNAME)
	    call imunmap (im)
	    }
	}

	# Review all apertures containing data and write sums
	do i = 1, MAX_NR_BEAMS
	    do j = 1, 2
	    if (beam_stat[i,j] != INDEFI) {
		call wrt_spec (images[1,i], Memr[accum[i,j]], expo_sum[i,j],
		    ofile, start_rec, Memc[title[i,j]], npts[i,j], ids[i,j])

		start_rec = start_rec + 1
	    }

	call clputi ("onedspec.next_rec", start_rec)
	call sfree (sp)
	call clpcls (root)
end

# ACCUM_SPEC -- Accumulate spectra by beams

procedure su_accum_spec (im, len, expo, beam_stat, beam, accum, expo_sum,
	title, ids, object)

pointer	im, accum[MAX_NR_BEAMS,2], title[MAX_NR_BEAMS,2], ids[MAX_NR_BEAMS,2]
real	expo, expo_sum[MAX_NR_BEAMS,2]
int	beam_stat[MAX_NR_BEAMS,2], beam, len[MAX_NR_BEAMS,2]
int	object

int	npts
pointer	pix

pointer	imgl1r()

begin
	npts = IM_LEN (im, 1)

	# Allocate storage for this beam if necessary
	call load_ids_hdr (ids[beam, object], im, 1)

	# Map pixels and optionally correct for coincidence
	pix = imgl1r (im, 1)

	# Add in the current data
	npts = min (npts, len[beam, object])

	call aaddr (Memr[pix], Memr[accum[beam, object]], 
	    Memr[accum[beam, object]], npts)

	beam_stat[beam, object] = beam_stat[beam, object] + 1
	expo_sum [beam, object] = expo_sum [beam, object] + expo
end

# WRT_SPEC -- Write out normalized spectrum

procedure wrt_spec (image, accum, expo_sum, ofile, start, title, npts, ids)

char	image[SZ_FNAME]
real	accum[ARB], expo_sum
int	start, npts
char	ofile[SZ_FNAME]
char	title[SZ_LINE]
pointer	ids

char	output[SZ_FNAME], temp[SZ_LINE]
pointer	im, imnew, newpix

pointer	immap(), impl1r()
int	strlen()

begin
	im = immap (image, READ_ONLY, 0)
10	call strcpy (ofile, output, SZ_FNAME)
	call sprintf (output[strlen (output) + 1], SZ_FNAME, ".%04d")
	    call pargi (start)

	# Create new image with a user area
	# If an error occurs, ask user for another name to try
	# since many open errors result from trying to overwrite an
	# existing image.

	iferr (imnew = immap (output, NEW_COPY, im)) {
	    call eprintf ("Cannot create [%s] -- Already exists??\07\n")
		call pargstr (output)
	    call clgstr ("newoutput", ofile, SZ_FNAME)
	    go to 10
	}

	call strcpy ("Summation:", temp, SZ_LINE)
	call sprintf (temp[strlen (temp) + 1], SZ_LINE, "%s")
	    call pargstr (title)
	call strcpy (temp, IM_TITLE (imnew), SZ_LINE)

	newpix = impl1r (imnew, 1)
	call amovr (accum, Memr[newpix], npts)

	ITM (ids) = expo_sum
	call store_keywords (ids, imnew)
	call imunmap (im)
	call imunmap (imnew)

	call printf ("%s sum for aperture %1d --> [%s]\n")
	    if (OFLAG(ids) == 1)
		call pargstr ("Object")
	    else
		call pargstr ("Sky   ")
	    call pargi (BEAM(ids))
	    call pargstr (output)
end
