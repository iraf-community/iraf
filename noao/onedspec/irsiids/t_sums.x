include	<error.h>
include	<imhdr.h>

define	MAX_NR_BEAMS	100	# Max number of instrument apertures

# T_SUMS -- Compute sums of strings of spectra according to
#	    Aperture number and object/sky flag. So for IIDS/IRS
#           type spectra, 4 sums will be generated.
#           In general, there will be 2N sums where N is the number
#           apertures.

procedure t_sums ()

pointer	image					# Image name to be added
pointer	images					# Image name to be added
pointer	ofile					# Output image file name
pointer	recstr					# Record number string
int	recs					# Spectral record numbers
int	root, nrecs				# CL and ranges flags
real	expo					# Exposure time
pointer	bstat[2]				# Status of each aperture
pointer	npts[2]					# Length of spectrum
pointer	esum[2]					# Accumulated exposure time
pointer	accum[2]				# Pointers to beam accumulators
pointer	title[2]
int	beam, object
int	start_rec

int	i, j
pointer	sp, work, im

real	imgetr()
int	clgeti(), clpopni(), imgeti()
int	get_next_image(), decode_ranges()
pointer	immap()

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (images, MAX_NR_BEAMS, TY_POINTER)
	call salloc (ofile, SZ_FNAME, TY_CHAR)
	call salloc (recstr, SZ_LINE, TY_CHAR)
	call salloc (recs, 300, TY_INT)
	call salloc (accum, MAX_NR_BEAMS, TY_POINTER)
	call salloc (title, MAX_NR_BEAMS, TY_POINTER)
	call amovki (NULL, Memi[images], MAX_NR_BEAMS)
	call salloc (work, 2*5*MAX_NR_BEAMS, TY_STRUCT)
	bstat[1] = work
	bstat[2] = work + MAX_NR_BEAMS
	npts[1] = work + 2 * MAX_NR_BEAMS
	npts[2] = work + 3 * MAX_NR_BEAMS
	esum[1] = work + 4 * MAX_NR_BEAMS
	esum[2] = work + 5 * MAX_NR_BEAMS
	accum[1] = work + 6 * MAX_NR_BEAMS
	accum[2] = work + 7 * MAX_NR_BEAMS
	title[1] = work + 8 * MAX_NR_BEAMS
	title[2] = work + 9 * MAX_NR_BEAMS

	# Get task parameters.
	root = clpopni ("input")

	# Get input record numbers
	call clgstr ("records", Memc[recstr], SZ_LINE)
	if (decode_ranges (Memc[recstr], Memi[recs], 100, nrecs) == ERR)
	    call error (0, "Bad range specification")

	call clgstr ("output", Memc[ofile], SZ_LINE)

	start_rec = clgeti ("start_rec")

	call reset_next_image ()

	# Clear all beam status flags
	call amovki (INDEFI, Memi[bstat[1]], MAX_NR_BEAMS*2)
	call aclrr (Memr[esum[1]], MAX_NR_BEAMS*2)

	call printf ("Accumulating spectra --\n")
	call flush (STDOUT)

	while (get_next_image (root, Memi[recs], nrecs, Memc[image],
	    SZ_FNAME) != EOF) {
	    iferr (im = immap (Memc[image], READ_ONLY, 0)) {
		call erract (EA_WARN)
		next
	    }

	    # Load header
	    iferr (beam = imgeti (im, "BEAM-NUM"))
		beam = 0
	    if (beam < 0  || beam > MAX_NR_BEAMS-1)
		call error (0, "Invalid aperture number")

	    # Select array: Object = array 2; sky = array 1
	    iferr (object = imgeti (im, "OFLAG"))
		object = 1
	    if (object == 1)
		object = 2
	    else
		object = 1

	    iferr (expo = imgetr (im, "EXPOSURE"))
		iferr (expo = imgetr (im, "ITIME"))
		    iferr (expo = imgetr (im, "EXPTIME"))
			expo = 1

	    # Add spectrum into accumulator
	    if (IS_INDEFI (Memi[bstat[object]+beam])) {
	        Memi[npts[object]+beam] = IM_LEN (im,1)
	        call salloc (Memi[accum[object]+beam], IM_LEN(im,1), TY_REAL)
	        call aclrr (Memr[Memi[accum[object]+beam]], IM_LEN(im,1))
	        Memi[bstat[object]+beam] = 0

	        call salloc (Memi[title[object]+beam], SZ_LINE, TY_CHAR)
	        call strcpy (IM_TITLE(im), Memc[Memi[title[object]+beam]],
		    SZ_LINE)
	    }

	    call su_accum_spec (im, Memi[npts[1]], expo, Memi[bstat[1]],
		beam+1, Memi[accum[1]], Memr[esum[1]], Memi[title[1]], object)

	    call printf ("[%s] %s spectrum added to aperture %1d\n")
		call pargstr (Memc[image])
		if (object == 2)
		    call pargstr ("object")
		else
		    call pargstr ("sky   ")
		call pargi (beam)
	    call flush (STDOUT)

	    if (Memi[images+beam] == NULL)
		call salloc (Memi[images+beam], SZ_FNAME, TY_CHAR)
	    call strcpy (Memc[image], Memc[Memi[images+beam]], SZ_FNAME)
	    call imunmap (im)
	}

	# Review all apertures containing data and write sums
	do i = 0, MAX_NR_BEAMS-1
	    do j = 1, 2
	    if (!IS_INDEFI (Memi[bstat[j]+i])) {
		call wrt_spec (Memc[Memi[images+i]], Memr[Memi[accum[j]+i]],
		    Memr[esum[j]+i], Memc[ofile], start_rec,
		    Memc[Memi[title[j]+i]], Memi[npts[j]+i], i, j)

		start_rec = start_rec + 1
	    }

	call clputi ("next_rec", start_rec)
	call sfree (sp)
	call clpcls (root)
end

# ACCUM_SPEC -- Accumulate spectra by beams

procedure su_accum_spec (im, len, expo, beam_stat, beam, accum, expo_sum,
	title, object)

pointer	im, accum[MAX_NR_BEAMS,2], title[MAX_NR_BEAMS,2]
real	expo, expo_sum[MAX_NR_BEAMS,2]
int	beam_stat[MAX_NR_BEAMS,2], beam, len[MAX_NR_BEAMS,2]
int	object

int	npts
pointer	pix

pointer	imgl1r()

begin
	npts = IM_LEN (im, 1)

	# Map pixels and optionally correct for coincidence
	pix = imgl1r (im)

	# Add in the current data
	npts = min (npts, len[beam, object])

	call aaddr (Memr[pix], Memr[accum[beam, object]], 
	    Memr[accum[beam, object]], npts)

	beam_stat[beam, object] = beam_stat[beam, object] + 1
	expo_sum [beam, object] = expo_sum [beam, object] + expo
end

# WRT_SPEC -- Write out normalized spectrum

procedure wrt_spec (image, accum, expo_sum, ofile, start, title, npts, object,
	beam)

char	image[SZ_FNAME]
real	accum[ARB], expo_sum
int	start, npts
char	ofile[SZ_FNAME]
char	title[SZ_LINE]
int	object, beam

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

	newpix = impl1r (imnew)
	call amovr (accum, Memr[newpix], npts)

	call imaddr (imnew, "EXPOSURE", expo_sum)
	call imunmap (im)
	call imunmap (imnew)

	call printf ("%s sum for aperture %1d --> [%s]\n")
	    if (object == 1)
		call pargstr ("Object")
	    else
		call pargstr ("Sky   ")
	    call pargi (beam)
	    call pargstr (output)
	call flush (STDOUT)
end
