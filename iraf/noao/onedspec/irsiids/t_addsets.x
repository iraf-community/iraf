include	<error.h>
include	<imhdr.h>


# T_ADDSETS -- Add a series of spectra by subsets. A single spectrum
#  is produced for every "subset" number of input spectra. The input
#  list is accumulated until "subset" number of spectra have been
#  encountered. The result is then written out.
#
#  If the input data are calibrated (CA_FLAG = 0) then the result
#  is an average over the subset size, but the header exposure
#  time is updated.
#
#  If the data is uncalibrated then the resulting spectrum is a sum
#  of the total counts.

procedure t_addsets ()

pointer	image
pointer	recstr, ofile
int	root, start_rec, subset
int	nrecs
int	nrem, ifile, ca_flag
real	itm, expo, wt, wtsum
bool	weight
pointer	sp, recs, im, cur_pix, sp_sum

real	imgetr()
int	clpopni(), clgeti(), imgeti()
int	get_next_image(), decode_ranges()
bool	clgetb()
pointer	immap(), imgl1r()

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (ofile, SZ_FNAME, TY_CHAR)
	call salloc (recstr, SZ_LINE, TY_CHAR)
	call salloc (recs, 300, TY_INT)

	# Open input file name template
	root   = clpopni ("input")

	# Get range specification if any
	call clgstr ("records", Memc[recstr], SZ_LINE)
	if (decode_ranges (Memc[recstr], Memi[recs], 100, nrecs) == ERR)
	    call error (0, "Bad range specification")

	# Get rootname for output files and starting record
	call clgstr ("output", Memc[ofile], SZ_FNAME)
	start_rec = clgeti ("start_rec")

	# Get subset size
	subset = clgeti ("subset")

	# Apply integration time weighting?
	weight = clgetb ("weighting")

	# Initialize range decoder
	call reset_next_image ()

	#Initialize file counter
	ifile = 0
	wtsum = 0.0

	# Loop over all input images by subsets
	while (get_next_image (root, Memi[recs], nrecs, Memc[image], 
	    SZ_FNAME) != EOF) {

	    # Open image
	    iferr (im = immap (Memc[image], READ_ONLY, 0)) {
		call erract (EA_WARN)
		next
	    }

	    # Allocate space for current subset
	    if (mod (ifile, subset) == 0) {
		call calloc (sp_sum, IM_LEN (im,1), TY_REAL)

		# Zero exposure counter
		expo = 0.0
	    }

	    # Add in current spectrum
	    iferr (itm = imgetr (im, "EXPOSURE"))
		iferr (itm = imgetr (im, "ITIME"))
		    iferr (itm = imgetr (im, "EXPTIME"))
			itm = 1
	    iferr (ca_flag = imgeti (im, "CA-FLAG"))
		ca_flag = -1
	    cur_pix = imgl1r (im)

	    # Apply integration time weighting
	    if (weight)
		wt = itm
	    else
		wt = 1.0

	    if (ca_flag != 0)
		wt = 1.0

	    wtsum = wtsum + wt
	    call amulkr (Memr[cur_pix], wt, Memr[cur_pix], IM_LEN(im,1))
	    call aaddr (Memr[cur_pix], Memr[sp_sum], Memr[sp_sum], IM_LEN(im,1))
	    expo = expo + itm

	    # Issue status report
	    call printf ("[%s] added\n")
		call pargstr (Memc[image])

	    ifile = ifile + 1
	    if (mod (ifile, subset) == 0) {
		call wrt_set (Memr[sp_sum], subset, im, Memc[ofile], start_rec,
		    expo, wtsum, ca_flag)
		wtsum = 0.0
		call mfree (sp_sum, TY_REAL)
	    } else
		call imunmap (im)

	}
	# Check that there are no remaining spectra in an unfulfilled subset
	nrem = mod (ifile, subset)
	if (nrem != 0) {
	    call wrt_set (Memr[sp_sum], nrem, im, Memc[ofile], start_rec, 
		expo, wtsum, ca_flag)
	    wtsum = 0.0
	    call mfree (sp_sum, TY_REAL)

	    call eprintf ("Unfulfilled subset accumulation written - ")
	    call eprintf ("missing %d spectra\n")
		call pargi (subset - nrem)
	}

	# Update record number
	call clputi ("next_rec", start_rec)

	# Free space
	call sfree (sp)
	call clpcls (root)
end

# WRT_SET -- Write spectra ccumulated from the set

procedure wrt_set (sp_sum, subset, im, ofile, start_rec, expo, wtsum, ca_flag)

real	sp_sum[ARB]
int	subset, start_rec, ca_flag
pointer	im
char	ofile[SZ_FNAME]
real	expo, wtsum

char	newfile[SZ_FNAME]
pointer imnew, newpix

pointer	impl1r(), immap()
int	strlen()

begin
	# Create new spectrum - first make up a name
	call strcpy (ofile, newfile, SZ_FNAME)
	call sprintf (newfile[strlen (newfile) + 1], SZ_FNAME, ".%04d")
	    call pargi (start_rec)

	imnew = immap (newfile, NEW_COPY, im)

	IM_NDIM (imnew) = 1
	IM_LEN (imnew,1) = IM_LEN (im,1)
	IM_PIXTYPE (imnew) = TY_REAL
	call strcpy (IM_TITLE(im), IM_TITLE(imnew), SZ_LINE)

	call imunmap (im)

	newpix = impl1r (imnew)

	# If this spectrum is calibrated, perform an average
	# weighted by integration time and copy new pixels into image
	if (ca_flag == 0)
	    call adivkr (sp_sum, real (wtsum), Memr[newpix], IM_LEN(imnew,1))
	else
	    call amovr (sp_sum, Memr[newpix], IM_LEN(imnew,1))

	# Update keyword
	call imaddr (imnew, "EXPOSURE", expo)

	# Send user report
	call printf ("writing [%s]: %s\n")
	    call pargstr (newfile)
	    call pargstr (IM_TITLE(imnew))
	call flush (STDOUT)

	call imunmap (imnew)

	# Update record counter
	start_rec = start_rec + 1
end
