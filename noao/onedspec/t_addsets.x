include	<mach.h>
include	<ctype.h>
include	<error.h>
include	<imhdr.h>
include	<fset.h>
include "oned.h"
include "idsmtn.h"

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

char	image[SZ_FNAME]
char	rec_numbers[SZ_LINE], ofile[SZ_FNAME]
int	root, nfiles, start_rec, subset
int	nrecs, records[3, MAX_RANGES]
int	nrem, ifile
real	expo, wt, wtsum
bool	weight
pointer	ids, sp, sp1, im, cur_pix, sp_sum

int	clpopni(), clplen(), clgeti()
int	get_next_image(), decode_ranges()
bool	clgetb()
pointer	immap(), imgl1r()

begin
	# Open input file name template
	root   = clpopni ("input")
	nfiles = clplen (root)

	# Get range specification if any
	call clgstr ("records", rec_numbers, SZ_LINE)
	if (decode_ranges (rec_numbers, records, MAX_RANGES, nrecs) == ERR)
	    call error (0, "Bad range specification")

	# Get rootname for output files and starting record
	call clgstr ("output", ofile, SZ_FNAME)
	start_rec = clgeti ("start_rec")

	# Get subset size
	subset = clgeti ("subset")

	# Apply integration time weighting?
	weight = clgetb ("weighting")

	# Force STDOUT flush
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Initialize range decoder
	call reset_next_image ()

	# Mark dynamic space
	call smark (sp)

	# Allocate space for IDS header
	call salloc (ids, LEN_IDS, TY_STRUCT)
	call salloc (POINT(ids), MAX_NCOEFF, TY_REAL)

	#Initialize file counter
	ifile = 0
	wtsum = 0.0

	# Loop over all input images by subsets
	while (get_next_image (root, records, nrecs, image, 
	    SZ_FNAME) != EOF) {

	    # Open image
	    iferr (im = immap (image, READ_ONLY, 0)) {
		call eprintf ("Header info not available for [%s]")
		    call pargstr (image)
	    }

	    # Load header data into input and output header arrays
	    call load_ids_hdr (ids, im, 1)

	    # Allocate space for current subset
	    if (mod (ifile, subset) == 0) {
		call smark (sp1)
		call salloc (sp_sum, IM_LEN (im,1), TY_REAL)
		call amovkr (0.0, Memr[sp_sum], IM_LEN (im,1))

		# Zero exposure counter
		expo = 0.0
	    }

	    # Add in current spectrum
	    cur_pix = imgl1r (im, 1)

	    # Apply integration time weighting
	    if (weight)
		wt = ITM(ids)
	    else
		wt = 1.0

	    if (CA_FLAG(ids) != 0)
		wt = 1.0

	    wtsum = wtsum + wt
	    call amulkr (Memr[cur_pix], wt, Memr[cur_pix], IM_LEN(im,1))
	    call aaddr (Memr[cur_pix], Memr[sp_sum], Memr[sp_sum], IM_LEN(im,1))
	    expo = expo + ITM(ids)

	    # Issue status report
	    call printf ("[%s] added\n")
		call pargstr (image)

	    ifile = ifile + 1
	    if (mod (ifile, subset) == 0) {
		call wrt_set (Memr[sp_sum], subset, ids, im, ofile, start_rec,
		    expo, wtsum)
		wtsum = 0.0
		call sfree (sp1)
	    } else
		call imunmap (im)

	}
	# Check that there are no remaining spectra in an unfulfilled subset
	nrem = mod (ifile, subset)
	if (nrem != 0) {
	    call wrt_set (Memr[sp_sum], nrem, ids, im, ofile, start_rec, 
		expo, wtsum)
	    wtsum = 0.0
	    call sfree (sp1)

	    call eprintf ("Unfulfilled subset accumulation written - ")
	    call eprintf ("missing %d spectra\n")
		call pargi (subset - nrem)
	}

	# Update record number
	call clputi ("onedspec.next_rec", start_rec)

	# Free space
	call sfree (sp)
	call clpcls (root)
end

# WRT_SET -- Write spectra ccumulated from the set

procedure wrt_set (sp_sum, subset, ids, im, ofile, start_rec, expo, wtsum)

real	sp_sum[ARB]
int	subset, start_rec
pointer	ids, im
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

	newpix = impl1r (imnew, 1)

	# If this spectrum is calibrated, perform an average
	# weighted by integration time and copy new pixels into image
	if (CA_FLAG(ids) == 0)
	    call adivkr (sp_sum, real (wtsum), Memr[newpix], IM_LEN(imnew,1))
	else
	    call amovr (sp_sum, Memr[newpix], IM_LEN(imnew,1))

	# Update keyword
	ITM(ids) = expo
	call store_keywords (ids, imnew)

	# Send user report
	call printf ("writing [%s]: %s\n")
	    call pargstr (newfile)
	    call pargstr (IM_TITLE(imnew))

	call imunmap (imnew)

	# Update record counter
	start_rec = start_rec + 1
end
