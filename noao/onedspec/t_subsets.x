include	<mach.h>
include	<ctype.h>
include	<error.h>
include	<imhdr.h>
include	<fset.h>
include "oned.h"
include "idsmtn.h"

# T_SUBSETS -- Sub a series of spectra by pairs. A single spectrum
#  is produced for every pair.
#

procedure t_subsets ()

char	image[SZ_FNAME]
char	rec_numbers[SZ_LINE], ofile[SZ_FNAME]
int	root, nfiles, start_rec, subset
int	nrecs, records[3, MAX_RANGES]
int	npts, nrem, ifile, tog
real	expo, wtsum
pointer	ids[2], sp, sp1, im[2], cur_pix, sp_sum

int	clpopni(), clplen(), clgeti()
int	get_next_image(), decode_ranges()
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

	# Force STDOUT flush
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Initialize range decoder
	call reset_next_image ()

	# Mark dynamic space
	call smark (sp)

	# Allocate space for IDS header
	call salloc (ids[1], LEN_IDS, TY_STRUCT)
	call salloc (POINT(ids[1]), MAX_NCOEFF, TY_REAL)
	call salloc (ids[2], LEN_IDS, TY_STRUCT)
	call salloc (POINT(ids[2]), MAX_NCOEFF, TY_REAL)

	#Initialize file counter
	ifile = 0

	# Set weighting value needed by spectrum writer
	wtsum = 1.0

	# Define subset of operation is a pair
	subset = 2

	# Loop over all input images by subsets
	while (get_next_image (root, records, nrecs, image, 
	    SZ_FNAME) != EOF) {

	    # Get toggle value
	    tog = mod (ifile, 2) + 1

	    # Open image
	    iferr (im[tog] = immap (image, READ_ONLY, 0)) {
		call eprintf ("Header info not available for [%s]")
		    call pargstr (image)
	    }

	    # Load header data into input and output header arrays
	    call load_ids_hdr (ids[tog], im[tog], 1)
	    cur_pix = imgl1r (im[tog], 1)

	    # Allocate space for the sum
	    if (mod (ifile,2) == 0) {
		npts = IM_LEN (im[tog],1)
		call smark (sp1)
		call salloc (sp_sum, npts, TY_REAL)
		call aclrr (Memr[sp_sum], npts)

		# Zero exposure counter
		expo = 0.0

		# Add first spectrum
	        call amovr (Memr[cur_pix], Memr[sp_sum], npts)

	        expo = ITM(ids[tog])
	        call printf ("[%s] added\n")
	    	call pargstr (image)

	    } else {
		# Subtract second spectrum
		call asubr (Memr[sp_sum], Memr[cur_pix], Memr[sp_sum], 
		    min (npts, IM_LEN(im[tog],1)))
		call printf ("[%s] subtracted\n")
	    	call pargstr (image)
		call imunmap (im[2])

		call wrt_set (Memr[sp_sum], subset, ids[1], im[1], ofile, 
		    start_rec, expo, wtsum)
		call sfree (sp1)
	    }

	    ifile = ifile + 1
	}
	# Check that there are no remaining spectra in an unfulfilled subset
	nrem = mod (ifile, 2)
	if (nrem != 0) {
	    call sfree (sp1)

	    call eprintf ("Unfulfilled pair ignored\n")
	}

	# Update record number
	call clputi ("onedspec.next_rec", start_rec)

	# Free space
	call sfree (sp)
	call clpcls (root)
end
