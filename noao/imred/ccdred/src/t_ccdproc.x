include	<imhdr.h>
include	<error.h>
include	"ccdred.h"
include	"ccdtypes.h"

define	CACHEUNIT	1000000.	# Units of max_cache parameter

# T_CCDPROC -- Process CCD images
#
# This is the main procedure for processing CCD images.  The images are
# corrected for bad pixels, overscan levels, zero levels, dark counts,
# flat field response, illumination errors, and fringe response.  They
# may also be trimmed.  The input is a list of images to be processed.
# Each image must match any image type requested.  The checking of
# whether to apply each correction, getting the required parameters, and
# logging the operations is left to separate procedures, one for each
# correction.  The actual processing is done by a specialized procedure
# designed to be very efficient.  These procedures may also process
# calibration images if necessary.  There are two data type paths; one
# for short pixel types and one for all other pixel types (usually
# real).

procedure t_ccdproc ()

int	list			# List of CCD images to process
int	ccdtype			# CCD image type
int	interactive		# Fit overscan interactively?
int	max_cache		# Maximum image cache size

bool	clgetb()
real	clgetr()
int	imtopenp(), imtgetim(), imtlen()
pointer	sp, input, output, str, in, out, ccd
errchk	set_input, set_output, ccddelete, cal_open
errchk	set_fixpix, set_zero, set_dark, set_flat, set_illum, set_fringe

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Get the list and instrument translation file.  Open the translation
	# file.  Initialize the interactive flag and the calibration images.

	list = imtopenp ("images")
	call clgstr ("instrument", Memc[input], SZ_FNAME)
	call hdmopen (Memc[input])
	call set_interactive ("", interactive)
	call cal_open (list)
	if (imtlen (list) < 3)
	    max_cache = 0.
	else
	    max_cache = CACHEUNIT * clgetr ("max_cache")
	call ccd_open (max_cache)

	# Process each image.
	while (imtgetim (list, Memc[input], SZ_FNAME) != EOF) {
	    if (clgetb ("noproc")) {
		call printf ("%s:\n")
		    call pargstr (Memc[input])
	    }
	    call set_input (Memc[input], in, ccdtype)
	    if (in == NULL)
		next

	    # Use a temporary image for output which will replace the input
	    # image after processing.

	    call mktemp ("tmp", Memc[output], SZ_FNAME)
	    call set_output (in, out, Memc[output])

	    # Set processing parameters applicable to all images.
	    call set_proc (in, out, ccd)
	    call set_sections (ccd)
	    call set_trim (ccd)
	    call set_fixpix (ccd)
	    call set_overscan (ccd)

	    # Set processing parameters for the standard CCD image types.
	    switch (ccdtype) {
	    case ZERO:
	    case DARK:
	        call set_zero (ccd)
	    case FLAT:
	        call set_zero (ccd)
	        call set_dark (ccd)
		CORS(ccd, FINDMEAN) = YES
		CORS(ccd, MINREP) = YES
	    case ILLUM:
	        call set_zero (ccd)
	        call set_dark (ccd)
	        call set_flat (ccd)
	    case OBJECT, COMP:
	        call set_zero (ccd)
	        call set_dark (ccd)
	        call set_flat (ccd)
		iferr {
	            call set_illum (ccd)
	            call set_fringe (ccd)
		} then
		    call erract (EA_WARN)
	    default:
	        call set_zero (ccd)
	        call set_dark (ccd)
	        call set_flat (ccd)
		iferr {
		    call set_illum (ccd)
	            call set_fringe (ccd)
	        } then
		    call erract (EA_WARN)
		CORS(ccd, FINDMEAN) = YES
	    }

	    # Do the processing if the COR flag is set.

	    if (COR(ccd) == YES) {
		call doproc (ccd)
		call set_header (ccd)

		# Replace the input image by the corrected image.
	        call imunmap (in)
	        call imunmap (out)
		iferr (call ccddelete (Memc[input])) {
		    call imdelete (Memc[output])
		    call error (1,
			"Can't delete or make backup of original image")
		}
	        call imrename (Memc[output], Memc[input])
	    } else {
		# Delete the temporary output image leaving the input unchanged.
	        call imunmap (in)
	        iferr (call imunmap (out))
		    ;
		iferr (call imdelete (Memc[output]))
		    ;
	    }
	    call free_proc (ccd)

	    # Do special processing on certain image types.
	    switch (ccdtype) {
	    case ZERO:
		call readcor (Memc[input])
	    case FLAT:
		call ccdmean (Memc[input])
	    }
	}

	# Finish up.
	call hdmclose ()
	call imtclose (list)
	call cal_close ()
	call ccd_close ()
	call sfree (sp)
end
