include	<error.h>
include	"ccdred.h"
include	"ccdtypes.h"

# CCDPROC -- Process a CCD image of a specified CCD image type.
#
# The input image is corrected for bad pixels, overscan levels, zero
# levels, dark counts, flat field, illumination, and fringing.  It may also
# be trimmed.  The checking of whether to apply each correction, getting the
# required parameters, and logging the operations is left to separate
# procedures, one for each correction.  The actual processing is done by
# a specialized procedure designed to be very efficient.  These
# procedures may also process calibration images if necessary.
# The specified image type overrides the image type in the image header.
# There are two data type paths; one for short data types and one for
# all other data types (usually real).

procedure ccdproc (input, ccdtype)

char	input[ARB]		# CCD image to process
int	ccdtype			# CCD type of image (independent of header).

pointer	sp, output, str, in, out, ccd, immap()
errchk	immap, set_output, ccddelete
errchk	set_fixpix, set_zero, set_dark, set_flat, set_illum, set_fringe

begin
	call smark (sp)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Map the image, make a working output image and set the processing
	# parameters.

	in = immap (input, READ_ONLY, 0)
	call mktemp ("tmp", Memc[output], SZ_FNAME)
	call set_output (in, out, Memc[output])
	call set_proc (in, out, ccd)
	call set_sections (ccd)
	call set_trim (ccd)
	call set_fixpix (ccd)
	call set_overscan (ccd)

	# Set processing appropriate for the various image types.
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
	    call set_illum (ccd)
	    call set_fringe (ccd)
	default:
	    call set_zero (ccd)
	    call set_dark (ccd)
	    call set_flat (ccd)
	    call set_illum (ccd)
	    call set_fringe (ccd)
	    CORS(ccd, FINDMEAN) = YES
	}

	# Do the processing if the COR flag is set.
	if (COR(ccd) == YES) {
	    call doproc (ccd)
	    call set_header (ccd)

	    # Replace the input by the output image.
	    call imunmap (in)
	    call imunmap (out)
	    iferr (call ccddelete (input)) {
		call imdelete (Memc[output])
		call error (1,
		    "Can't delete or make backup of original image")
	    }
	    call imrename (Memc[output], input)
	} else {
	    # Delete the temporary output image leaving the input unchanged.
	    call imunmap (in)
	    iferr (call imunmap (out))
		;
	    iferr (call imdelete (Memc[output]))
		;
	}
	call free_proc (ccd)

	# Do special processing for calibration images.
	switch (ccdtype) {
	case ZERO:
	    call readcor (input)
	case FLAT:
	    call ccdmean (input)
	}

	call sfree (sp)
end
