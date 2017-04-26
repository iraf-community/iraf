include	"ccdred.h"

# T_MKILLUMCOR -- Make flat field illumination correction images.
#
# The input flat field images are processed and smoothed to obtain
# illumination correction images.  These illumination correction images
# are used to correct already processed images for illumination effects
# introduced by the flat field.

procedure t_mkillumcor()

int	listin			# List of input CCD images
int	listout			# List of output CCD images
int	ccdtype			# CCD image type
int	interactive		# Fit overscan interactively?

bool	clgetb(), streq()
int	imtopenp(), imtgetim()
pointer	sp, input, output, tmp, str, in, out, ccd
errchk	set_input, set_output, ccddelete

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (tmp, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Get the lists and instrument translation file.  Open the translation
	# file.  Initialize the interactive flag and the calibration images.

	listin = imtopenp ("input")
	listout = imtopenp ("mkillumcor.output")
	call clgstr ("instrument", Memc[input], SZ_FNAME)
	call hdmopen (Memc[input])
	call set_interactive ("", interactive)
	call cal_open (NULL)
	call ccd_open (0)

	# Process each image.
	while (imtgetim (listin, Memc[input], SZ_FNAME) != EOF) {
	    if (clgetb ("noproc")) {
		call printf ("%s: mkillumcor\n")
		    call pargstr (Memc[input])
	    }

	    #  Set input and output images.
	    call set_input (Memc[input], in, ccdtype)
	    if (in == NULL)
		next

	    if (imtgetim (listout, Memc[output], SZ_FNAME) == EOF)
		call strcpy (Memc[input], Memc[output], SZ_FNAME)
	    if (Memc[output] == EOS)
		call strcpy (Memc[input], Memc[output], SZ_FNAME)
	    if (streq (Memc[input], Memc[output]))
	        call mktemp ("tmp", Memc[tmp], SZ_FNAME)
	    else
		call strcpy (Memc[output], Memc[tmp], SZ_FNAME)
	    call set_output (in, out, Memc[tmp])

	    # Process image as an illumination image.
	    call set_proc (in, out, ccd)
	    call set_sections (ccd)
	    call set_trim (ccd)
	    call set_fixpix (ccd)
	    call set_overscan (ccd)
	    call set_zero (ccd)
	    call set_dark (ccd)
	    CORS(ccd, FINDMEAN) = YES

	    # Do the processing if the COR flag is set.
	    if (COR(ccd) == YES) {
	        call doproc (ccd)
	        call set_header (ccd)

		# Replace the input image by the corrected image.
	        call imunmap (in)
	        call imunmap (out)
		if (streq (Memc[input], Memc[output])) {
		    call ccddelete (Memc[input])
	            call imrename (Memc[tmp], Memc[input])
		} else
		    call strcpy (Memc[output], Memc[input], SZ_FNAME)
	    } else {
		# Make a copy if necessary.
	        call imunmap (in)
	        call imunmap (out)
		call imdelete (Memc[tmp])
	    }
	    call free_proc (ccd)

	    # Do special processing.
	    call mkillumination (Memc[input], Memc[output], YES, YES)
	    if (!streq (Memc[input], Memc[output]))
		call ccdcopy (Memc[input], Memc[output])
	}

	# Finish up.
	call hdmclose ()
	call imtclose (listin)
	call imtclose (listout)
	call cal_close ()
	call ccd_close ()
	call sfree (sp)
end
