include <imhdr.h>
include	"ccdred.h"


# T_MKFRINGECOR -- CL task to make fringe correction image.  The large scale
# background of the input images is subtracted from the input image to obtain
# the output fringe correction image.  The image is first processed if needed.

procedure t_mkfringecor()

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
	listout = imtopenp ("mkfringecor.output")
	call clgstr ("instrument", Memc[input], SZ_FNAME)
	call hdmopen (Memc[input])
	call set_interactive ("", interactive)
	call cal_open (NULL)
	call ccd_open (0)

	# Process each image.
	while (imtgetim (listin, Memc[input], SZ_FNAME) != EOF) {
	    if (clgetb ("noproc")) {
		call printf ("%s: mkfringecor\n")
		    call pargstr (Memc[input])
	    }
	    
	    # Set input and output images.  Use temporary image if needed.
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

	    # Process image as a flat field image.
	    call set_proc (in, out, ccd)
	    call set_sections (ccd)
	    call set_trim (ccd)
	    call set_fixpix (ccd)
	    call set_overscan (ccd)
	    call set_zero (ccd)
	    call set_dark (ccd)
	    call set_flat (ccd)
	    call set_illum (ccd)

	    # Do the processing.
	    if (CORS(ccd) == YES) {
	        call doproc (ccd)
	        call set_header (ccd)

	        # Finish up
	        call imunmap (in)
	        call imunmap (out)
	        if (streq (Memc[input], Memc[output])) {
		    call ccddelete (Memc[input])
	            call imrename (Memc[tmp], Memc[input])
		} else
		    call strcpy (Memc[output], Memc[input], SZ_FNAME)
	    } else {
		# Delete the temporary output image.  Make a copy if needed.
	        call imunmap (in)
	        call imunmap (out)
		call imdelete (Memc[tmp])
	    }
	    call free_proc (ccd)

	    # Do special processing.
	    call mkfringecor (Memc[input], Memc[output])
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


# MKFRINGECOR -- Given an input image which has been processed make the output
# fringe correction image.

procedure mkfringecor (input, output)

char	input[SZ_FNAME]		# Input image
char	output[SZ_FNAME]	# Output image

int	i, nc, nl
pointer	sp, str, illum, tmp, in, im, out, out1
bool	clgetb(), ccdflag(), streq()
pointer	immap(), imgl2r(), impl2r()
errchk	immap, ccddelete

begin
	# Check if this operation has been done.
	in = immap (input, READ_ONLY, 0)
	if (ccdflag (in, "mkfringe")) {
	    call imunmap (in)
	    return
	}

	# Print operation if not processing.
	if (clgetb ("noproc")) {
	    call eprintf (
		"  [TO BE DONE] Make fringe correction\n")
		call pargstr (input)
	    call imunmap (in)
	    return
	}

	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (illum, SZ_FNAME, TY_CHAR)
	call salloc (tmp, SZ_FNAME, TY_CHAR)

	# Make the illumination image.
	call imunmap (in)
	call strcpy (input, Memc[tmp], SZ_FNAME)
	call mktemp ("tmp", Memc[illum], SZ_FNAME)
	call mkillumination (Memc[tmp], Memc[illum], NO, NO)

	in = immap (input, READ_ONLY, 0)
	im = immap (Memc[illum], READ_ONLY, 0)

	# Create the temporary output.
	if (streq (input, output)) {
	    call mktemp ("tmp", Memc[tmp], SZ_FNAME)
	    call set_output (in, out, Memc[tmp])
	    out1 = in
	} else {
	    call set_output (in, out, output)
	    out1 = out
	}

	# Subtract the illumination from input image.
	nc = IM_LEN(out,1)
	nl = IM_LEN(out,2)
	do i = 1, nl
	    call asubr (Memr[imgl2r(in,i)], Memr[imgl2r(im,i)],
		Memr[impl2r(out,i)], nc)

	# Log the operation.
	call sprintf (Memc[str], SZ_LINE, "Fringe correction created")
	call timelog (Memc[str], SZ_LINE)
	call ccdlog (out1, Memc[str])
	call hdmpstr (out, "mkfringe", Memc[str])
	call hdmpstr (out, "imagetyp", "fringe")

	# Finish up
	call imunmap (in)
	call imunmap (im)
	call imunmap (out)
	call imdelete (Memc[illum])
	if (streq (input, output)) {
	    call ccddelete (input)
	    call imrename (Memc[tmp], input)
	} else
	    call strcpy (output, input, SZ_FNAME)
	call sfree (sp)
end
