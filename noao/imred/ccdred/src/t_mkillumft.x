include <imhdr.h>
include	"ccdred.h"


# T_MKILLUMFLAT -- Make illumination corrected flat field images.
#
# The input flat field images are processed and smoothed to obtain
# illumination pattern.  The illumination pattern is then divided out
# of the input image to make the output illumination corrected flat field
# image.

procedure t_mkillumflat()

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
	listout = imtopenp ("mkillumflat.output")
	call clgstr ("instrument", Memc[input], SZ_FNAME)
	call hdmopen (Memc[input])
	call set_interactive ("", interactive)
	call cal_open (NULL)
	call ccd_open (0)

	# Process each image.
	while (imtgetim (listin, Memc[input], SZ_FNAME) != EOF) {
	    if (clgetb ("noproc")) {
		call printf ("%s: mkillumflat\n")
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
	    call mkillumflat (Memc[input], Memc[output])
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


# MKILLUMFLAT -- Take the processed input image and make the illumination
# corrected flat field output image.  The illumination pattern is created
# as a temporary image and then the applied to the input flat field
# image to make the final output flat field image.  If the input and
# output names are the same the operation is done in place.

procedure mkillumflat (input, output)

char	input[SZ_FNAME]		# Input image
char	output[SZ_FNAME]	# Output image

int	i, nc, nl
real	scale
long	time
pointer	sp, str, illum, tmp, in, im, out, out1, data

bool	clgetb(), ccdflag(), streq()
int	hdmgeti()
real	hdmgetr(), clgetr(), divzero()
pointer	immap(), imgl2r(), impl2r()
errchk	immap, ccddelete
extern	divzero()

real	rdivzero	# Result for divion by zero
int	ndivzero	# Number of zero divisions
common	/cdivzero/ rdivzero, ndivzero

begin
	# Check if this operation has been done.
	in = immap (input, READ_ONLY, 0)
	if (ccdflag (in, "illumflt")) {
	    call imunmap (in)
	    return
	}

	# Print operation if not processing.
	if (clgetb ("noproc")) {
	    call eprintf (
		"  [TO BE DONE] Remove illumination\n")
		call pargstr (input)
	    call imunmap (in)
	    return
	}

	# Get and set task parameters for division by zero.
	rdivzero = clgetr ("divbyzero")
	ndivzero = 0

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
	iferr (scale = hdmgetr (im, "ccdmean"))
	    scale = 1.
	iferr (time = hdmgeti (im, "ccdmeant"))
	    time = IM_MTIME(im)
	if (time < IM_MTIME(im))
	    scale = 1.

	# Create the temporary output.
	if (streq (input, output)) {
	    call mktemp ("tmp", Memc[tmp], SZ_FNAME)
	    call set_output (in, out, Memc[tmp])
	    out1 = in
	} else {
	    call set_output (in, out, output)
	    out1 = out
	}

	# Divide the illumination and flat field images with scaling.
	nc = IM_LEN(out,1)
	nl = IM_LEN(out,2)
	do i = 1, nl {
	    data = impl2r (out, i)
	    call advzr (Memr[imgl2r(in,i)], Memr[imgl2r(im,i)],
		Memr[data], nc, divzero)
	    if (scale != 1.)
	        call amulkr (Memr[data], scale, Memr[data], nc)
	}

	# Log the operation.
	if (ndivzero > 0) {
	    call sprintf (Memc[str], SZ_LINE,
		"Warning: %d divisions by zero replaced by %g")
		call pargi (ndivzero)
		call pargr (rdivzero)
	    call ccdlog (out1, Memc[str])
	}
	call sprintf (Memc[str], SZ_LINE, "Removed illumination from flat")
	call sprintf (Memc[str], SZ_LINE,
	    "Illumination flat created from %s")
	    call pargstr (input)
	call timelog (Memc[str], SZ_LINE)
	call ccdlog (out1, Memc[str])
	call hdmpstr (out, "illumflt", Memc[str])
	call hdmpstr (out, "imagetyp", "flat")

	# Finish up
	call imunmap (in)
	call imunmap (im)
	call imunmap (out)
	call imdelete (Memc[illum])

	# The input name is changed to the output name for further processing.
	if (streq (input, output)) {
	    call ccddelete (input)
	    call imrename (Memc[tmp], input)
	} else
	    call strcpy (output, input, SZ_FNAME)
	call sfree (sp)
end
