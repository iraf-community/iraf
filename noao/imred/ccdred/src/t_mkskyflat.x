include <imhdr.h>
include	"ccdred.h"
include	"ccdtypes.h"


# T_MKSKYFLAT -- Apply a sky observation to a flat field to remove the
# residual illumination pattern.

procedure t_mkskyflat()

int	listin			# List of input CCD images
int	listout			# List of output CCD images
int	ccdtype			# CCD image type
int	interactive		# Fit overscan interactively?

bool	flatcor, ccdflag(), clgetb(), streq()
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
	listout = imtopenp ("mkskyflat.output")
	call clgstr ("instrument", Memc[input], SZ_FNAME)
	call hdmopen (Memc[input])
	call set_interactive ("", interactive)
 
	# Force flat fields even if flatcor=no
	flatcor = clgetb ("flatcor")
	call clputb ("flatcor", true)
	call cal_open (NULL)
	call ccd_open (0)
	call clputb ("flatcor", flatcor)

	# Process each image.
	while (imtgetim (listin, Memc[input], SZ_FNAME) != EOF) {
	    if (clgetb ("noproc")) {
		call printf ("%s: mkskyflat\n")
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

	    # Process image as an illumination image.
	    call set_proc (in, out, ccd)
	    call set_sections (ccd)
	    call set_trim (ccd)
	    call set_fixpix (ccd)
	    call set_overscan (ccd)
	    call set_zero (ccd)
	    call set_dark (ccd)
	    call set_flat (ccd)

	    # Do the processing.
	    if (CORS(ccd) == YES) {
	        call doproc (ccd)
	        call set_header (ccd)

	        # Finish up
	        flatcor = ccdflag (out, "flatcor")
	        call imunmap (in)
	        call imunmap (out)
	        if (streq (Memc[input], Memc[output])) {
		    call ccddelete (Memc[input])
	            call imrename (Memc[tmp], Memc[input])
		} else
		    call strcpy (Memc[output], Memc[input], SZ_FNAME)
	    } else {
		# Delete the temporary output image.  Make a copy if needed.
	        flatcor = ccdflag (out, "flatcor")
	        call imunmap (in)
	        call imunmap (out)
		call imdelete (Memc[tmp])
	    }
	    call free_proc (ccd)

	    # Do special processing.
	    if (!flatcor) {
	        call eprintf (
		    "%s: WARNING - Image should be flat fielded first\n")
		    call pargstr (Memc[input])
	    }
	    call mkillumination (Memc[input], Memc[output], NO, YES)
	    call mkskyflat (Memc[input], Memc[output])
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


# MKSKYFLAT -- Make a sky flat by dividing the input illumination image by
# the flat field.

procedure mkskyflat (input, output)

char	input[SZ_FNAME]		# Input image
char	output[SZ_FNAME]	# Output image

int	i, nc, nl
long	time
real	scale
pointer	sp, str, flat, tmp, in, im, out, out1, data

int	hdmgeti()
bool	clgetb(), ccdflag(), streq()
real	hdmgetr()
pointer	immap(), imgl2r(), impl2r()
errchk	immap, ccddelete

begin
	# Check if this operation has been done.
	in = immap (input, READ_ONLY, 0)
	if (ccdflag (in, "skyflat")) {
	    call imunmap (in)
	    return
	}

	# Print operation if not processing.
	if (clgetb ("noproc")) {
	    call eprintf (
		"  [TO BE DONE] Convert %s to sky flat\n")
		call pargstr (input)
	    call imunmap (in)
	    return
	}

	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (flat, SZ_FNAME, TY_CHAR)
	call salloc (tmp, SZ_FNAME, TY_CHAR)

	# Get the flat field.
	call cal_image (in, FLAT, 1, Memc[flat], SZ_FNAME)
	im = immap (Memc[flat], READ_ONLY, 0)
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

	# Multiply the illumination and flat field images with scaling.
	nc = IM_LEN(out,1)
	nl = IM_LEN(out,2)
	do i = 1, nl {
	    data = impl2r (out, i)
	    call amulr (Memr[imgl2r(in,i)], Memr[imgl2r(im,i)],
		Memr[data], nc)
	    if (scale != 1.)
	        call adivkr (Memr[data], scale, Memr[data], nc)
	}

	# Log the operation.
	call sprintf (Memc[str], SZ_LINE,
	    "Sky flat created from %s and %s")
	    call pargstr (input)
	    call pargstr (Memc[flat])
	call timelog (Memc[str], SZ_LINE)
	call ccdlog (out1, Memc[str])
	call hdmpstr (out, "skyflat", Memc[str])
	call hdmpstr (out, "imagetyp", "flat")

	# Finish up
	call imunmap (in)
	call imunmap (im)
	call imunmap (out)
	if (streq (input, output)) {
	    call ccddelete (input)
	    call imrename (Memc[tmp], input)
	} else
	    call strcpy (output, input, SZ_FNAME)
	call sfree (sp)
end
