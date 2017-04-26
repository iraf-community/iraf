# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include <error.h>
include	<gset.h>
include	<fset.h>
include	<imhdr.h>
include	"crtpict.h"

# T_CRTPICT -- Code for the CRTPICT replacement.  Input to the procedure
# is an IRAF image; output is a file of GKI metacode instructions, 
# essentially x,y,z for each pixel on the dicomed.  The image intensities
# are scaled to the dynamic range of the output device.  The image
# is also scaled spatially, either expanded or reduced.  

procedure t_crtpict ()

bool	redir
pointer	sp, cl, gp, im, command, image, word, title, output, ofile, dev
int	cmd, stat, fd

pointer	immap(), gopen()
bool	clgetb(), streq()
int	strncmp(), clgeti(), btoi(), fstati(), open(), getline()
int	imtopenp(), list, imtgetim()
real	clgetr()

begin
	call smark (sp)
	call salloc (cl, LEN_CLPAR, TY_STRUCT)
	call salloc (command, SZ_LINE, TY_CHAR)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (word, SZ_LINE, TY_CHAR)
	call salloc (title, SZ_LINE, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (ofile, SZ_FNAME, TY_CHAR)
	call salloc (dev, SZ_FNAME, TY_CHAR)

	# If the input has been redirected, input is read from the named
	# command file.  If not, each image name in the input template is
	# plotted.
	
	call fseti (STDOUT, F_FLUSHNL, YES)
	if (fstati (STDIN, F_REDIR) == YES) {
	    call printf ("Input has been redirected\n")
	    redir = true
	    cmd = open ("STDIN", READ_ONLY, TEXT_FILE)
	} else 
	    list = imtopenp ("input")

	# The user can "trap" the output metacode and intercept the
	# spooling process if an output file is specified.
	call clgstr ("output", Memc[output], SZ_FNAME)
	if (!streq (Memc[output], "")) {
	    call strcpy (Memc[output], Memc[ofile], SZ_FNAME)
	    fd = open (Memc[ofile], NEW_FILE, BINARY_FILE)
	} else
	    fd = STDGRAPH

	# Now get the parameters necessary to calculate z1, z2.
	call clgstr ("ztrans", ZTRANS(cl), SZ_LINE)
	if (strncmp (ZTRANS(cl), "auto", 1) == 0) {
	    CONTRAST(cl) = clgetr ("contrast")
	    NSAMPLE_LINES(cl) = clgeti ("nsample_lines")
	} else if (strncmp (ZTRANS(cl), "min_max", 1) == 0) {
	    Z1(cl) = clgetr ("z1")
	    Z2(cl) = clgetr ("z2")
	    if (abs (Z1(cl) - Z2(cl)) < EPSILON) {
		CONTRAST(cl) = clgetr ("contrast")
		if (abs (CONTRAST(cl)) - 1.0 > EPSILON)
		    NSAMPLE_LINES(cl) = clgeti ("nsample_lines")
	    }
	} else if (strncmp (ZTRANS(cl), "user", 1) == 0)
	    call clgstr ("lutfile", UFILE(cl), SZ_FNAME)

	# Get parameters necessary to compute the spatial transformation.
	FILL(cl) = btoi (clgetb ("auto_fill"))
	if (FILL(cl) == NO) {
	    XMAG(cl) = clgetr ("xmag")
	    YMAG(cl) = clgetr ("ymag")
	    if (XMAG(cl) < 0)
		XMAG(cl) = 1 / abs (XMAG(cl))
	    if (YMAG(cl) < 0)
		YMAG(cl) = 1 / abs (YMAG(cl))
	}

	if (FILL(cl) == YES || XMAG(cl) - 1.0 > EPSILON || 
	    YMAG(cl) - 1.0 > EPSILON) {
	    # Find out how spatial scaling is to be done
	    REPLICATE(cl) = btoi (clgetb ("replicate"))
	    if (REPLICATE(cl) == NO) {
	        # Get block averaging factors
	        X_BA(cl) = clgetr ("x_blk_average")
	        Y_BA(cl) = clgetr ("y_blk_average")
	    }
	} else
	    REPLICATE(cl) = YES

	# And for the plotting fractions:
	PERIM(cl) = btoi (clgetb ("perimeter"))
	GRAPHICS_FRACTION(cl) = clgetr ("graphics_fraction")
	IMAGE_FRACTION(cl) = clgetr ("image_fraction")
	GREYSCALE_FRACTION(cl) = clgetr ("greyscale_fraction")

	# Get the output device name and determine the graphics pointer.
	call clgstr ("device", Memc[dev], SZ_FNAME)
	gp = gopen (Memc[dev], NEW_FILE, fd)

	# Loop over commands until EOF
	repeat {
	    if (redir) {
		if (getline (STDIN, Memc[command]) == EOF)
		    break
		call sscan (Memc[command])
		    call gargwrd (Memc[word], SZ_LINE)
		if (!streq (Memc[word], "plot")) {
		    # Pixel window has been stored as WCS 2
		    call gseti (gp, G_WCS, 2)
		    call gscan (gp, Memc[command])
		    next
		} else 
		    call gargwrd (Memc[image], SZ_FNAME)
	    } else {
		stat = imtgetim (list, Memc[image], SZ_FNAME)
		if (stat == EOF)
		    break
	    }

	    # Open the input image; if an error occurs, go to next image in list
	    iferr (im = immap (Memc[image], READ_ONLY, 0)) {
		call erract (EA_WARN)
		next
	    }

	    iferr (call crt_plot_image (gp, im, Memc[image], cl))
		call erract (EA_WARN)

	    # Add title to metacode file
	    call sprintf (Memc[title], SZ_LINE, "CRTPICT: %s")
		call pargstr (IM_TITLE(im))
	    call gmftitle (gp, Memc[title])
	    call imunmap (im)

	    # Clear frame for next picture (unless plotting to terminal)
	    if (strncmp (Memc[dev], "stdgraph", 4) == 0)
	        call gflush (gp)
	    else
	        call gclear (gp)
	}

	# Clean up and close files
	call gclose (gp)
	call close (fd)

	if (redir)
	    call close (cmd)
	else
	    call imtclose (list)

	call sfree (sp)
	call flush (STDOUT)
end
