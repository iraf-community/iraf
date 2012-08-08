include <fset.h>
include <gset.h>
include <imhdr.h>
include <imset.h>

define	TV_NLINES	128


# T_DISPNAME -- Get the displayed image name.

procedure t_dispname ()

pointer	image	               	# pointer to name of the image

pointer	sp, im, iw, tmpname
int	frame, wcs_status
bool	verbose

int	clgeti(), imstati()
bool	clgetb()
pointer	imd_mapframe(), iw_open()

begin
	# Set standard output to flush on newline.
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Allocate working space.
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (tmpname, SZ_FNAME, TY_CHAR)

	frame = clgeti ("frame")
	verbose = clgetb ("verbose")


	# Open the frame as an image.
	im = imd_mapframe (frame, READ_WRITE, YES)
        iw = iw_open (im, frame, Memc[image], SZ_FNAME, wcs_status)

	call clpstr ("name", Memc[image])
	if (verbose) {
	    call printf ("%s\n")
		call pargstr (Memc[image])
	}

	call iw_close (iw)
	call imunmap (im)
	call sfree (sp)
end
