include <imhdr.h>
include <gset.h>
include <gio.h>
include "igi.h"
include "commands.h"

procedure ig_wcslab (igs)

#  IG_WCSLAB -- Implements WCSLAB.  Draw the coordinate axes and labels
#  in WC, intended for celestial coordinates.  Use the mwcs in the
#  image header.  The ZSECTION command must have been used to read an
#  image.  Use the stored image name, open the image, and open the wcs.

## 7/2/93  ZGL
## 7/8/93  Add epar argument

pointer	igs		# Parameters structure

int	igps
pointer	im		# Image descriptor pointer
pointer	mw		# MWCS descriptor
real    c1, c2, l1, l2
pointer	tokvals		# Token value structure
int	token
pointer	pp		# Pset pointer
bool	wsactive
pointer	gp
bool	edit		# Edit psets?

int	gettok(), immap(), imaccess(), and()
pointer	mw_openim(), clopset()

begin
	call lcmdcat (igs, YES)
	igps = PLOT_PARMS(igs)

	tokvals = TOKEN_VALUE(igs)
	token = gettok (igs)

	if (!IS_NEWCOMMAND(token)) {
	    # An argument 
	    call lcmdcat (igs, NO)

	    if (LOP_VALC(tokvals) == 'e' || LOP_VALC(tokvals) == 'E') {
		# Edit the igiwcs pset
		edit = true

		# Open the pset
		pp = clopset ("wlpars")

		gp = GIO_GP(igs)
		wsactive = (and (GP_GFLAGS(gp), GF_WSACTIVE) != 0)

		if (wsactive)
		    call gdeactivate (gp, 0)

		call clepset (pp)

		if (wsactive)
		    call greactivate (gp, AW_PAUSE)

		# Close the pset
		call clcpset (pp)
	    }

	} else
	    edit = false

	if (MG_DATASRC(igps) == IMAGE_DATA) {
	    # An image was read using ZSECTTION

	    if (MG_FILE_NAME(igps) == EOS) {
    		call eprintf ("No File name saved ")
		return

	    } else if (imaccess (MG_FILE_NAME(igps), READ_ONLY) == NO) {
		call eprintf ("File %s not found ")
		    call pargstr (MG_FILE_NAME(igps))
		return

	    } else {
		# The image is there
		if (DEBUG_OUTPUT(igs) == YES) {
		    call eprintf ("image:  %s ")
			call pargstr (MG_FILE_NAME(igps))
		}

		# Map the image
		im = immap (MG_FILE_NAME(igps), READ_ONLY, NULL)

		# Get the image's MWCS.
		mw = mw_openim (im)

		c1 = 1.0
		c2 = real (IM_LEN(im,1))
		l1 = 1.0
		l2 = real (IM_LEN(im,2))
	    }

	} else {
	    # No image

	    if (edit) {
		# Open the pset
		pp = clopset ("wcspars")

		gp = GIO_GP(igs)
		wsactive = (and (GP_GFLAGS(gp), GF_WSACTIVE) != 0)

		if (wsactive)
		    call gdeactivate (gp, 0)

		call clepset (pp)

		if (wsactive)
		    call greactivate (gp, AW_PAUSE)

		# Close the pset
		call clcpset (pp)
	    }

	    im = NULL
	    mw = NULL

	    if (DEBUG_OUTPUT(igs) == YES) {
		call eprintf ("No image read; using wcspars ")
	    }

	    c1 = 0.0
	    c2 = 0.0
	    l1 = 0.0
	    l2 = 0.0
	}

        call wcslab (mw, c1, c2, l1, l2, GIO_GP(igs), EOS)

	call gflush (GIO_GP(igs))

#	call lcmdcat (igs, NO)
	call cmdcat  (igs, NO)

	if (mw != NULL)
	    call mw_close (mw)

	if (im != NULL)
	    call imunmap (im)
end
