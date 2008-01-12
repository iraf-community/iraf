# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<mach.h>
include	"wdes.h"
include	"crtpict.h"


# CRT_ESTABLISH_TRANSFORM -- Set up both the spatial and greyscale mapping.
# The window descriptor "wdes" is filled.

procedure crt_establish_transform (gp, im, cl, wdes)

pointer	gp
pointer im
pointer	cl
pointer	wdes

int	w1, len_stdline
real	z1, z2

bool	fp_equalr()
int	strncmp()
errchk	crt_xy_scale, strcmp, im_minmax, zscale

begin
	# Procedure xy_scale calculates and stores the spatial
	# transformation fields of structure wdes.

	call crt_xy_scale (gp, cl, im, wdes)

	w1 = W_WC(wdes, 1)

	# Now for the intensity to greyscale mapping.  Values z1 and z2,
	# the intensities that map to the lowest and highest greyscale
	# levels, are calculated and stored in the wdes structure. First,
	# set up the default values.  These will not be changed if 
	# ztrans = "user".

	W_ZT(w1) = W_USER
	z1 = INDEFR
	z2 = INDEFR

	# Put up to date min, max values in the header structure, if necessary
	if (IM_LIMTIME(im) < IM_MTIME(im))
	    call im_minmax (im, IM_MIN(im), IM_MAX(im))

	if (strncmp (ZTRANS(cl), "min_max", 1) == 0) {
	    W_ZT(w1) = W_LINEAR
	    if (Z1(cl) != Z2(cl)) {
		# Use values set by user
		z1 = Z1(cl)
		z2 = Z2(cl)
	    } else {
		# Use image min and max unless the user has set the contrast
		# parameter to alter the slope of the transfer function.
		if (abs (CONTRAST(cl) - 1.0) > EPSILON) {
		    # CONTRAST = 1.0
		    z1 = IM_MIN(im)
		    z2 = IM_MAX(im)
		} else if (abs (CONTRAST(cl) + 1.0) > EPSILON) {
		    # CONTRAST = -1.0
		    z1 = IM_MAX(im)
		    z2 = IM_MIN(im)
		} else {
		    len_stdline = SAMPLE_SIZE / NSAMPLE_LINES(cl)
		    call zscale (im, z1, z2, CONTRAST(cl), SAMPLE_SIZE, 
			len_stdline)
		}
	    }
	}

	if (strncmp (ZTRANS(cl), "auto", 1) == 0) {
	    W_ZT(w1) = W_LINEAR
	    # Calculate optimum z1 and z2 from image mode
	    len_stdline = SAMPLE_SIZE / NSAMPLE_LINES(cl)
	    call zscale (im, z1, z2, CONTRAST(cl), SAMPLE_SIZE, len_stdline)
	    if (IM_PIXTYPE(im) == TY_SHORT) {
	        if (short (z1) == short (z2))
		    call error (0, 
			"No range in data, ztrans=auto failed: z1 = z2")
	    }
	    else if (fp_equalr (z1, z2))
		call error (0, "No range in data, ztrans=auto failed: z1=z2")
	}

	# Set the intensity extremes of the window descriptor
	if (strncmp (ZTRANS(cl), "none", 1) == 0) {
	    W_ZT(w1) = W_UNITARY
	    W_ZS(w1) = IM_MIN(im)
	    W_ZE(w1) = IM_MAX(im)
	} else {
	    W_ZS(w1) = z1
	    W_ZE(w1) = z2
	}
end
