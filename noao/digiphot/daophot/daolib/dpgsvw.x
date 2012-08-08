# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <imio.h>
include <imhdr.h>
include <math.h>

# DP_GSWV -- Set the data window and viewport for the image display.

procedure dp_gswv (id, image, im, max_nframes)

pointer	id			# pointer to the image display graphics stream
char	image			# the input image name
pointer	im			# pointer to the input image
int	max_nframes		# the maximum number of display frames

real	vx1, vx2, vy1, vy2

begin
	if (id == NULL)
	    return
	call dp_gsview (image, im, max_nframes, vx1, vx2, vy1, vy2)
	call gsview (id, vx1, vx2, vy1, vy2)
	call gswind (id, 1.0, real (IM_LEN(im,1)), 1.0, real (IM_LEN(im,2)))
end


# DP_GSVIEW -- Map the viewport and window of the image display.

procedure dp_gsview (image, im, max_nframes, vx1, vx2, vy1, vy2)

char	image			# the input image name
pointer	im			# pointer to the input image
int	max_nframes		# the maximum number of display frames
real	vx1, vx2, vy1, vy2	# the output viewport

int	i, frame, wcs_status, dim1, dim2, step1, step2
pointer	sp, rimname, frimage, frimname, frim, iw
real	x1, x2, y1, y2, fx1, fx2, fy1, fy2, junkx, junky
bool	streq()
pointer	imd_mapframe(), iw_open()

begin
	# Allocate some memory.
	call smark (sp)
	call salloc (rimname, SZ_FNAME, TY_CHAR)
	call salloc (frimage, SZ_FNAME, TY_CHAR)
	call salloc (frimname, SZ_FNAME, TY_CHAR)

	# Get the root image name.
	call imgimage (image, Memc[rimname], SZ_FNAME)

	# Loop through the defined image frames searching for the one
	# which has the image loaded.

	frame = 0
	do i = 1, max_nframes {
	    frim = imd_mapframe (i, READ_ONLY, NO)
	    iw = iw_open (frim, i, Memc[frimage], SZ_FNAME, wcs_status)
	    call imgimage (Memc[frimage], Memc[frimname], SZ_FNAME)
	    if (streq (Memc[rimname], Memc[frimname])) {
		frame = i
		break
	    } else {
	        call iw_close (iw)
	        call imunmap (frim)
	    }
	}

	# Default to current frame if the image has not been displayes?
	if (frame == 0) {
	    call eprintf ("Warning: image %s is not loaded in the display\n")
		call pargstr (Memc[rimname])
	    vx1 = 0.0
	    vx2 = 1.0
	    vy1 = 0.0
	    vy2 = 1.0
	    call sfree (sp)
	    return
	}

	# Find the beginning and end points of the requested image section.
	# We already know at this point that the input logical image is
	# 2-dimensional. However this 2-dimensional section may be part of
	# n-dimensional image.

	# X dimension.
	dim1 = IM_VMAP(im,1)
	step1 = IM_VSTEP(im,dim1)
	if (step1 >= 0) {
	    x1 = IM_VOFF(im,dim1) + 1
	    x2 = x1 + IM_LEN(im,1) - 1
	} else {
	    x1 = IM_VOFF(im,dim1) - 1
	    x2 = x1 - IM_LEN(im,1) + 1
	}

	# Y dimension.
	dim2 = IM_VMAP(im,2)
	step2 = IM_VSTEP(im,dim2)
	if (step2 >= 0) {
	    y1 = IM_VOFF(im,dim2) + 1
	    y2 = y1 + IM_LEN(im,2) - 1
	} else {
	    y1 = IM_VOFF(im,dim2) - 1
	    y2 = y1 - IM_LEN(im,2) + 1
	}

	# Get the frame buffer coordinates corresponding to the lower left
	# and upper right corners of the image section.

	call iw_im2fb (iw, x1, y1, fx1, fy1)
	call iw_im2fb (iw, x2, y2, fx2, fy2)
	if (fx1 > fx2) {
	    junkx = fx1
	    fx1 = fx2 
	    fx2 = junkx
	}
	if (fy1 > fy2) {
	    junky = fy1
	    fy1 = fy2 
	    fy2 = junky
	}

	# Check that some portion of the input image is in the display.
	# If not select the default viewport and window coordinates.
	if (fx1 > IM_LEN(frim,1) || fx2 < 1.0 || fy1 > IM_LEN(frim,2) ||
	    fy2 < 1.0) {
	    vx1 = 0.0
	    vx2 = 1.0
	    vy1 = 0.0
	    vy2 = 1.0
	    call iw_close (iw)
	    call imunmap (frim)
	    call sfree (sp)
	    return
	}

	# Compute a new viewport and window for X.
	if (fx1 >= 1.0)
	    vx1 = max (0.0, min (1.0, (fx1 - 0.5) / IM_LEN(frim,1)))
	else
	    vx1 = 0.0
	if (fx2 <= IM_LEN(frim,1))
	    vx2 = max (0.0, min (1.0, (fx2 + 0.5) / IM_LEN(frim,1)))
	else
	    vx2 = 1.0

	# Compute a new viewport and window for Y.
	if (fy1 >= 1.0)
	    vy1 = max (0.0, min (1.0, (fy1 - 0.5) / IM_LEN(frim,2)))
	else
	    vy1 = 0.0
	if (fy2 <= IM_LEN(frim,2))
	    vy2 = max (0.0, min (1.0, (fy2 + 0.5)  / IM_LEN(frim,2)))
	else
	    vy2 = 1.0

	# Clean up.
	call iw_close (iw)
	call imunmap (frim)
	call sfree (sp)
end
