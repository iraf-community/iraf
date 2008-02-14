# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <imio.h>
include <imhdr.h>
include <gset.h>
include <math.h>

# WL_IMD_VIEWPORT -- Map the viewport and window of the image display.

procedure wl_imd_viewport (frame, im, c1, c2, l1, l2, vl, vr, vb, vt)

int	frame			# I:   display frame to be overlayed
pointer	im			# I:   pointer to the input image
real	c1, c2, l1, l2		# I/O: input/output window
real	vl, vr, vb, vt		# I/O: input/output viewport

int	wcs_status, dim1, dim2, step1, step2
pointer	sp, frimage, frim, iw
real	x1, x2, y1, y2, fx1, fx2, fy1, fy2, junkx, junky
real	vx1, vx2, vy1, vy2, nx1, nx2, ny1, ny2
pointer	imd_mapframe(), iw_open()


begin
	# If all of the viewport parameters were defined by the user
	# use the default viewport and window.
	if (! IS_INDEFR(vl) && ! IS_INDEFR(vr) && ! IS_INDEFR(vb) &&
	    ! IS_INDEFR(vt))
	    return

	# Allocate some memory.
	call smark (sp)
	call salloc (frimage, SZ_FNAME, TY_CHAR)

	# Open the requested display frame and get the loaded image name.
	# If this name is blank, use the default viewport and window.

	frim = imd_mapframe (frame, READ_ONLY, YES)
	iw = iw_open (frim, frame, Memc[frimage], SZ_FNAME, wcs_status)
	if (Memc[frimage] == EOS || wcs_status == ERR) {
	    call iw_close (iw)
	    call imunmap (frim)
	    call sfree (sp)
	    return
	}

	# Find the beginning and end points of the requested image section.
	# We already know at this point that the input logical image is
	# 2-dimensional. However this 2-dimensional section may be part of
	# n-dimensional image.

	# X dimension.
	dim1 = IM_VMAP(im,1)
	step1 = IM_VSTEP(im,1)
	if (step1 >= 0) {
	    x1 = IM_VOFF(im,dim1) + 1
	    x2 = x1 + IM_LEN(im,1) - 1
	} else {
	    x1 = IM_VOFF(im,dim1) - 1
	    x2 = x1 - IM_LEN(im,1) + 1
	}

	# Y dimension.
	dim2 = IM_VMAP(im,2)
	step2 = IM_VSTEP(im,2)
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
	    call iw_close (iw)
	    call imunmap (frim)
	    call sfree (sp)
	    return
	}

	# Compute a new viewport and window for X.
	if (fx1 >= 1.0) {
	    vx1 = max (0.0, min (1.0, (fx1 - 0.5) / IM_LEN(frim,1)))
	    nx1 = 1.0
	} else {
	    vx1 = 0.0
	    call iw_fb2im (iw, 1.0, 1.0, junkx, junky)
	    if (step1 >= 0)
	        nx1 = max (1.0, junkx - x1 + 1.0)
	    else
	        nx2 = max (1.0, junkx - x2 + 1.0)
	}
	if (fx2 <= IM_LEN(frim,1)) {
	    vx2 = max (0.0, min (1.0, (fx2 + 0.5) / IM_LEN(frim,1)))
	    nx2 = IM_LEN(im,1)
	} else {
	    vx2 = 1.0
	    call iw_fb2im (iw, real(IM_LEN(frim,1)), real (IM_LEN(frim,2)),
	        junkx, junky)
	    if (step1 >= 0)
	        nx2 = min (real (IM_LEN(im,1)),  junkx - x1 + 1.0)
	    else
	        nx1 = min (real (IM_LEN(im,1)),  junkx - x2 + 1.0)
	}

	# Compute a new viewport and window for Y.
	if (fy1 >= 1.0) {
	    vy1 = max (0.0, min (1.0, (fy1 - 0.5) / IM_LEN(frim,2)))
	    ny1 = 1.0
	} else {
	    vy1 = 0.0
	    call iw_fb2im (iw, 1.0, 1.0, junkx, junky)
	    if (step2 >= 0)
	        ny1 = max (1.0, junky - y1 + 1)
	    else
	        ny2 = max (1.0, junky - y2 + 1)
	}
	if (fy2 <= IM_LEN(frim,2)) {
	    vy2 = max (0.0, min (1.0, (fy2 + 0.5)  / IM_LEN(frim,2)))
	    ny2 = IM_LEN(im,2)
	} else {
	    vy2 = 1.0
	    call iw_fb2im (iw, real (IM_LEN(frim,1)), real (IM_LEN(frim,2)),
	        junkx, junky)
	    if (step2 >= 0)
	        ny2 = min (real (IM_LEN(im,2)), junky - y1 + 1.0)
	    else
	        ny1 = min (real (IM_LEN(im,2)), junky - y2 + 1.0)
	}

	# Define a the new viewport and window. 
	if (IS_INDEFR(vl)) {
	    vl = vx1
	    c1 = nx1
	}
	if (IS_INDEFR(vr)) {
	    vr = vx2
	    c2 = nx2
	}
	if (IS_INDEFR(vb)) {
	    vb = vy1
	    l1 = ny1
	}
	if (IS_INDEFR(vt)) {
	    vt = vy2
	    l2 = ny2
	}

	# Clean up.
	call iw_close (iw)
	call imunmap (frim)
	call sfree (sp)
end


define  EDGE1  0.1
define  EDGE2  0.9
define  EDGE3  0.12
define  EDGE4  0.92

# WL_MAP_VIEWPORT -- Set device viewport wcslab plots. If not specified by
# user, a default viewport centered on the device is used.

procedure wl_map_viewport (gp, c1, c2, l1, l2, ux1, ux2, uy1, uy2, fill)

pointer	gp			# I:   pointer to graphics descriptor
real	c1, c2, l1, l2		# I:   the column and line limits
real	ux1, ux2, uy1, uy2	# I/O: NDC coordinates of requested viewort
bool	fill			# I:   fill viewport (vs preserve aspect ratio)

int	ncols, nlines
real	xcen, ycen, ncolsr, nlinesr, ratio, aspect_ratio
real	x1, x2, y1, y2, ext, xdis, ydis
bool	fp_equalr()
real	ggetr()
data	ext /0.0625/

begin
	ncols = nint (c2 - c1) + 1
	ncolsr = real (ncols)
	nlines = nint (l2 - l1) + 1
	nlinesr = real (nlines)

	# Determine the standard window sizes.
	if (fill) {
    	    x1 = 0.0;  x2 = 1.0
    	    y1 = 0.0;  y2 = 1.0
	} else {
    	    x1 = EDGE1;  x2 = EDGE2
    	    y1 = EDGE3;  y2 = EDGE4
	}

	# If any values were specified, then replace them here.
	if (! IS_INDEFR(ux1))
	    x1 = ux1
	if (! IS_INDEFR(ux2))
	    x2 = ux2
	if (! IS_INDEFR(uy1))
	    y1 = uy1
	if (! IS_INDEFR(uy2))
	    y2 = uy2

	# Calculate optimum viewport, as in NCAR's conrec, hafton.
	if (! fill) {
    	    ratio = min (ncolsr, nlinesr) / max (ncolsr, nlinesr)
    	    if (ratio >= ext) {
      	        if (ncols > nlines) 
        	    y2 = (y2 - y1) * nlinesr / ncolsr + y1
      		else 
                    x2 = (x2 - x1) * ncolsr / nlinesr + x1
    	    }
	}

	xdis = x2 - x1
	ydis = y2 - y1
	xcen = (x2 + x1) / 2.
	ycen = (y2 + y1) / 2.

	# So far, the viewport has been calculated so that equal numbers of
	# image pixels map to equal distances in NDC space, regardless of 
	# the aspect ratio of the device.  If the parameter "fill" has been
	# set to no, the user wants to compensate for a non-unity aspect 
	# ratio and make equal numbers of image pixels map to into the same 
	# physical distance on the device, not the same NDC distance.

	if (! fill) {
	    aspect_ratio = ggetr (gp, "ar")
	    if (fp_equalr (aspect_ratio, 0.0))
	        aspect_ratio = 1.0

            if (aspect_ratio < 1.0)
                # Landscape
                xdis = xdis * aspect_ratio
            else if (aspect_ratio > 1.0)
                # Portrait
                ydis = ydis / aspect_ratio
        }

	ux1 = xcen - (xdis / 2.0)
	ux2 = xcen + (xdis / 2.0)
	uy1 = ycen - (ydis / 2.0)
	uy2 = ycen + (ydis / 2.0)

	call gsview (gp, ux1, ux2, uy1, uy2)
	call gswind (gp, c1, c2, l1, l2)
end


# WL_W2LD -- Transform world coordinates to logical coordinates.

procedure wl_w2ld (wlct, flip, wx, wy, lx, ly, npts)

pointer wlct                # I: the MWCS coordinate transformation descriptor
int	flip                # I: true if the axes are transposed
double  wx[npts], wy[npts]  # I: the world coordinates
double  lx[npts], ly[npts]  # O: the logical coordinates
int     npts                # I: the number of points to translate

begin
	if (flip == YES)
	    call mw_v2trand (wlct, wx, wy, ly, lx, npts)
	else
	    call mw_v2trand (wlct, wx, wy, lx, ly, npts)
end


# WL_L2WD -- Transform logical coordinates to world coordinates.

procedure wl_l2wd (lwct, flip, lx, ly, wx, wy, npts)

pointer lwct                # I: the MWCS coordinate transformation descriptor
int	flip                # I: true if the axes are transposed
double  lx[npts], ly[npts]  # I: the logical coordinates
double  wx[npts], wy[npts]  # O: the world coordinates
int     npts                # I: the number of points to translate

begin
	if (flip == YES)
	    call mw_v2trand (lwct, ly, lx, wx, wy, npts)
	else
	    call mw_v2trand (lwct, lx, ly, wx, wy, npts)
end


# WL_MAX_ELEMENT_ARRAY -- Return the index of the maximum array element.
#
# Description
#  This function returns the index of the maximum value of the input array.

int procedure wl_max_element_array (array, npts)

double	array[ARB] 	 # I: the array to look through for the maximum
int	npts             # I: the number of points in the array

int	i, maximum

begin
	maximum = 1
	for (i = 2; i <= npts; i = i + 1)
	    if (array[i] > array[maximum])
		maximum = i

	return (maximum)
end


# WL_DISTANCED - Determine the distance between two points.

double procedure wl_distanced (x1, y1, x2, y2)

double	x1, y1		# I: coordinates of point 1
double	x2, y2		# I: coordinates of point 2

double	a, b

begin
	a = x1 - x2
	b = y1 - y2
	return (sqrt ((a * a) + (b * b)))
end


# WL_DISTANCER -- Determine the distance between two points.

real procedure wl_distancer (x1, y1, x2, y2)

real	x1, y1		# I: coordinates of point 1
real	x2, y2		# I: coordinates of point 2

real a, b

begin
	a = x1 - x2
	b = y1 - y2
	return (sqrt ((a * a) + (b * b)))
end


# The dimensionality.
define N_DIM 2

# Define some memory management.
define ONER Memr[$1+$2-1]

# WL_ROTATE -- Rotate a vector.

procedure wl_rotate (x, y, npts, angle, nx, ny)

real	x[npts], y[npts]    # I: the vectors to rotate
int	npts                # I: the number of points in the vectors
real	angle               # I: the angle to rotate (radians)
real	nx[npts], ny[npts]  # O: the transformed vectors

pointer sp, center, mw
pointer mw_open(), mw_sctran()

begin
	# Get some memory.
	call smark (sp)
	call salloc (center, N_DIM, TY_REAL)

	mw = mw_open (NULL, N_DIM)
	ONER(center,1) = 0.
	ONER(center,2) = 0.
	call mw_rotate (mw, -DEGTORAD( angle ), ONER(center,1), 3b)
	call mw_v2tranr (mw_sctran (mw, "physical", "logical", 3b),
            x, y, nx, ny, npts)

	call mw_close (mw)
	call sfree (sp)
end
