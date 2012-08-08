# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

#### load2.x (from load.x) ####

include <mach.h>
include <imset.h>
include <imhdr.h>
include	<error.h>
include	<gki.h>
include <fio.h>
include <fset.h>
include	"gwindow.h"
include	"../lib/ids.h"
include	"cv.h"

# DS_LOAD_DISPLAY -- Map an image into the display window.  In general this
#   involves independent linear transformations in the X, Y, and Z (greyscale)
#   dimensions.  If a spatial dimension is larger than the display window then
#   the image is block averaged.  If a spatial dimension or a block averaged
#   dimension is smaller than the display window then linear interpolation is
#   used to expand the image.  Both the input image and the output device appear
#   to us as images, accessed via IMIO.
#
# World coordinate system 0 (WCS 0) defines the position and size of the device
#   window in NDC coordinates (0-1 in either axis).  WCS 1 assigns a pixel
#   coordinate system to the same window.  If we convert the NDC coordinates of
#   the window into device coordinates in pixels, then the ratios of the window
#   coordinates in pixels to the image coordinates in pixels defines the real
#   magnification factors for the two spatial axes.  If the pixel coordinates
#   are out of bounds then the image will be displayed centered in the window
#   with zero fill at the edges.  If the frame has not been erased then the fill
#   areas must be explicitly zeroed.

procedure ds_load_display (im, wdes, border_erase)

pointer	im			# input image
pointer	wdes			# graphics window descriptor
bool	border_erase

int	wx1, wx2, wy1, wy2	# device window to be filled with image data
real	px1, px2, py1, py2	# image coords in fractional image pixels
real	pxsize, pysize		# size of image section in fractional pixels
real	wxcenter, wycenter	# center of device window in frac device pixels
real	xmag, ymag		# x,y magnification ratios
pointer	w0, w1			# world coord systems 0 (NDC) and 1 (pixel)

include	"cv.com"

begin
	# Compute pointers to WCS 0 and 1.
	w0 = W_WC(wdes,0)
	w1 = W_WC(wdes,1)

	# Compute X and Y magnification ratios required to map image into
	# the device window in device pixel units.

	xmag = (W_XE(w0) - W_XS(w0)) * cv_xres / (W_XE(w1) - W_XS(w1))
	ymag = (W_YE(w0) - W_YS(w0)) * cv_yres / (W_YE(w1) - W_YS(w1))

	# Compute the coordinates of the image section to be displayed.
	# This is not necessarily the same as WCS 1 since the WCS coords
	# need not be inbounds.

	px1 = max (1.0,		 W_XS(w1))
	px2 = min (real (IM_LEN(im,1)), W_XE(w1))
	py1 = max (1.0,		 W_YS(w1))
	py2 = min (real (IM_LEN(im,2)), W_YE(w1))

	# Now compute the coordinates of the image section to be written in
	# device pixel units.  This section must lie within or on the device
	# window.
	# This computation for I2S will give 257, which does differ by one
	# for the Y center (due to inversion in I2S).  This should not matter,
	# but if it does, this comment will change!

	pxsize = px2 - px1
	pysize = py2 - py1
	wxcenter = (W_XE(w0) + W_XS(w0)) / 2.0 * cv_xres + 1
	wycenter = (W_YE(w0) + W_YS(w0)) / 2.0 * cv_yres + 1

	wx1 = max (1, int (wxcenter - (pxsize / 2.0 * xmag)))
	wx2 = max (wx1, min (cv_xres, int (wx1 + (pxsize * xmag))))
	wy1 = max (1, int (wycenter - (pysize / 2.0 * ymag)))
	wy2 = max (wy1, min (cv_yres, int (wy1 + (pysize * ymag))))

	# Display the image data, ignoring zero filling at the boundaries.

	call ds_map_image (im, px1,px2,py1,py2, wx1,wx2,wy1,wy2,
	    W_ZS(w1), W_ZE(w1), W_ZT(w1), W_UPTR(w1))

	# Zero the border of the window if the frame has not been erased,
	# and if the displayed section does not occupy the full window.

	if (border_erase)
	    call ds_erase_border (im, wdes, wx1,wx2,wy1,wy2)
end


# DS_MAP_IMAGE -- Map an image section from the input image to a section
# (window) of the output image (the display device).  All spatial scaling is 
# handled by the "scaled input" package, i.e., SIGL2[SR].  Our task is to
# get lines from the scaled input image, transform the greyscale if necessary,
# and write the lines to the output device.

procedure ds_map_image (im, px1,px2,py1,py2, wx1,wx2,wy1,wy2, z1,z2,zt, uptr)

pointer	im			# input image
real	px1,px2,py1,py2		# input section
int	wx1,wx2,wy1,wy2		# output section
real	z1,z2			# range of input greylevels to be mapped.
int	zt			# log or linear greylevel transformation
pointer uptr			# pointer to user transformation table

bool	unitary_greyscale_transformation
short	lut1, lut2, z1_s, z2_s, dz1_s, dz2_s
real	dz1, dz2
int	wy, nx, ny, xblk, yblk
pointer	in, out, si
pointer	sigl2s(), sigl2r(), sigl2_setup()
errchk	sigl2s, sigl2r, sigl2_setup
real	xs, xe, y
pointer	sp, outr
bool    fp_equalr()
real	if_elogr()
extern	if_elogr

include "cv.com"

begin
	call smark (sp)

	# Set up for scaled image input.

	nx = wx2 - wx1 + 1
	ny = wy2 - wy1 + 1
	xblk = INDEFI
	yblk = INDEFI
	si = sigl2_setup (im, px1,px2,nx,xblk, py1,py2,ny,yblk)

	# Output array, and limiting x values in NDC

	call salloc (out, nx, TY_SHORT)
	xs = real(wx1 - 1) * cv_xcon / GKI_MAXNDC
	# Don't subtract 1 from wx2 as we want it to be first one not filled
	xe = real(wx2) * cv_xcon / GKI_MAXNDC
	if ( xe > 1.0)
	    xe = 1.0

	# The device ZMIN and ZMAX parameters define the acceptable range
	# of greyscale values for the output device (e.g., 0-255 for most 8-bit
	# display devices).  For the general display, we use 0 and the
	# device "z" resolution.  Values Z1 and Z2 are mapped linearly or
	# logarithmically into these.

	dz1 = 0
	dz2 = cv_zres-1

	# If the user specified the transfer function, see that the
	# intensity and greyscale values are in range.

	if (zt == W_USER) {
	    call alims (Mems[uptr], SZ_BUF, lut1, lut2)
	    dz1_s = short (dz1)
	    dz2_s = short (dz2)
	    if (lut2 < dz1_s || lut1 > dz2_s)
		call eprintf ("User specified greyscales out of range\n")
	    if (z2 < IM_MIN(im) || z1 > IM_MAX(im))
		call eprintf ("User specified intensities out of range\n")
	}

	# Type short pixels are treated as a special case to minimize vector
	# operations for such images (which are common).  If the image pixels
	# are either short or real then only the ALTR (greyscale transformation)
	# vector operation is required.  The ALTR operator linearly maps
	# greylevels in the range Z1:Z2 to DZ1:DZ2, and does a floor ceiling
	# of DZ1:DZ2 on all pixels outside the range.  If unity mapping is
	# employed the data is simply copied, i.e., floor ceiling constraints
	# are not applied.  This is very fast and will produce a contoured
	# image on the display which will be adequate for some applications.

	if (zt == W_UNITARY)
	    unitary_greyscale_transformation = true
	else
	    unitary_greyscale_transformation =
		(fp_equalr (dz1,z1) && fp_equalr (dz2,z2)) || fp_equalr (z1,z2)

	if (IM_PIXTYPE(im) == TY_SHORT && zt != W_LOG) {

	    # Set dz1_s and dz2_s depending on transformation
	    if (zt != W_USER) {
	        dz1_s = short (dz1)
	        dz2_s = short (dz2)
	    } else {
	        dz1_s = short (STARTPT)
	        dz2_s = short (ENDPT)
	    }
	    z1_s = short (z1)
	    z2_s = short (z2)

	    for (wy=wy1;  wy <= wy2;  wy=wy+1) {
		in  = sigl2s (si, wy - wy1 + 1)
		y = real(wy-1) * cv_ycon / GKI_MAXNDC
		if (unitary_greyscale_transformation)
		    call gpcell (cv_gp, Mems[in], nx, 1, xs, y, xe, y) 
		else if (zt == W_USER) {
		    call amaps (Mems[in], Mems[out], nx, z1_s,z2_s, dz1_s,dz2_s)
		    call aluts (Mems[out], Mems[out], nx, Mems[uptr])
		    call gpcell (cv_gp, Mems[out], nx, 1, xs, y, xe, y) 
		} else {
		    call amaps (Mems[in], Mems[out], nx, z1_s,z2_s, dz1_s,dz2_s)
		    call gpcell (cv_gp, Mems[out], nx, 1, xs, y, xe, y) 
		}
	    }
	} else {
	    call salloc (outr, nx, TY_REAL)
	    for (wy=wy1;  wy <= wy2;  wy=wy+1) {
		in  = sigl2r (si, wy - wy1 + 1)
		y = real(wy - 1) * cv_ycon / GKI_MAXNDC

		if (zt == W_LOG) {
		    call amapr (Memr[in], Memr[outr], nx,
			z1, z2, 1.0, 10.0 ** MAXLOG)
		    call alogr (Memr[outr], Memr[outr], nx, if_elogr)
		    call amapr (Memr[outr], Memr[outr], nx,
			1.0, real(MAXLOG), dz1, dz2)
		    call achtrs (Memr[outr], Mems[out], nx)
		} else if (unitary_greyscale_transformation) {
		    call achtrs (Memr[in], Mems[out], nx)
		} else if (zt == W_USER) {
		    call amapr (Memr[in], Memr[outr], nx, z1,z2, STARTPT,ENDPT) 
		    call achtrs (Memr[outr], Mems[out], nx)
		    call aluts (Mems[out], Mems[out], nx, Mems[uptr])
		} else {
		    call amapr (Memr[in], Memr[outr], nx, z1, z2, dz1, dz2)
		    call achtrs (Memr[outr], Mems[out], nx)
		}
		call gpcell (cv_gp, Mems[out], nx, 1, xs, y, xe, y)
	    }
	}

	call sfree (sp)
	call sigl2_free (si)
end


# DS_ERASE_BORDER -- Zero the border of the window if the frame has not been
# erased, and if the displayed section does not occupy the full window.
# It would be more efficient to do this while writing the greyscale data to
# the output image, but that would complicate the display procedures and frames
# are commonly erased before displaying an image.

procedure ds_erase_border (im, wdes, wx1,wx2,wy1,wy2)

pointer	im			# input image
pointer	wdes			# window descriptor
int	wx1,wx2,wy1,wy2		# section of display window filled by image data

int	dx1,dx2,dy1,dy2		# coords of full display window in device pixels
int	j, n, n1
pointer	w0
pointer	sp, zero
real	xls, xle, xrs, xre, y

include	"cv.com"

begin
	call smark (sp)
	call salloc (zero, cv_xres, TY_SHORT)
	call aclrs (Mems[zero], cv_xres)

	# Compute device pixel coordinates of the full display window.
	w0 = W_WC(wdes,0)
	dx1 = W_XS(w0) * (cv_xres - 1) + 1
	dx2 = W_XE(w0) * (cv_xres - 1) + 1
	dy1 = W_YS(w0) * (cv_yres - 1) + 1
	dy2 = W_YE(w0) * (cv_yres - 1) + 1

	# Determine left and right (exclusive), start and end, x values in NDC
	# for pixels not already filled.
	# If, say, dx1 < wx1, we want to clear dx1 through wx1-1, which means
	# that for gpcell, we want the (right) end points to be the first
	# pixel not cleared.
	xls = real(dx1 - 1) * cv_xcon / GKI_MAXNDC
	xle = real(wx1) * cv_xcon / GKI_MAXNDC
	if (xle > 1.0)
	    xle = 1.0
	xre = real(dx2 - 1) * cv_xcon / GKI_MAXNDC
	xrs = real(wx2) * cv_xcon / GKI_MAXNDC
	if (xre > 1.0)
	    xre = 1.0

	# Erase lower margin.
	n = dx2 - dx1 + 1
	for (j=dy1;  j < wy1;  j=j+1) {
	    y = real(j-1) * cv_ycon / GKI_MAXNDC
	    call gpcell (cv_gp, Mems[zero], n, 1, xls, y, xre, y)
	}

	# Erase left and right margins.  By doing the right margin of a line
	# immediately after the left margin we have a high liklihood that the
	# display line will still be in the FIO buffer.

	n = wx1 - dx1
	n1 = dx2 - wx2
	for (j=wy1;  j <= wy2;  j=j+1) {
	    y = real(j-1) * cv_ycon / GKI_MAXNDC
	    if (dx1 < wx1)
		call gpcell (cv_gp, Mems[zero], n, 1, xls, y, xle, y)
	    if (wx2 < dx2)
		call gpcell (cv_gp, Mems[zero], n1, 1, xrs, y, xre, y)
	}

	# Erase upper margin.
	n = dx2 - dx1 + 1
	for (j=wy2+1;  j <= dy2;  j=j+1) {
	    y = real(j-1) * cv_ycon / GKI_MAXNDC
	    call gpcell (cv_gp, Mems[zero], n, 1, xls, y, xre, y)
	}

	call sfree (sp)
end


# IF_ELOG -- The error function for log10. Note that MAX_EXPONENT is
# currently an integer so it is converted to the appropriate data type
# before being returned.

real procedure if_elogr (x)

real   x                               # the input pixel value

begin
        return (real(-MAX_EXPONENT))
end

