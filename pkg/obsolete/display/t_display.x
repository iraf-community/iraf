# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include <imset.h>
include <imhdr.h>
include	<error.h>
include	"display.h"
include	"gwindow.h"
include	"iis.h"

# DISPLAY - Display an image.  The specified image section is mapped into
# the specified section of an image display frame.  The mapping involves
# a linear transformation in X and Y and a linear or logarithmic transformation
# in Z (greyscale).  Images of all pixel datatypes are supported, and there
# no upper limit on the size of an image.  The display device is interfaced
# to FIO as a file and is accessed herein via IMIO as just another imagefile.
# The physical characteristics of the display (i.e., X, Y, and Z resolution)
# are taken from the image header.  The display frame buffer is the pixel
# storage "file".

procedure t_display()

int	frame
char	image[SZ_FNAME]
int	chan[MAXCHAN], i
bool	frame_erase, border_erase
pointer	im, ds, wdes, sp, w

bool	clgetb()
int	clgeti()
pointer	immap(), dsmap()
errchk	immap, dsmap, imunmap, ds_getparams
include	"iis.com"

begin
	call smark (sp)
	call salloc (wdes, LEN_WDES, TY_STRUCT)
	call aclri (Memi[wdes], LEN_WDES)

	# Open input imagefile.
	call clgstr ("image", image, SZ_FNAME)
	im = immap (image, READ_ONLY, 0)

	if (IM_NDIM(im) <= 0)
	    call error (1, "image has no pixels")

	# Open display device as an image.
	frame = clgeti ("frame")
	call iis_setframe (frame)

	frame_erase = clgetb ("erase")
	if (frame_erase)
	    ds = dsmap (frame, WRITE_ONLY, MONO, chan)
	else
	    ds = dsmap (frame, READ_WRITE, MONO, chan)

	# Pick up the frame size and other device parameters.
	iis_xdim   = IM_LEN(ds,1)
	iis_ydim   = IM_LEN(ds,2)
	iis_config = IM_LEN(ds,3)	# frame buffer configuration number
	iis_server = IM_LEN(ds,4)	# device is actually a display server

	# Optimize for sequential i/o.
	call imseti (im, IM_ADVICE, SEQUENTIAL)
	call imseti (ds, IM_ADVICE, SEQUENTIAL)

	# Get display parameters and set up transformation.
	call ds_getparams (im, ds, wdes, image, frame)

	# Compute and output the screen to image pixel WCS.
	call ds_setwcs (im, ds, wdes, image, frame, chan)

	# Erase frame before loading?
	if (frame_erase)
	    call zersim (chan)

	# Display frame being loaded?
	if (clgetb ("select_frame"))
	    call zfrmim (chan)

	# Erase the border (space between displayed image section and edge of
	# window) only if screen was not erased and border erasing is enabled.

	if (frame_erase)
	    border_erase = false
	else
	    border_erase = clgetb ("border_erase")

	# Display the image.
	call ds_load_display (im, ds, wdes, border_erase)

	# Free storage.
	do i = 0, W_MAXWC {
	    w = W_WC(wdes,i)
	    if (W_UPTR(w) != NULL)
		call ds_ulutfree (W_UPTR(w))
	}

	call imunmap (im)
	call imunmap (ds)

	call sfree (sp)
end


# DS_GETPARAMS -- Get the parameters controlling how the image is mapped
# into the display frame.  Set up the transformations and save in the graphics
# descriptor file.  If "repeat" mode is enabled, read the graphics descriptor
# file and reuse the transformations therein.

procedure ds_getparams (im, ds, wdes, image, frame)

pointer	im, ds, wdes		# Image, display, and graphics descriptors
char	image[SZ_FNAME]		# Should be determined from im
int	frame			# Should be determined from ds

bool	fill, zscale_flag, zrange_flag, zmap_flag
real	xcenter, ycenter
real	xsize, ysize, pxsize, pysize
real	xmag, ymag, xscale, yscale
real	z1, z2, contrast
int	nsample_lines, ncols, nlines, len_stdline, order
pointer	w, sp, ztrans, lutfile

int	clgeti()
real	clgetr()
pointer	ds_ulutalloc()
bool	streq(), clgetb()

begin
	call smark (sp)
	call salloc (ztrans, SZ_FNAME, TY_CHAR)

	# Set up a new graphics descriptor structure defining the coordinate
	# transformation used to map the image into the display frame.

	call strcpy (image, W_IMSECT(wdes), W_SZIMSECT)
	ncols  = IM_LEN(im,1)
	nlines = IM_LEN(im,2)

	# The fill, zscale, and zrange parameters determine the algorithms to
	# be used to scale the image in the spatial and greyscale dimensions.
	# If greyscale mapping is disabled the zscale and zrange options are
	# disabled.  Greyscale mapping can also be disabled by turning off
	# zscale and zrange and setting Z1 and Z2 to the device greyscale min
	# and max values, producing a unitary transformation.

	fill = clgetb ("fill")
	call clgstr ("ztrans", Memc[ztrans], SZ_FNAME)
	if (streq (Memc[ztrans], "none") || streq (Memc[ztrans], "user")) {
	    zscale_flag = false
	    zrange_flag = false
	    zmap_flag   = false
	} else {
	    zmap_flag = true
	    zscale_flag = clgetb ("zscale")
	    if (!zscale_flag)
		zrange_flag = clgetb ("zrange")
	}

	# Determine Z1 and Z2, the range of input greylevels to be mapped into
	# the fixed range of display greylevels.

	if (zscale_flag) {
	    # Autoscaling is desired.  Compute Z1 and Z2 which straddle the
	    # median computed by sampling a portion of the image.

	    contrast = clgetr ("contrast")
	    nsample_lines = clgeti ("nsample_lines")
	    len_stdline = SAMPLE_SIZE / nsample_lines
	    call zscale (im, z1, z2, contrast, SAMPLE_SIZE, len_stdline)

	} else if (zrange_flag) {
	    nsample_lines = clgeti ("nsample_lines")
	    call maxmin (im, z1, z2, nsample_lines)

	} else if (zmap_flag) {
	    z1 = clgetr ("z1")
	    z2 = clgetr ("z2")
	} else {
	    z1 = IM_MIN(ds)
	    z2 = IM_MAX(ds)
	}

	# Determine the display window into which the image is to be mapped
	# in normalized device coordinates.

	xcenter = max(0.0, min(1.0, clgetr ("xcenter")))
	ycenter = max(0.0, min(1.0, clgetr ("ycenter")))
	xsize   = max(0.0, min(1.0, clgetr ("xsize")))
	ysize   = max(0.0, min(1.0, clgetr ("ysize")))
	order   = max(0, min(1, clgeti ("order")))

	# Determine X and Y scaling ratios required to map the image into the
	# normalized display window.  If spatial scaling is not desired filling
	# must be disabled and XMAG and YMAG must be set to 1.0 in the
	# parameter file.  Fill mode will always produce an aspect ratio of 1;
	# if nonequal scaling is required then the magnification ratios must
	# be set explicitly by the user.

	if (fill) {
	    # Compute scale in units of window coords per data pixel required
	    # to scale image to fit window.

	    xmag = ((IM_LEN(ds,1) - 1) * xsize) / max (1, (ncols  - 1))
	    ymag = ((IM_LEN(ds,2) - 1) * ysize) / max (1, (nlines - 1))

	    if (xmag > ymag)
		xmag = ymag
	    else
		ymag = xmag

	} else {
	    # Compute scale required to provide image magnification ratios
	    # specified by the user.  Magnification is specified in units of
	    # display pixels, i.e, a magnification ratio of 1.0 means that
	    # image pixels will map to display pixels without scaling.

	    xmag = clgetr ("xmag")
	    ymag = clgetr ("ymag")
	}

	xscale = 1.0 / ((IM_LEN(ds,1) - 1) / xmag)
	yscale = 1.0 / ((IM_LEN(ds,2) - 1) / ymag)

	# Set device window limits in normalized device coordinates.
	# World coord system 0 is used for the device window.

	w = W_WC(wdes,0)
	W_XS(w) = xcenter - xsize / 2.0
	W_XE(w) = xcenter + xsize / 2.0
	W_YS(w) = ycenter - ysize / 2.0
	W_YE(w) = ycenter + ysize / 2.0

	# Set pixel coordinates of window, world coordinate system #1.

	w = W_WC(wdes,1)
	pxsize = xsize / xscale
	pysize = ysize / yscale

	# If the image is too large to fit in the window given the scaling
	# factors XSCALE and YSCALE, the following will set starting and ending
	# pixel coordinates in the interior of the image.  If the image is too
	# small to fill the window then the pixel coords will reference beyond
	# the bounds of the image.

	W_XS(w) = (ncols  - 1) / 2.0 + 1 - (pxsize / 2.0)
	W_XE(w) = W_XS(w) + pxsize
	W_YS(w) = (nlines - 1) / 2.0 + 1 - (pysize / 2.0)
	W_YE(w) = W_YS(w) + pysize

	# Order of interpolator used for spatial transformation.
	W_XT(w) = order
	W_YT(w) = order

	# Determine whether a log or linear greyscale transformation is
	# desired.

	if (streq (Memc[ztrans], "log")) {
	    W_ZT(w) = W_LOG
	} else if (streq (Memc[ztrans], "linear")) {
	    W_ZT(w) = W_LINEAR
	} else if (streq (Memc[ztrans], "none")) {
	    W_ZT(w) = W_UNITARY
	} else if (streq (Memc[ztrans], "user")) {
	    W_ZT(w) = W_USER
	    call salloc (lutfile, SZ_FNAME, TY_CHAR)
	    call clgstr ("lutfile", Memc[lutfile], SZ_FNAME)
	    W_UPTR(w) = ds_ulutalloc (Memc[lutfile], z1, z2)
	} else {
	    call eprintf ("Bad greylevel transformation '%s'\n")
		call pargstr (Memc[ztrans])
	    W_ZT(w) = W_LINEAR
	}

	# Set up the greyscale transformation.
	W_ZS(w) = z1
	W_ZE(w) = z2

	call printf ("z1=%g z2=%g\n")
	    call pargr (z1)
	    call pargr (z2)
	call flush (STDOUT)

	# The user world coordinate system should be set from the CTRAN
	# structure in the image header, but for now we just make it equal
	# to the pixel coordinate system.

	call amovi (Memi[w], Memi[W_WC(wdes,2)], LEN_WC)
	W_UPTR(W_WC(wdes,2)) = NULL		# should not copy pointers!!
	call sfree (sp)
end


# DS_SETWCS -- Compute the rotation matrix needed to convert screen coordinates
# (zero indexed, y-flipped) to image pixel coordinates, allowing both for the
# transformation from screen space to the image section being displayed, and
# from the image section to the physical input image.
#
# NOTE -- This code assumes that the display device is zero-indexed and
# y-flipped; this is usually the case, but should be parameterized in the
# graphcap.  This code also assumes that the full device screen is being used,
# and that we are not assigning multiple WCS to different regions of the screen.

procedure ds_setwcs (im, ds, wdes, image, frame, chan)

pointer	im, ds, wdes		# image, display, and coordinate descriptors
char	image[SZ_FNAME]		# image section name
int	frame
int	chan

real	a, b, c, d, tx, ty
int	wcsfile, ip, i, j, axis[2]
pointer	w0, w1, sp, title, dir, fname, ftemp, device, textbuf, imname
long	lv[IM_MAXDIM], pv1[IM_MAXDIM], pv2[IM_MAXDIM]
int	envfind(), open(), strncmp()
include	"iis.com"

begin
	call smark (sp)
	call salloc (title, SZ_LINE, TY_CHAR)
	call salloc (dir, SZ_PATHNAME, TY_CHAR)
	call salloc (imname, SZ_FNAME, TY_CHAR)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)
	call salloc (ftemp, SZ_PATHNAME, TY_CHAR)
	call salloc (device, SZ_FNAME, TY_CHAR)
	call salloc (textbuf, SZ_WCSTEXT, TY_CHAR)

	# Compute the rotation matrix needed to transform screen pixel coords
	# to image section coords.

	w0 = W_WC(wdes,0)
	w1 = W_WC(wdes,1)

	# X transformation.
	a  = (W_XE(w1) - W_XS(w1)) / ((W_XE(w0) - W_XS(w0)) * (IM_LEN(ds,1)-1))
	c  = 0.0			# not rotated, cross term is zero
	tx = W_XS(w1) - a * (W_XS(w0) * IM_LEN(ds,1))

	# Y transformation.
	b  = 0.0			# not rotated, cross term is zero
	d  = (W_YE(w1) - W_YS(w1)) / ((W_YE(w0) - W_YS(w0)) * (IM_LEN(ds,2)-1))
	ty = W_YS(w1)

	# Now allow for the Y-flip (origin at upper left in display window).
	d = -d
	ty = W_YE(w1) + (-d) * ((1.0 - W_YE(w0)) * IM_LEN(ds,2))

	# Get the physical image coordinates of the two opposite corners of
	# the image section being displayed.  This ignores questions of images
	# with dimensionality greater than 2, which are better dealt with
	# elsewhere.

	lv[1] = 1;  lv[2] = 1
	call imaplv (im, lv, pv1, 2)

	lv[1] = 2;  lv[2] = 2
	call imaplv (im, lv, pv2, 2)

	# Determine which physical axes contribute to the image section.
	i = 1
	axis[1] = 1;  axis[2] = 2
	do j = 1, IM_MAXDIM
	    if (pv1[j] != pv2[j]) {
		axis[i] = j
		i = i + 1
	    }

	# These imply a new rotation matrix which we won't bother to work out
	# separately here.  Multiply the two rotation matrices and add the
	# translation vectors to get the overall transformation from screen
	# coordinates to image coordinates.

	a = a * (pv2[axis[1]] - pv1[axis[1]])
	d = d * (pv2[axis[2]] - pv1[axis[2]])

	lv[1] = nint(tx);  lv[2] = nint(ty)
	call imaplv (im, lv, pv2, 2)

	tx = pv2[axis[1]]
	ty = pv2[axis[2]]

	# Construct the WCS filename, "dir$device_frame.wcs".

	if (envfind ("wcsdir", Memc[dir], SZ_PATHNAME) <= 0)
	    if (envfind ("WCSDIR", Memc[dir], SZ_PATHNAME) <= 0)
		if (envfind ("uparm", Memc[dir], SZ_PATHNAME) <= 0)
		    call strcpy ("tmp$", Memc[dir], SZ_PATHNAME)

	if (envfind ("stdimage", Memc[device], SZ_FNAME) <= 0)
	    call strcpy ("display", Memc[device], SZ_FNAME)

	# Get a temporary file in the WCS directory.
	call sprintf (Memc[ftemp], SZ_PATHNAME, "%swcs")
	    call pargstr (Memc[dir])
	call mktemp (Memc[ftemp], Memc[ftemp], SZ_PATHNAME)

	# Get the final WCS file filename.
	call sprintf (Memc[fname], SZ_PATHNAME, "%s%s_%d.wcs")
	    call pargstr (Memc[dir])
	    if (strncmp (Memc[device], "imt", 3) == 0)
		call pargstr ("imtool")
	    else
		call pargstr (Memc[device])
	    call pargi (frame)

	# Get the image title string minus any newline.
	call strcpy (IM_TITLE(im), Memc[title], SZ_LINE)
	for (ip=title;  Memc[ip] != '\n' && Memc[ip] != EOS;  ip=ip+1)
	    ;
	Memc[ip] = EOS

	# Get the image name minus any image section.
	call imgimage (image, Memc[imname], SZ_FNAME)

	# Format the WCS text.
	call sprintf (Memc[textbuf], SZ_WCSTEXT,
	    "%s - %s\n%g %g %g %g %g %g %g %g %d\n")
	    call pargstr (Memc[imname])
	    call pargstr (Memc[title])
	    call pargr (a)
	    call pargr (b)
	    call pargr (c)
	    call pargr (d)
	    call pargr (tx)
	    call pargr (ty)
	    call pargr (W_ZS(w1))
	    call pargr (W_ZE(w1))
	    call pargi (W_ZT(w1))

	# If we are writing to a display server (device has the logical
	# cursor capability), output the WCS text via the datastream,
	# else use a text file.  The datastream set-WCS is also used to
	# pass the frame buffer configuration to server devices.

	if (iis_server == YES) {
	    call imd_setwcs (chan, Memc[textbuf])
	} else {
	    # Update the WCS file.
	    iferr (wcsfile = open (Memc[ftemp], TEMP_FILE, TEXT_FILE)) {
		call erract (EA_WARN)
	    } else {
		# Now delete the old file, if any, and write the new one.
		# To avoid process race conditions, create the new file as an
		# atomic operation, first writing a new file and then renaming
		# it to create the WCS file.

		iferr (call delete (Memc[fname]))
		    ;

		# Output the file version.
		call putline (wcsfile, Memc[textbuf])
		call close (wcsfile)

		# Install the new file.
		iferr (call rename (Memc[ftemp], Memc[fname]))
		    call erract (EA_WARN)
	    }
	}

	call sfree (sp)
end


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

procedure ds_load_display (im, ds, wdes, border_erase)

pointer	im			# input image
pointer	ds			# output image
pointer	wdes			# graphics window descriptor
bool	border_erase

int	order			# order of spatial interpolator
int	wx1, wx2, wy1, wy2	# device window to be filled with image data
real	px1, px2, py1, py2	# image coords in fractional image pixels
real	pxsize, pysize		# size of image section in fractional pixels
real	wxcenter, wycenter	# center of device window in frac device pixels
real	xmag, ymag		# x,y magnification ratios
pointer	w0, w1			# world coord systems 0 (NDC) and 1 (pixel)

begin
	# Compute pointers to WCS 0 and 1.
	w0 = W_WC(wdes,0)
	w1 = W_WC(wdes,1)

	# Compute X and Y magnification ratios required to map image into
	# the device window in device pixel units.

	xmag = (W_XE(w0) - W_XS(w0)) * IM_LEN(ds,1) / (W_XE(w1) - W_XS(w1))
	ymag = (W_YE(w0) - W_YS(w0)) * IM_LEN(ds,2) / (W_YE(w1) - W_YS(w1))

	# Compute the coordinates of the image section to be displayed.
	# This is not necessarily the same as WCS 1 since the WCS coords
	# need not be inbounds.

	px1 = max (1,		 int(W_XS(w1)))
	px2 = min (IM_LEN(im,1), int(W_XE(w1)))
	py1 = max (1,		 int(W_YS(w1)))
	py2 = min (IM_LEN(im,2), int(W_YE(w1)))

	# Now compute the coordinates of the image section to be written in
	# device pixel units.  This section must lie within or on the device
	# window.

	pxsize = px2 - px1
	pysize = py2 - py1
	wxcenter = (W_XE(w0) + W_XS(w0)) / 2.0 * IM_LEN(ds,1) + 1
	wycenter = (W_YE(w0) + W_YS(w0)) / 2.0 * IM_LEN(ds,2) + 1

	wx1 = max (  1, int (wxcenter - (pxsize / 2.0 * xmag)))
	wx2 = max (wx1, min (IM_LEN(ds,1), int (wx1 + (pxsize * xmag))))
	wy1 = max (  1, int (wycenter - (pysize / 2.0 * ymag)))
	wy2 = max (wy1, min (IM_LEN(ds,2), int (wy1 + (pysize * ymag))))

	order = max (W_XT(w1), W_YT(w1))

#call eprintf ("px1=%g px2=%g py1=%g py2=%g xmag=%g ymag=%g\n")
#call pargr(px1); call pargr(px2)
#call pargr(py1); call pargr(py2)
#call pargr(xmag); call pargr(ymag)
#call eprintf ("wx1=%d wx2=%d wy1=%d wy2=%d wxcen=%g wycen=%g\n")
#call pargi(wx1); call pargi(wx2)
#call pargi(wy1); call pargi(wy2)
#call pargr(wxcenter); call pargr(wycenter)
#call eprintf ("xs=%g xe=%g ys=%g ye=%g xlen=%d ylen=%d\n")
#call pargr(W_XS(w0)); call pargr(W_XE(w0))
#call pargr(W_YS(w0)); call pargr(W_YE(w0))
#call pargi(IM_LEN(ds,1)); call pargi(IM_LEN(ds,2))

	# Display the image data, ignoring zero filling at the boundaries.

	call ds_map_image (im, ds, px1,px2,py1,py2, wx1,wx2,wy1,wy2,  order,
	    W_ZS(w1), W_ZE(w1), W_ZT(w1), W_UPTR(w1))

	# Zero the border of the window if the frame has not been erased,
	# and if the displayed section does not occupy the full window.

	if (border_erase)
	    call ds_erase_border (im, ds, wdes, wx1,wx2,wy1,wy2)
end


# DS_MAP_IMAGE -- Map an image section from the input image to a section
# (window) of the output image (the display device).  All spatial scaling is 
# handled by the "scaled input" package, i.e., SIGL2[SR].  Our task is to
# get lines from the scaled input image, transform the greyscale if necessary,
# and write the lines to the output device.

procedure ds_map_image (im,ds, px1,px2,py1,py2, wx1,wx2,wy1,wy2, order
	z1,z2,zt,uptr)

pointer	im			# input image
pointer	ds			# output image
real	px1,px2,py1,py2		# input section
int	wx1,wx2,wy1,wy2		# output section
int	order			# interpolator order (0=replicate, 1=linear)
real	z1,z2			# range of input greylevels to be mapped.
int	zt			# log or linear greylevel transformation
pointer	uptr			# pointer to user transformation table

real	dz1, dz2
int	wy, nx, ny, xblk, yblk
pointer	sp, in, out, si, rtemp
bool	unitary_greyscale_transformation
short	lut1, lut2, dz1_s, dz2_s, z1_s, z2_s

bool	fp_equalr()
pointer	imps2s(), imps2r(), sigl2s(), sigl2r(), sigl2_setup()
errchk	imps2s, imps2r, sigl2s, sigl2r, sigl2_setup

begin
	# Set up for scaled image input.
	nx = wx2 - wx1 + 1
	ny = wy2 - wy1 + 1
	xblk = INDEFI
	yblk = INDEFI
	si = sigl2_setup (im, px1,px2,nx,xblk, py1,py2,ny,yblk, order)

	# The device IM_MIN and IM_MAX parameters define the acceptable range
	# of greyscale values for the output device (e.g., 0-255 for most 8-bit
	# display devices).  Values Z1 and Z2 are mapped linearly or
	# logarithmically into IM_MIN and IM_MAX.

	dz1 = IM_MIN(ds)
	dz2 = IM_MAX(ds)

	# If the user specifies the transfer function, verify that the
	# intensity and greyscale are in range.

	if (zt == W_USER) {
	    call alims (Mems[uptr], U_MAXPTS, lut1, lut2)
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

	if (zt == W_UNITARY) {
	    unitary_greyscale_transformation = true
	} else if (zt == W_LINEAR) {
	    unitary_greyscale_transformation =
		((fp_equalr(z1,dz1) && fp_equalr(z2,dz2)) || fp_equalr(z1,z2))
	} else
	    unitary_greyscale_transformation = false

	if (IM_PIXTYPE(im) == TY_SHORT && zt != W_LOG) {
	    z1_s = z1;  z2_s = z2

	    for (wy=wy1;  wy <= wy2;  wy=wy+1) {
		in  = sigl2s (si, wy - wy1 + 1)
		out = imps2s (ds, wx1, wx2, wy, wy)

		if (unitary_greyscale_transformation) {
		    call amovs (Mems[in], Mems[out], nx)
		} else if (zt == W_USER) {
		    dz1_s = U_Z1; dz2_s = U_Z2
		    call amaps (Mems[in],Mems[out],nx, z1_s,z2_s, dz1_s,dz2_s)
		    call aluts (Mems[out], Mems[out], nx, Mems[uptr])
		} else {
		    dz1_s = dz1; dz2_s = dz2
		    call amaps (Mems[in],Mems[out],nx, z1_s,z2_s, dz1_s,dz2_s)
		}
	    }

	} else if (zt == W_USER) {
	    call smark (sp)
	    call salloc (rtemp, nx, TY_REAL)

	    for (wy=wy1;  wy <= wy2;  wy=wy+1) {
		in  = sigl2r (si, wy - wy1 + 1)
		out = imps2s (ds, wx1, wx2, wy, wy)

		call amapr (Memr[in], Memr[rtemp], nx, z1, z2, 
		    real(U_Z1), real(U_Z2))
		call achtrs (Memr[rtemp], Mems[out], nx)
		call aluts (Mems[out], Mems[out], nx, Mems[uptr])
	    }

	    call sfree (sp)

	} else {
	    for (wy=wy1;  wy <= wy2;  wy=wy+1) {
		in  = sigl2r (si, wy - wy1 + 1)
		out = imps2r (ds, wx1, wx2, wy, wy)

		if (unitary_greyscale_transformation)  {
		    call amovr (Memr[in], Memr[out], nx)
		} else if (zt == W_LOG) {
		    call amapr (Memr[in], Memr[out], nx,
			z1, z2, 1.0, 10.0 ** MAXLOG)
		    call alogr (Memr[out], Memr[out], nx)
		    call amapr (Memr[out], Memr[out], nx,
			1.0, real(MAXLOG), dz1, dz2)
		} else
		    call amapr (Memr[in], Memr[out], nx, z1, z2, dz1, dz2)
	    }
	}

	call sigl2_free (si)
end


# DS_ERASE_BORDER -- Zero the border of the window if the frame has not been
# erased, and if the displayed section does not occupy the full window.
# It would be more efficient to do this while writing the greyscale data to
# the output image, but that would complicate the display procedures and frames
# are commonly erased before displaying an image.

procedure ds_erase_border (im, ds, wdes, wx1,wx2,wy1,wy2)

pointer	im			# input image
pointer	ds			# output image (display) 
pointer	wdes			# window descriptor
int	wx1,wx2,wy1,wy2		# section of display window filled by image data

int	dx1,dx2,dy1,dy2		# coords of full display window in device pixels
int	j
pointer	w0
pointer	imps2s()
errchk	imps2s

begin
	# Compute device pixel coordinates of the full display window.
	w0 = W_WC(wdes,0)
	dx1 = W_XS(w0) * (IM_LEN(ds,1) - 1) + 1
	dx2 = W_XE(w0) * (IM_LEN(ds,1) - 1) + 1
	dy1 = W_YS(w0) * (IM_LEN(ds,2) - 1) + 1
	dy2 = W_YE(w0) * (IM_LEN(ds,2) - 1) + 1

	# Erase lower margin.
	for (j=dy1;  j < wy1;  j=j+1)
	    call aclrs (Mems[imps2s (ds, dx1, dx2, j, j)], dx2 - dx1 + 1)

	# Erase left and right margins.  By doing the right margin of a line
	# immediately after the left margin we have a high liklihood that the
	# display line will still be in the FIO buffer.

	for (j=wy1;  j <= wy2;  j=j+1) {
	    if (dx1 < wx1)
		call aclrs (Mems[imps2s (ds, dx1, wx1-1, j, j)], wx1 - dx1)
	    if (wx2 < dx2)
		call aclrs (Mems[imps2s (ds, wx2+1, dx2, j, j)], dx2 - wx2)
	}

	# Erase upper margin.
	for (j=wy2+1;  j <= dy2;  j=j+1)
	    call aclrs (Mems[imps2s (ds, dx1, dx2, j, j)], dx2 - dx1 + 1)
end
