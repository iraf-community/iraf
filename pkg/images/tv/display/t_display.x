# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include <imset.h>
include <imhdr.h>
include	<error.h>
include	<pmset.h>
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

char	image[SZ_FNAME]		# Image to display
int	frame			# Display frame
int	erase			# Erase frame?

int	i
pointer	sp, wdes, im, ds

bool	clgetb()
int	clgeti(), btoi(), imd_wcsver(), imtlen(), imtgetim()
pointer	immap(), imd_mapframe1(), imtopenp()
errchk	immap, imd_mapframe1
errchk	ds_getparams, ds_setwcs, ds_load_display, ds_erase_border

begin
	call smark (sp)
	call salloc (wdes, LEN_WDES, TY_STRUCT)
	call aclri (Memi[wdes], LEN_WDES)

	# Open input imagefile.
	im = imtopenp ("image")
	if (imtlen (im) != 1)
	    call error (1, "Only one image may be displayed")
	i = imtgetim (im, image, SZ_FNAME)
	call imtclose (im)
	#call clgstr ("image", image, SZ_FNAME)
	im = immap (image, READ_ONLY, 0)
	if (IM_NDIM(im) <= 0)
	    call error (1, "image has no pixels")

        # Query server to get the WCS version, this also tells us whether
	# we can use the all 16 supported frames.
        if (imd_wcsver() == 0)
	    call clputi ("display.frame.p_max", 4)
	else
	    call clputi ("display.frame.p_max", 16)


	# Open display device as an image.
	frame = clgeti ("frame")
	W_FRAME(wdes)  = frame

	erase = btoi (clgetb ("erase"))
	if (erase == YES)
	    ds = imd_mapframe1 (frame, WRITE_ONLY,
		btoi (clgetb ("select_frame")), erase)
	else
	    ds = imd_mapframe1 (frame, READ_WRITE,
		btoi (clgetb ("select_frame")), erase)

	# Get display parameters and set up transformation.
	call ds_getparams (im, ds, wdes)

	# Compute and output the screen to image pixel WCS.
	call ds_setwcs (im, ds, wdes, image, frame)

	# Display the image and zero the border if necessary.
	call ds_load_display (im, ds, wdes)
	if (!clgetb ("erase") && clgetb ("border_erase"))
	    call ds_erase_border (im, ds, wdes)

	# Free storage.
	call maskcolor_free (W_OCOLORS(wdes))
	call maskcolor_free (W_BPCOLORS(wdes))
	do i = 0, W_MAXWC
	    if (W_UPTR(W_WC(wdes,i)) != NULL)
		call ds_ulutfree (W_UPTR(W_WC(wdes,i)))
	call imunmap (ds)
	call imunmap (im)

	call sfree (sp)
end


# DS_GETPARAMS -- Get the parameters controlling how the image is mapped
# into the display frame.  Set up the transformations and save in the graphics
# descriptor file.  If "repeat" mode is enabled, read the graphics descriptor
# file and reuse the transformations therein.

procedure ds_getparams (im, ds, wdes)

pointer	im, ds, wdes		#I Image, display, and graphics descriptors

bool	fill, zscale_flag, zrange_flag, zmap_flag
real	xcenter, ycenter, xsize, ysize
real	xmag, ymag, xscale, yscale, pxsize, pysize
real	z1, z2, contrast
int	nsample, ncols, nlines
pointer	wnwin, wdwin, wwwin, wipix, wdpix, zpm, bpm
pointer	sp, str, ztrans, lutfile

int	clgeti(), clgwrd(), nowhite()
real	clgetr()
pointer	maskcolor_map(), ds_pmmap(), zsc_pmsection()
pointer	ds_ulutalloc()
bool	streq(), clgetb()
errchk	maskcolor_map, ds_pmmap, zsc_pmsection, mzscale

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (ztrans, SZ_FNAME, TY_CHAR)

	# Get overlay mask and colors.
	call clgstr ("overlay", W_OVRLY(wdes), W_SZSTRING)
	call clgstr ("ocolors", Memc[str], SZ_LINE)
	W_OCOLORS(wdes) = maskcolor_map (Memc[str])

	# Get bad pixel mask.
	call clgstr ("bpmask", W_BPM(wdes), W_SZSTRING)
	W_BPDISP(wdes) = clgwrd ("bpdisplay", Memc[str], SZ_LINE, BPDISPLAY)
	call clgstr ("bpcolors", Memc[str], SZ_LINE)
	W_BPCOLORS(wdes) = maskcolor_map (Memc[str])

	# Determine the display window into which the image is to be mapped
	# in normalized device coordinates.

	xcenter = max(0.0, min(1.0, clgetr ("xcenter")))
	ycenter = max(0.0, min(1.0, clgetr ("ycenter")))
	xsize   = max(0.0, min(1.0, clgetr ("xsize")))
	ysize   = max(0.0, min(1.0, clgetr ("ysize")))

	# Set up a new graphics descriptor structure defining the coordinate
	# transformation used to map the image into the display frame.

	wnwin = W_WC(wdes,W_NWIN)
	wdwin = W_WC(wdes,W_DWIN)
	wwwin = W_WC(wdes,W_WWIN)
	wipix = W_WC(wdes,W_IPIX)
	wdpix = W_WC(wdes,W_DPIX)

	# Determine X and Y scaling ratios required to map the image into the
	# normalized display window.  If spatial scaling is not desired filling
	# must be disabled and XMAG and YMAG must be set to 1.0 in the
	# parameter file.  Fill mode will always produce an aspect ratio of 1;
	# if nonequal scaling is required then the magnification ratios must
	# be set explicitly by the user.

	fill = clgetb ("fill")
	ncols  = IM_LEN(im,1)
	nlines = IM_LEN(im,2)
	if (fill) {
	    # Compute scale in units of window coords per data pixel required
	    # to scale image to fit window.

	    xmag = (IM_LEN(ds,1) * xsize) / ncols
	    ymag = (IM_LEN(ds,2) * ysize) / nlines

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

	xscale = 1.0 / (IM_LEN(ds,1) / xmag)
	yscale = 1.0 / (IM_LEN(ds,2) / ymag)

	# Set device window limits in normalized device coordinates.
	# World coord system 0 is used for the device window.

	W_XS(wnwin) = xcenter - xsize / 2.0
	W_XE(wnwin) = xcenter + xsize / 2.0
	W_YS(wnwin) = ycenter - ysize / 2.0
	W_YE(wnwin) = ycenter + ysize / 2.0

	# Set pixel coordinates of window.
	# If the image is too large to fit in the window given the scaling
	# factors XSCALE and YSCALE, the following will set starting and ending
	# pixel coordinates in the interior of the image.  If the image is too
	# small to fill the window then the pixel coords will reference beyond
	# the bounds of the image.  Note that the 0.5 is because NDC has
	# the screen corner at 0 while screen pixels have the corner at 0.5.

	pxsize = xsize / xscale
	pysize = ysize / yscale

	W_XS(wdwin) = (ncols / 2.0) - (pxsize / 2.0) + 0.5
	W_XE(wdwin) = W_XS(wdwin) + pxsize
	W_YS(wdwin) = (nlines / 2.0) - (pysize / 2.0) + 0.5
	W_YE(wdwin) = W_YS(wdwin) + pysize

	# Compute X and Y magnification ratios required to map image into
	# the device window in device pixel units.

	xmag = (W_XE(wnwin)-W_XS(wnwin))*IM_LEN(ds,1)/(W_XE(wdwin)-W_XS(wdwin))
	ymag = (W_YE(wnwin)-W_YS(wnwin))*IM_LEN(ds,2)/(W_YE(wdwin)-W_YS(wdwin))

	# Compute the coordinates of the image section to be displayed.
	# Round down if upper pixel is exactly at one-half.

	W_XS(wipix) = max (1, nint(W_XS(wdwin)))
	W_XE(wipix) = min (ncols, nint(W_XE(wdwin)-1.01))
	W_YS(wipix) = max (1, nint(W_YS(wdwin)))
	W_YE(wipix) = min (nlines, nint(W_YE(wdwin)-1.01))

	# Now compute the image and display pixels to be used.
	# The image may be truncated to fit in the display window.
	# These are integer coordinates at the pixel centers.

	pxsize = W_XE(wipix) - W_XS(wipix) + 1
	pysize = W_YE(wipix) - W_YS(wipix) + 1
	xcenter = (W_XE(wnwin) + W_XS(wnwin)) / 2.0 * IM_LEN(ds,1) + 0.5
	ycenter = (W_YE(wnwin) + W_YS(wnwin)) / 2.0 * IM_LEN(ds,2) + 0.5

	#W_XS(wdpix) = max (1, nint (xcenter - (pxsize/2.0*xmag) + 0.5))
	W_XS(wdpix) = max (1, int (xcenter - (pxsize/2.0*xmag) + 0.5))
	W_XE(wdpix) = min (IM_LEN(ds,1), nint (W_XS(wdpix)+pxsize*xmag - 1.01))
	#W_YS(wdpix) = max (1, nint (ycenter - (pysize/2.0*ymag) + 0.5))
	W_YS(wdpix) = max (1, int (ycenter - (pysize/2.0*ymag) + 0.5))
	W_YE(wdpix) = min (IM_LEN(ds,2), nint (W_YS(wdpix)+pysize*ymag - 1.01))

	# Now adjust the display window to be consistent with the image and
	# display pixels to be used.

	W_XS(wdwin) = W_XS(wnwin) * IM_LEN(ds,1) + 0.5
	W_XE(wdwin) = W_XE(wnwin) * IM_LEN(ds,1) + 0.5
	W_YS(wdwin) = W_YS(wnwin) * IM_LEN(ds,2) + 0.5
	W_YE(wdwin) = W_YE(wnwin) * IM_LEN(ds,2) + 0.5
	W_XS(wdwin) = (W_XS(wipix)-0.5) + (W_XS(wdwin)-(W_XS(wdpix)-0.5))/xmag
	W_XE(wdwin) = (W_XS(wipix)-0.5) + (W_XE(wdwin)-(W_XS(wdpix)-0.5))/xmag
	W_YS(wdwin) = (W_YS(wipix)-0.5) + (W_YS(wdwin)-(W_YS(wdpix)-0.5))/ymag
	W_YE(wdwin) = (W_YS(wipix)-0.5) + (W_YE(wdwin)-(W_YS(wdpix)-0.5))/ymag

	# Order of interpolator used for spatial transformation.
	W_XT(wdwin) = max(0, min(1, clgeti ("order")))
	W_YT(wdwin) = W_XT(wdwin)

	# Determine the greyscale transformation.
	call clgstr ("ztrans", Memc[ztrans], SZ_FNAME)
	if (streq (Memc[ztrans], "log"))
	    W_ZT(wdwin) = W_LOG
	else if (streq (Memc[ztrans], "linear"))
	    W_ZT(wdwin) = W_LINEAR
	else if (streq (Memc[ztrans], "none"))
	    W_ZT(wdwin) = W_UNITARY
	else if (streq (Memc[ztrans], "user")) {
	    W_ZT(wdwin) = W_USER
	    call salloc (lutfile, SZ_FNAME, TY_CHAR)
	    call clgstr ("lutfile", Memc[lutfile], SZ_FNAME)
	    W_UPTR(wdwin) = ds_ulutalloc (Memc[lutfile], z1, z2)
	} else {
	    call eprintf ("Bad greylevel transformation '%s'\n")
		call pargstr (Memc[ztrans])
	    W_ZT(wdwin) = W_LINEAR
	}

	# The zscale, and zrange parameters determine the algorithms for
	# determining Z1 and Z2, the range of input z values to be mapped
	# into the fixed range of display greylevels.  If sampling and no
	# sample mask is given then create one as a subsampled image section.
	# If greyscale mapping is disabled the zscale and zrange options are
	# disabled.  Greyscale mapping can also be disabled by turning off
	# zscale and zrange and setting Z1 and Z2 to the device greyscale min
	# and max values, producing a unitary transformation.

	if (W_ZT(wdwin) == W_UNITARY || W_ZT(wdwin) == W_USER) {
	    zscale_flag = false
	    zrange_flag = false
	    zmap_flag   = false
	} else {
	    zmap_flag = true
	    zscale_flag = clgetb ("zscale")
	    if (!zscale_flag)
		zrange_flag = clgetb ("zrange")
	}

	if (zscale_flag || (zrange_flag && IM_LIMTIME(im) < IM_MTIME(im))) {
	    call clgstr ("zmask", W_ZPM(wdes), W_SZSTRING)
	    nsample = max (100, clgeti ("nsample"))
	    if (nowhite (W_ZPM(wdes), W_ZPM(wdes), W_SZSTRING) > 0) {
		if (W_ZPM(wdes) == '[')
		    zpm = zsc_pmsection (W_ZPM(wdes), im)
		else
		    zpm = ds_pmmap (W_ZPM(wdes), im)
	    } else
		zpm = NULL
	    iferr (bpm = ds_pmmap (W_BPM(wdes), im)) {
		call erract (EA_WARN)
		bpm = NULL
	    }
	}

	if (zscale_flag) {
	    # Autoscaling is desired.  Compute Z1 and Z2 which straddle the
	    # median computed by sampling a portion of the image.

	    contrast = clgetr ("contrast")
	    call mzscale (im, zpm, bpm, contrast, nsample, z1, z2)
	    if (zpm != NULL)
		call imunmap (zpm)
	    if (bpm != NULL)
		call imunmap (bpm)

	} else if (zrange_flag) {
	    # Use the limits in the header if current otherwise get the
	    # minimum and maximum of the sample mask.
	    if (IM_LIMTIME(im) >= IM_MTIME(im)) {
		z1 = IM_MIN(im)
		z2 = IM_MAX(im)
	    } else {
		call mzscale (im, zpm, bpm, 0., nsample, z1, z2)
		if (zpm != NULL)
		    call imunmap (zpm)
		if (bpm != NULL)
		    call imunmap (bpm)
	    }

	} else if (zmap_flag) {
	    z1 = clgetr ("z1")
	    z2 = clgetr ("z2")
	} else {
	    z1 = IM_MIN(ds)
	    z2 = IM_MAX(ds)
	}

	W_ZS(wdwin) = z1
	W_ZE(wdwin) = z2

	call printf ("z1=%g z2=%g\n")
	    call pargr (z1)
	    call pargr (z2)
	call flush (STDOUT)

	# The user world coordinate system should be set from the CTRAN
	# structure in the image header, but for now we just make it equal
	# to the pixel coordinate system.

	call amovi (Memi[wdwin], Memi[wwwin], LEN_WC)
	W_UPTR(wwwin) = NULL		# should not copy pointers!!
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

procedure ds_setwcs (im, ds, wdes, image, frame)

pointer	im, ds, wdes		# image, display, and coordinate descriptors
char	image[SZ_FNAME]		# image section name
int	frame			# frame

real	a, b, c, d, tx, ty
int	ip, i, j, axis[2]
real	sx, sy
int	dx, dy, snx, sny, dnx, dny
pointer	sp, imname, title, wnwin, wdwin
pointer	src, dest, region, objref
long	lv[IM_MAXDIM], pv1[IM_MAXDIM], pv2[IM_MAXDIM]

bool	streq()

begin
	call smark (sp)
	call salloc (imname, SZ_FNAME, TY_CHAR)
	call salloc (title,  SZ_LINE,  TY_CHAR)
        call salloc (region, SZ_FNAME, TY_CHAR)
        call salloc (objref, SZ_FNAME, TY_CHAR)

	# Compute the rotation matrix needed to transform screen pixel coords
	# to image section coords.

	wnwin = W_WC(wdes,W_NWIN)
	wdwin = W_WC(wdes,W_DWIN)

	# X transformation.
	a  = (W_XE(wdwin)-W_XS(wdwin))/((W_XE(wnwin)-W_XS(wnwin))*IM_LEN(ds,1))
	c  = 0.0			# not rotated, cross term is zero
	tx = W_XS(wdwin) - a * (W_XS(wnwin) * IM_LEN(ds,1))

	# Y transformation.
	b  = 0.0			# not rotated, cross term is zero
	d  = (W_YE(wdwin)-W_YS(wdwin))/((W_YE(wnwin)-W_YS(wnwin))*IM_LEN(ds,2))
	ty = W_YS(wdwin) - d * (W_YS(wnwin) * IM_LEN(ds,2))

	# Now allow for the Y-flip (origin at upper left in display window).
	d = -d
	ty = W_YE(wdwin) - d * ((1.0 - W_YE(wnwin)) * IM_LEN(ds,2))

	# Now translate the screen corner to the center of the screen pixel.
	tx = tx + 0.5 * a
	ty = ty + 0.5 * d

	# Determine the logical to physical mapping by evaluating two points.
	# and determining the axis reduction if any.  pv1 will be the
	# offset and pv2-pv1 will be the scale.

	call aclrl (pv1, IM_MAXDIM)
	call aclrl (lv, IM_MAXDIM)
	call imaplv (im, lv, pv1, 2)
	call amovkl (long(1), lv, IM_MAXDIM)
	call aclrl (pv2, IM_MAXDIM)
	call imaplv (im, lv, pv2, 2)

	i = 1
	axis[1] = 1;  axis[2] = 2
	do j = 1, IM_MAXDIM
	    if (pv1[j] != pv2[j]) {
		axis[i] = j
		i = i + 1
	    }

	pv2[axis[1]] = (pv2[axis[1]] - pv1[axis[1]])
	pv2[axis[2]] = (pv2[axis[2]] - pv1[axis[2]])

	# These imply a new rotation matrix which we won't bother to work out
	# separately here.  Multiply the two rotation matrices and add the
	# translation vectors to get the overall transformation from screen
	# coordinates to image coordinates.
	a = a * pv2[axis[1]]
	d = d * pv2[axis[2]]
	tx = tx * pv2[axis[1]] + pv1[axis[1]]
	ty = ty * pv2[axis[2]] + pv1[axis[2]]

	# Get the image name (minus image section) and
	# title string (minus any newline.
	call ds_gimage (im, image, Memc[imname], SZ_FNAME)
	call strcpy (IM_TITLE(im), Memc[title], SZ_LINE)
	for (ip=title;  Memc[ip] != '\n' && Memc[ip] != EOS;  ip=ip+1)
	    ;
	Memc[ip] = EOS


	# Define the mapping from the image pixels to frame buffer pixels.
        src  = W_WC(wdes,W_IPIX)
	sx   = W_XS(src)
	sy   = W_YS(src)
	snx  = (W_XE(src) - W_XS(src) + 1)
	sny  = (W_YE(src) - W_YS(src) + 1)

        dest = W_WC(wdes,W_DPIX)
	dx   = W_XS(dest)
	dy   = W_YS(dest)
	dnx  = (W_XE(dest) - W_XS(dest) + 1)
	dny  = (W_YE(dest) - W_YS(dest) + 1)

	# For a single image display the 'region' is fixed. The object ref
	# is the fully defined image node!prefix path, including any sections.
        # We need a special kludge to keep backward compatability with the
        # use of "dev$pix" as the standard test image name.
        call strcpy ("image", Memc[region], SZ_FNAME)
        if (streq (image, "dev$pix"))
            call fpathname ("dev$pix.imh", Memc[objref], SZ_PATHNAME)
        else
            call fpathname (image, Memc[objref], SZ_PATHNAME)

	# Add the mapping info to be written with the WCS.
	call imd_setmapping (Memc[region], sx, sy, snx, sny,
	    dx, dy, dnx, dny, Memc[objref])

	# Write the WCS.
	call imd_putwcs (ds, frame, Memc[imname], Memc[title],
	    a, b, c, d, tx, ty, W_ZS(wdwin), W_ZE(wdwin), W_ZT(wdwin))

	call sfree (sp)
end


# DS_GIMAGE -- Convert input image section name to a 2D physical image section.

procedure ds_gimage (im, input, output, maxchar)

pointer	im			#I IMIO pointer
char	input[ARB]		#I Input image name
char	output[maxchar]		#O Output image name
int	maxchar			#I Maximum characters in output name.

int	i, fd
pointer	sp, section, lv, pv1, pv2

int	stropen(), strlen()
bool	streq()

begin
	call smark (sp)
	call salloc (section, SZ_FNAME, TY_CHAR)
	call salloc (lv, IM_MAXDIM, TY_LONG)
	call salloc (pv1, IM_MAXDIM, TY_LONG)
	call salloc (pv2, IM_MAXDIM, TY_LONG)

	# Get endpoint coordinates in original image.
	call amovkl (long(1), Meml[lv], IM_MAXDIM)
	call aclrl (Meml[pv1], IM_MAXDIM)
	call imaplv (im, Meml[lv], Meml[pv1], 2)
	call amovl (IM_LEN(im,1), Meml[lv], IM_NDIM(im))
	call aclrl (Meml[pv2], IM_MAXDIM)
	call imaplv (im, Meml[lv], Meml[pv2], 2)

	# Set image section.
	fd = stropen (Memc[section], SZ_FNAME, NEW_FILE)
	call fprintf (fd, "[")
	do i = 1, IM_MAXDIM {
	    if (Meml[pv1+i-1] != Meml[pv2+i-1])
		call fprintf (fd, "*")
	    else if (Meml[pv1+i-1] != 0) {
		call fprintf (fd, "%d")
		call pargi (Meml[pv1+i-1])
	    } else
		break
	    call fprintf (fd, ",")
	}
	call close (fd)
	i = strlen (Memc[section])
	Memc[section+i-1] = ']'

	if (streq ("[*,*]", Memc[section]))
	    Memc[section] = EOS

	# Strip existing image section and add new section.
#	call imgimage (input, output, maxchar)
#	call strcat (Memc[section], output, maxchar)
	
	if (Memc[section] == EOS)
	    call imgimage (input, output, maxchar)
	else
	    call strcpy (input, output, maxchar)

	call sfree (sp)
end


# DS_LOAD_DISPLAY -- Map an image into the display window.  In general this
#   involves independent linear transformations in the X, Y, and Z (greyscale)
#   dimensions.  If a spatial dimension is larger than the display window then
#   the image is block averaged.  If a spatial dimension or a block averaged
#   dimension is smaller than the display window then linear interpolation is
#   used to expand the image.  Both the input image and the output device appear
#   to us as images, accessed via IMIO.  All spatial scaling is 
#   handled by the "scaled input" package, i.e., SIGM2[SR].  Our task is to
#   get lines from the scaled input image, transform the greyscale if necessary,
#   and write the lines to the output device.

procedure ds_load_display (im, ds, wdes)

pointer	im			# input image
pointer	ds			# output image
pointer	wdes			# graphics window descriptor

real	z1, z2, dz1, dz2, px1, px2, py1, py2
int	i, order, zt, wx1, wx2, wy1, wy2, wy, nx, ny, xblk, yblk, color
pointer	wdwin, wipix, wdpix, ovrly, bpm, pm, uptr
pointer	in, out, si, si_ovrly, si_bpovrly, ocolors, bpcolors, rtemp
bool	unitary_greyscale_transformation
short	lut1, lut2, dz1_s, dz2_s, z1_s, z2_s

bool	fp_equalr()
int	imstati(), maskcolor()
pointer	ds_pmmap(), imps2s(), imps2r()
pointer	sigm2s(), sigm2i(), sigm2r(), sigm2_setup()
errchk	ds_pmmap, imps2s, imps2r, sigm2s, sigm2i, sigm2r, sigm2_setup
errchk	maskexprn

begin
	wdwin = W_WC(wdes,W_DWIN)
	wipix = W_WC(wdes,W_IPIX)
	wdpix = W_WC(wdes,W_DPIX)

	# Set image and display pixels.
	px1 = nint (W_XS(wipix))
	px2 = nint (W_XE(wipix))
	py1 = nint (W_YS(wipix))
	py2 = nint (W_YE(wipix))
	wx1 = nint (W_XS(wdpix))
	wx2 = nint (W_XE(wdpix))
	wy1 = nint (W_YS(wdpix))
	wy2 = nint (W_YE(wdpix))

	z1 = W_ZS(wdwin)
	z2 = W_ZE(wdwin)
	zt = W_ZT(wdwin)
	uptr = W_UPTR(wdwin)
	order = max (W_XT(wdwin), W_YT(wdwin))

	# Setup scaled input and masks.
	si = NULL
	si_ovrly = NULL
	si_bpovrly = NULL
	nx = wx2 - wx1 + 1
	ny = wy2 - wy1 + 1
	xblk = INDEFI
	yblk = INDEFI

	ocolors = W_OCOLORS(wdes)
	iferr (ovrly = ds_pmmap (W_OVRLY(wdes), im)) {
	    call erract (EA_WARN)
	    ovrly = NULL
	}
	if (ovrly != NULL) {
	    xblk = INDEFI
	    yblk = INDEFI
	    si_ovrly = sigm2_setup (ovrly, NULL, px1,px2,nx,xblk,
		py1,py2,ny,yblk, -1)
	}

	bpcolors = W_BPCOLORS(wdes)
	switch (W_BPDISP(wdes)) {
	case BPDNONE:
	    si = sigm2_setup (im, NULL, px1,px2,nx,xblk, py1,py2,ny,yblk, order)
	case BPDOVRLY:
	    si = sigm2_setup (im, NULL, px1,px2,nx,xblk, py1,py2,ny,yblk, order)
	    iferr (bpm = ds_pmmap (W_BPM(wdes), im))
		bpm = NULL
	    if (bpm != NULL)
		si_bpovrly = sigm2_setup (bpm, NULL, px1,px2,nx,xblk,
		    py1,py2,ny,yblk, -1)
	case BPDINTERP:
	    iferr (bpm = ds_pmmap (W_BPM(wdes), im))
		bpm = NULL
	    if (bpm != NULL)
		pm = imstati (bpm, IM_PMDES)
	    else
		pm = NULL
	    si = sigm2_setup (im, pm, px1,px2,nx,xblk, py1,py2,ny,yblk, order)
	}

	# The device IM_MIN and IM_MAX parameters define the acceptable range
	# of greyscale values for the output device (e.g., 0-255 for most 8-bit
	# display devices).  Values Z1 and Z2 are mapped linearly or
	# logarithmically into IM_MIN and IM_MAX.

	dz1 = IM_MIN(ds)
	dz2 = IM_MAX(ds)
	if (fp_equalr (z1, z2)) {
	    z1 = z1 - 1
	    z2 = z2 + 1
	}

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
		(fp_equalr(z1,dz1) && fp_equalr(z2,dz2))
	} else
	    unitary_greyscale_transformation = false

	if (IM_PIXTYPE(im) == TY_SHORT && zt != W_LOG) {
	    z1_s = z1;  z2_s = z2
	    if (z1_s == z2_s) {
		z1_s = z1_s - 1
		z2_s = z2_s + 1
	    }

	    for (wy=wy1;  wy <= wy2;  wy=wy+1) {
		in  = sigm2s (si, wy - wy1 + 1)
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

		if (si_ovrly != NULL) {
		    in = sigm2i (si_ovrly, wy - wy1 + 1)
		    call maskexprn (ocolors, in, nx)
		    do i = 0, nx-1 {
			if (Memi[in+i] != 0) {
			    color = maskcolor (ocolors, Memi[in+i])
			    if (color >= 0)
				Mems[out+i] = color
			}
		    }
		}
		if (si_bpovrly != NULL) {
		    in = sigm2i (si_bpovrly, wy - wy1 + 1)
		    call maskexprn (bpcolors, in, nx)
		    do i = 0, nx-1 {
			if (Memi[in+i] != 0) {
			    color = maskcolor (bpcolors, Memi[in+i])
			    if (color >= 0)
				Mems[out+i] = color
			}
		    }
		}
	    }

	} else if (zt == W_USER) {
	    call salloc (rtemp, nx, TY_REAL)

	    for (wy=wy1;  wy <= wy2;  wy=wy+1) {
		in  = sigm2r (si, wy - wy1 + 1)
		out = imps2s (ds, wx1, wx2, wy, wy)

		call amapr (Memr[in], Memr[rtemp], nx, z1, z2, 
		    real(U_Z1), real(U_Z2))
		call achtrs (Memr[rtemp], Mems[out], nx)
		call aluts (Mems[out], Mems[out], nx, Mems[uptr])

		if (si_ovrly != NULL) {
		    in = sigm2i (si_ovrly, wy - wy1 + 1)
		    call maskexprn (ocolors, in, nx)
		    do i = 0, nx-1 {
			if (Memi[in+i] != 0) {
			    color = maskcolor (ocolors, Memi[in+i])
			    if (color >= 0)
				Mems[out+i] = color
			}
		    }
		}
		if (si_bpovrly != NULL) {
		    in = sigm2i (si_bpovrly, wy - wy1 + 1)
		    call maskexprn (bpcolors, in, nx)
		    do i = 0, nx-1 {
			if (Memi[in+i] != 0) {
			    color = maskcolor (bpcolors, Memi[in+i])
			    if (color >= 0)
				Mems[out+i] = color
			}
		    }
		}
	    }

	} else {
	    for (wy=wy1;  wy <= wy2;  wy=wy+1) {
		in  = sigm2r (si, wy - wy1 + 1)
		out = imps2r (ds, wx1, wx2, wy, wy)

		if (unitary_greyscale_transformation)  {
		    call amovr (Memr[in], Memr[out], nx)
		} else if (zt == W_LOG) {
		    call amapr (Memr[in], Memr[out], nx,
			z1, z2, 1.0, 10.0 ** MAXLOG)
                    do i = 0, nx-1
                        Memr[out+i] = log10 (Memr[out+i])
		    call amapr (Memr[out], Memr[out], nx,
			0.0, real(MAXLOG), dz1, dz2)
		} else
		    call amapr (Memr[in], Memr[out], nx, z1, z2, dz1, dz2)

		if (si_ovrly != NULL) {
		    in = sigm2i (si_ovrly, wy - wy1 + 1)
		    call maskexprn (ocolors, in, nx)
		    do i = 0, nx-1 {
			if (Memi[in+i] != 0) {
			    color = maskcolor (ocolors, Memi[in+i])
			    if (color >= 0)
				Memr[out+i] = color
			}
		    }
		}
		if (si_bpovrly != NULL) {
		    in = sigm2i (si_bpovrly, wy - wy1 + 1)
		    call maskexprn (bpcolors, in, nx)
		    do i = 0, nx-1 {
			if (Memi[in+i] != 0) {
			    color = maskcolor (bpcolors, Memi[in+i])
			    if (color >= 0)
				Memr[out+i] = color
			}
		    }
		}
	    }
	}

	call sigm2_free (si)
	if (si_ovrly != NULL)
	    call sigm2_free (si_ovrly)
	if (si_bpovrly != NULL)
	    call sigm2_free (si_bpovrly)
	if (ovrly != NULL)
	    call imunmap (ovrly)
	if (bpm != NULL)
	    call imunmap (bpm)
end


# DS_ERASE_BORDER -- Zero the border of the window if the frame has not been
# erased, and if the displayed section does not occupy the full window.
# It would be more efficient to do this while writing the greyscale data to
# the output image, but that would complicate the display procedures and frames
# are commonly erased before displaying an image.

procedure ds_erase_border (im, ds, wdes)

pointer	im			# input image
pointer	ds			# output image (display) 
pointer	wdes			# window descriptor

int	wx1,wx2,wy1,wy2		# section of display window filled by image data
int	dx1,dx2,dy1,dy2		# coords of full display window in device pixels
int	i, nx
pointer	wdwin, wdpix
pointer	imps2s()
errchk	imps2s

begin
	wdwin = W_WC(wdes,W_DWIN)
	wdpix = W_WC(wdes,W_DPIX)

	# Set display pixels and display window pixels.
	wx1 = nint (W_XS(wdpix))
	wx2 = nint (W_XE(wdpix))
	wy1 = nint (W_YS(wdpix))
	wy2 = nint (W_YE(wdpix))
	dx1 = max (1, nint (W_XS(wdwin)))
	dx2 = min (IM_LEN(ds,1), nint (W_XE(wdwin) - 0.01))
	dy1 = max (1, nint (W_YS(wdwin)))
	dy2 = min (IM_LEN(ds,2), nint (W_YE(wdwin) - 0.01))
	nx = dx2 - dx1 + 1

	# Erase lower margin.
	for (i=dy1;  i < wy1;  i=i+1)
	    call aclrs (Mems[imps2s (ds, dx1, dx2, i, i)], nx)

	# Erase left and right margins.  By doing the right margin of a line
	# immediately after the left margin we have a high liklihood that the
	# display line will still be in the FIO buffer.

	for (i=wy1;  i <= wy2;  i=i+1) {
	    if (dx1 < wx1)
		call aclrs (Mems[imps2s (ds, dx1, wx1-1, i, i)], wx1 - dx1)
	    if (wx2 < dx2)
		call aclrs (Mems[imps2s (ds, wx2+1, dx2, i, i)], dx2 - wx2)
	}

	# Erase upper margin.
	for (i=wy2+1;  i <= dy2;  i=i+1)
	    call aclrs (Mems[imps2s (ds, dx1, dx2, i, i)], nx)
end
