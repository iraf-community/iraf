# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

####  load1.x  (from load.x)  ####

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

# LOAD - Load an image.  The specified image section is mapped into
# the specified section of an image display frame.  The mapping involves
# a linear transformation in X and Y and a linear or logarithmic transformation
# in Z (greyscale).  Images of all pixel datatypes are supported, and there
# no upper limit on the size of an image.  The display device is interfaced
# via GIO metacode.

procedure t_load()

char	image[SZ_FNAME]
short	frame[IDS_MAXIMPL+1]
bool	frame_erase, border_erase
pointer	im, wdes, sp

pointer	gp
char	device[SZ_FNAME]
int	dd[LEN_GKIDD]

int	envgets()
short	clgets()
bool	clgetb()
pointer	immap(), gopen()

include	"cv.com"
errchk	immap, imunmap, ds_getparams

begin
	call smark (sp)
	call salloc (cv_stack, CVLEN, TY_SHORT)
	call salloc (wdes, LEN_WDES, TY_STRUCT)

	if (envgets ("stdimage", device, SZ_FNAME) == 0)
	    call error (EA_FATAL,
		"variable 'stdimage' not defined in environment")

	call ids_open (device, dd)
	call gki_inline_kernel (STDIMAGE, dd)
	# Need READ_WRITE so can call cvdisplay
	gp = gopen ( device, READ_WRITE, STDIMAGE)

	call fseti (STDIMAGE, F_TYPE, SPOOL_FILE)
	call fseti (STDIMAGE, F_CANCEL, OK)
	call ids_grstream (STDIMAGE)

	# to do:
	# initialize local variables: image display size, etc
	# instead of defines such as MCXSCALE, etc

	cv_maxframes = CV_MAXF
	cv_maxgraph  = CV_MAXG
	cv_xcen	     = CV_XCEN
	cv_ycen	     = CV_YCEN
	cv_xres	     = CV_XRES
	cv_yres	     = CV_YRES
	cv_zres	     = CV_ZRES
	cv_gp	     = gp
	cv_xcon	     = real(GKI_MAXNDC+1)/CV_XRES
	cv_ycon	     = real(GKI_MAXNDC+1)/CV_YRES
	cv_grch	     = CV_GRCHNUM
	cv_xwinc     = -1.			# Flag: Don't know what lut is

	# Open input imagefile.
	call clgstr ("image", image, SZ_FNAME)
	im = immap (image, READ_ONLY, 0)

	# Ultimately, we should get a sequence of frames, all of which get
	# loaded with the same image.

	frame[1] = clgets ("frame")
	frame[2] = IDS_EOD
	frame_erase = clgetb ("erase")

	# Optimize for sequential i/o.
	call imseti (im, IM_ADVICE, SEQUENTIAL)

	# The frame being displayed does not necessarily change when a new
	# frame is loaded.  (We might consider letting user select via the
	# cv package)

	if (clgetb ("select_frame")) {
	    call cvdisplay (IDS_OFF, IDS_DISPLAY_I, short(IDS_EOD),
		  short(IDS_EOD), short(IDS_EOD))
	    call cvdisplay (IDS_ON, IDS_DISPLAY_I, frame, short(IDS_EOD),
		  short(IDS_EOD))
	}

	if (frame_erase)
	    call cvcleari (frame)

	# Tell GIO what frame(s) to write
	call cv_iset (frame)

	# Done with all possible read/write calls to cv package.  Fix up so
	# don't read device if we erase the frame, so need WRITE_ONLY mode.
	# fseti on STDIMAGE didn't work.

	if (frame_erase) {
	    call gclose (gp)
	    call gki_inline_kernel (STDIMAGE, dd)
	    gp = gopen ( device, WRITE_ONLY, STDIMAGE)
	    cv_gp = gp
	    call fseti (STDIMAGE, F_TYPE, SPOOL_FILE)
	    call fseti (STDIMAGE, F_CANCEL, OK)
	}

	# Get display parameters and set up transformation.
	call ds_getparams (im, wdes, image, frame)

	# Erase the border (space between displayed image section and edge of
	# window) only if screen was not erased and border erasing is enabled.

	if (frame_erase)
	    border_erase = false
	else
	    border_erase = clgetb ("border_erase")

	# Display the image.
	call ds_load_display (im, wdes, border_erase)

	call imunmap (im)

	# All done.
	call gclose (gp)
	call ids_close()
	call sfree (sp)
end


# DS_GETPARAMS -- Get the parameters controlling how the image is mapped
# into the display frame.  Set up the transformations and save in the graphics
# descriptor file.

procedure ds_getparams (im, wdes, image, frame)

pointer	im, wdes		# Image and graphics descriptors
char	image[SZ_FNAME]		# Should be determined from im
short	frame[ARB]

bool	fill, zscale_flag, zrange_flag, zmap_flag
real	xcenter, ycenter
real	xsize, ysize, pxsize, pysize
real	xmag, ymag, xscale, yscale
real	z1, z2, contrast
int	nsample_lines, ncols, nlines, len_stdline
pointer	sp, w, ztrans, lut, lutfile

bool	clgetb()
int	clgeti()
real	clgetr()
bool	streq()

include	"cv.com"

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
	}

	# Determine the display window into which the image is to be mapped
	# in normalized device coordinates.

	xcenter = max(0.0, min(1.0, clgetr ("xcenter")))
	ycenter = max(0.0, min(1.0, clgetr ("ycenter")))
	xsize   = max(0.0, min(1.0, clgetr ("xsize")))
	ysize   = max(0.0, min(1.0, clgetr ("ysize")))

	# Determine X and Y scaling ratios required to map the image into the
	# normalized display window.  If spatial scaling is not desired filling
	# must be disabled and XMAG and YMAG must be set to 1.0 in the
	# parameter file.  Fill mode will always produce an aspect ratio of 1;
	# if nonequal scaling is required then the magnification ratios must
	# be set explicitly by the user.

	if (fill) {
	    # Compute scale in units of window coords per data pixel required
	    # to scale image to fit window.

	    xscale = xsize / max (1, (ncols  - 1))
	    yscale = ysize / max (1, (nlines - 1))

	    if (xscale < yscale)
		yscale = xscale
	    else
		xscale = yscale

	} else {
	    # Compute scale required to provide image magnification ratios
	    # specified by the user.  Magnification is specified in units of
	    # display pixels, i.e, a magnification ratio of 1.0 means that
	    # image pixels will map to display pixels without scaling.

	    xmag = clgetr ("xmag")
	    ymag = clgetr ("ymag")
	    xscale = 1.0 / ((cv_xres - 1) / xmag)
	    yscale = 1.0 / ((cv_yres - 1) / ymag)
	}

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

	# All spatial transformations are linear.
	W_XT(w) = W_LINEAR
	W_YT(w) = W_LINEAR

	# Determine whether a log or linear greyscale transformation is
	# desired.
	if (streq (Memc[ztrans], "log"))
	    W_ZT(w) = W_LOG
	else if (streq (Memc[ztrans], "linear"))
	    W_ZT(w) = W_LINEAR
	else if (streq (Memc[ztrans], "none"))
	    W_ZT(w) = W_UNITARY
	else if (streq (Memc[ztrans], "user")) {
	    W_ZT(w) = W_USER
	    call salloc (lutfile, SZ_FNAME, TY_CHAR)
	    call clgstr ("lutfile", Memc[lutfile], SZ_FNAME)
	    call cv_ulut (Memc[lutfile], z1, z2, lut)
	    W_UPTR(w) = lut
	} else {
	    call eprintf ("Bad greylevel transformation '%s'\n")
		call pargstr (Memc[ztrans])
	    W_ZT(w) = W_LINEAR
	}

	# Set up the greyscale transformation.
	W_ZS(w) = z1
	W_ZE(w) = z2

	# Tell the user what values were used.
	call printf ("cvl: z1 %6.1f, z2 %6.1f\n")
	    call pargr (z1)
	    call pargr (z2)

	# The user world coordinate system should be set from the CTRAN
	# structure in the image header, but for now we just make it equal
	# to the pixel coordinate system.

	call amovi (Memi[w], Memi[W_WC(wdes,2)], LEN_WC)
end
