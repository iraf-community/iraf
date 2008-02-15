# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>

.help imdwcs
.nf -------------------------------------------------------------------------
IMDWCS -- Simple interim WCS package for the display interface.  This is a
restricted use interface which will be obsoleted by a future interface.

	   iw = iw_open (ds, frame, imname, sz_imname, status)
	       iw_fb2im (iw, fb_x,fb_y, im_x,im_y)
	       iw_im2fb (iw, im_x,im_y, fb_x,fb_y)
	       iw_close (iw)


This facility uses the WCSDIR file mechanism to retrieve the WCS information
for a display frame.  The display name is given by the current value of the
'stdimage' environment variable.  Although the WCSDIR info supports a full
2D rotation matrix we recognize only scale and shift terms here.

NOTE -- The frame buffer coordinates used here are defined in the coordinate
system of the DISPLAY program, IMD_MAPFRAME, etc., i.e., the origin is at the
lower left corner of the frame, and the system is one-indexed.  The WCS file,
on the other hand, stores device frame buffer coordinates, which are zero
indexed with the origin at the upper left.
.endhelp --------------------------------------------------------------------

define	LEN_IWDES	6

define	IW_A		Memr[P2R($1)]	# x scale
define	IW_B		Memr[P2R($1+1)]	# cross term (not used)
define	IW_C		Memr[P2R($1+2)]	# cross term (not used)
define	IW_D		Memr[P2R($1+3)]	# y scale
define	IW_TX		Memr[P2R($1+4)]	# x shift
define	IW_TY		Memr[P2R($1+5)]	# y shift


# IW_OPEN -- Retrieve the WCS information for the given frame of the stdimage
# display device.  If the WCS for the frame cannot be accessed for any reason
# a unitary transformation is returned and wcs_status is set to ERR.  Note that
# this is not a hard error, i.e., a valid descriptor is still returned.

pointer procedure iw_open (ds, frame, imname, sz_imname, wcs_status)

pointer	ds		#I display image descriptor
int	frame		#I frame number for which WCS is desired
char	imname[ARB]	#O receives name of image loaded into frame (if any)
int	sz_imname	#I max chars out to imname[].
int	wcs_status	#O ERR if WCS cannot be accessed, OK otherwise

pointer	iw
int	server
char	junk[1]
int	imd_getwcs()
errchk	calloc

begin
	call calloc (iw, LEN_IWDES, TY_STRUCT)

	# Get the WCS.
	server = IM_LEN(ds,4)
	wcs_status = imd_getwcs (frame, server, imname, sz_imname, junk,0,
	    IW_A(iw), IW_B(iw), IW_C(iw), IW_D(iw), IW_TX(iw), IW_TY(iw))
 
	# Avoid divide by zero problems if invalid WCS.
	if (abs(IW_A(iw)) < .0001 || abs(IW_D(iw)) < .0001) {

	    IW_A(iw)  = 1.0;  IW_D(iw)  = 1.0
	    IW_TX(iw) = 0.0;  IW_TY(iw) = 0.0
	    wcs_status = ERR

	} else {
	    # Convert hardware FB to display FB coordinates.
	    IW_TY(iw) = IW_TY(iw) + (IW_D(iw) * (IM_LEN(ds,2)-1))
	    IW_D(iw)  = -IW_D(iw)
	}

	return (iw)
end


# IW_FB2IM -- Convert frame buffer coordinates to image pixel coordinates.

procedure iw_fb2im (iw, fb_x,fb_y, im_x,im_y)

pointer	iw		#I imd wcs descriptor
real	fb_x,fb_y	#I frame buffer X,Y coordinates
real	im_x,im_y	#O image pixel X,Y coordinates

begin
	im_x = (fb_x - 1) * IW_A(iw) + IW_TX(iw)
	im_y = (fb_y - 1) * IW_D(iw) + IW_TY(iw)
end


# IW_IM2FB -- Convert image pixel coordinates to frame buffer coordinates.

procedure iw_im2fb (iw, im_x,im_y, fb_x,fb_y)

pointer	iw		#I imd wcs descriptor
real	im_x,im_y	#I image pixel X,Y coordinates
real	fb_x,fb_y	#O frame buffer X,Y coordinates

begin
	fb_x = (im_x - IW_TX(iw)) / IW_A(iw) + 1
	fb_y = (im_y - IW_TY(iw)) / IW_D(iw) + 1
end


# IW_CLOSE -- Close the IW descriptor.

procedure iw_close (iw)

pointer	iw		#I imd wcs descriptor

begin
	call mfree (iw, TY_STRUCT)
end
