# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <syserr.h>
include <fset.h>
include "display.h"
include "zdisplay.h"
include "iis.h"

# DCONTROL -- Control functions for the image display device.  This has been
# cleaned up to eliminate unecessary operations and make it more efficient,
# but is only a throwaway program which breaks a few rules.  This file contains
# some explicitly IIS dependent code.

procedure t_dcontrol()

real	rate
int	zoom, type, status
pointer	sp, device, devinfo, tty
bool	erase, window, rgb_window, blink, match, roam
int	red_frame, green_frame, blue_frame, prim_frame, alt_frame, nframes
int	red_chan[2], green_chan[2], blue_chan[2], prim_chan[2], alt_chan[2]
char	type_string[SZ_FNAME], map_string[SZ_FNAME]
int	chan[2], alt1[2], alt2[2] alt3[2] alt4[2]

real	clgetr()
pointer	ttygdes()
bool	clgetb(), streq(), ttygetb()
int	clgeti(), clscan(), nscan(), envgets(), ttygets(), ttygeti(), btoi()
string	stdimage "stdimage"
include	"iis.com"
define	err_ 91

begin
	call smark (sp)
	call salloc (device, SZ_FNAME, TY_CHAR)
	call salloc (devinfo, SZ_LINE, TY_CHAR)

	# Get display parameters.

	call clgstr ("type", type_string, SZ_FNAME)
	call clgstr ("map",  map_string,  SZ_FNAME)

	red_frame   = clgeti ("red_frame")
	green_frame = clgeti ("green_frame")
	blue_frame  = clgeti ("blue_frame")
	prim_frame  = clgeti ("frame")
	alt_frame   = clgeti ("alternate")

	zoom        = clgeti ("zoom")
	rate        = clgetr ("rate")
	erase       = clgetb ("erase")
	window      = clgetb ("window")
	rgb_window  = clgetb ("rgb_window")
	blink       = clgetb ("blink")
	match       = clgetb ("match")
	roam        = clgetb ("roam")

	# Remember current frame.
	call clputi ("frame", prim_frame)
	call iis_setframe (prim_frame)

	# Get device information.
	call clgstr ("device", Memc[device], SZ_FNAME)
	if (streq (device, stdimage)) {
	    if (envgets (stdimage, Memc[device], SZ_FNAME) <= 0)
		call syserrs (SYS_ENVNF, stdimage)
	}
	tty = ttygdes (Memc[device])
	if (ttygets (tty, "DD", Memc[devinfo], SZ_LINE) <= 0)
	    call error (1, "no `DD' entry in graphcap entry for device")

	# Pick up the frame size and configuration number.
	iis_xdim   = ttygeti (tty, "xr")
	iis_ydim   = ttygeti (tty, "yr")
	iis_config = ttygeti (tty, "cn")
	iis_server = btoi (ttygetb (tty, "LC"))

	# Verify operation is legal on device.
	if (iis_server == YES) {
	    if (!streq (type_string, "frame"))
		goto err_
	    if (!streq (map_string, "mono"))
		goto err_
	    if (erase)
		;
	    if (roam)
		goto err_
	    if (window)
		goto err_
	    if (rgb_window)
		goto err_
	    if (blink)
		goto err_
	    if (match) {
err_		call eprintf ("operation not supported for display device %s\n")
		    call pargstr (Memc[device])
		call ttycdes (tty)
		call sfree (sp)
		return
	    }
	}

	# Access display.
	call strpak (Memc[devinfo], Memc[devinfo], SZ_LINE)
	call iisopn (Memc[devinfo], READ_WRITE, chan)
	if (chan[1] == ERR)
	    call error (2, "cannot open display")

	call fseti (STDOUT, F_FLUSHNL, YES)

	red_chan[1]   = FRTOCHAN(red_frame)
	green_chan[1] = FRTOCHAN(green_frame)
	blue_chan[1]  = FRTOCHAN(blue_frame)
	prim_chan[1]  = FRTOCHAN(prim_frame)
	alt_chan[1]   = FRTOCHAN(alt_frame)

	red_chan[2]   = MONO
	green_chan[2] = MONO
	blue_chan[2]  = MONO
	prim_chan[2]  = MONO
	alt_chan[2]   = MONO

	# Execute the selected control functions.
	if (streq (type_string, "rgb")) {
	    type = RGB
	    call zrgbim (red_chan, green_chan, blue_chan)
	} else if (streq (type_string, "frame")) {
	    type = FRAME
	    call zfrmim (prim_chan)
	} else
	    call error (3, "unknown display type")

	# Set display mapping.
	call zmapim (prim_chan, map_string)

	if (erase) {
	    switch (type) {
	    case RGB:
	        call zersim (red_chan)
	        call zersim (green_chan)
	        call zersim (blue_chan)
	    case FRAME:
		call zersim (prim_chan)
	    }

	} else {
	    if (roam) {
		call printf ("Roam display and exit by pushing any button\n")
		call zrmim (prim_chan, zoom)
	    }

	    if (window) {
	        call printf ("Window display and exit by pushing any button\n")
	        call zwndim (prim_chan)
	    }

	    if (rgb_window) {
	        call printf ("Window display and exit by pushing any button\n")
		call zwndim3 (red_chan, green_chan, blue_chan)
	    }

	    if (match)
		call zmtcim (alt_chan, prim_chan)

	    if (blink) {
		if (clscan ("alternate") != EOF) {
		    call gargi (alt1[1])
		    call gargi (alt2[1])
		    call gargi (alt3[1])
		    call gargi (alt4[1])
		    nframes = nscan()

		    alt1[1] = FRTOCHAN(alt1[1])
		    alt2[1] = FRTOCHAN(alt2[1])
		    alt3[1] = FRTOCHAN(alt3[1])
		    alt4[1] = FRTOCHAN(alt4[1])

		    alt1[2] = MONO
		    alt2[2] = MONO
		    alt3[2] = MONO
		    alt4[2] = MONO

		    call printf ("Exit by pushing any button\n")
		    call zblkim (alt1, alt2, alt3, alt4, nframes, rate)
		}
	    }
	}

	# Close display.
	call zclsim (chan[1], status)
	call ttycdes (tty)
	call sfree (sp)
end
