# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctotok.h>
include	<ctype.h>
include	<gki.h>
include	"../lib/ids.h"

# ZOOM -- zoom, then pan, the display.  If zoom power == 1, then
# don't bother panning.

procedure zoom()

char	token[SZ_LINE]
int	tok, count, power, cnum
short	frames[IDS_MAXIMPL+2]			# frames, graphics, EOD
real	x, y
int	ctoi, ip

include "cv.com"

begin
	# get power for zoom

	call gargtok (tok, token, SZ_LINE)
	if (tok != TOK_NUMBER) {
	    call eprintf ("Bad zoom power: %s\n")
		call pargstr (token)
	    return
	}
	ip = 1
	count = ctoi(token, ip, power)

	# which frames to zoom

	frames[1] = IDS_EOD		# default all frames
	call gargtok (tok, token, SZ_LINE)
	call strlwr (token)
	if (token[1] == 'f') {
	    call cv_frame (token[2], frames)
	    if (frames[1] == ERR)
		return
	} else if (tok == TOK_NUMBER) {
	    call cv_frame (token[1], frames)
	    if (frames[1] == ERR)
		return
	} else {
	    call eprintf ("Unexpected input: %s\n")
		call pargstr (token)
	    return
	}

	# where to zoom ... find which frame to read cursor position from

	cnum = frames[1]
	if (cnum == IDS_EOD)
	    cnum = 0
	call cv_rcur (cnum, x, y)
	call cvzoom (frames, power, x, y)
	call pansub (frames)
end
