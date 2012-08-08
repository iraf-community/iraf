# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include "../lib/ids.h"
include "iis.h"

# ZDISPLAY_I -- Display the referenced image planes in the given color(s)
# and in the given quadrants of the screen.

procedure zdisplay_i (sw, frames, color, quad)

short	sw			# on or off
short	frames[ARB]		# frame list
short	color[ARB]		# color list
short	quad[ARB]		# quadrant list


bool	off
short	channels
short	select[LEN_SELECT]
int	q,c,index, temp
int	mq			# mapped quadrant
int	mapquad()
short	iispack()
int	and(), or(), xor()

include	"iis.com"
include "../lib/ids.com"	# for i_maxframes! only

begin
	if ( sw == IDS_ON ) {
	    off = false
	} else
	    off = true

	# first find out what is on
	call iishdr(IREAD+VRETRACE, LEN_SELECT, COMMAND+LUT, ADVXONTC, 0,0,0)
	call iisio (select, LEN_SELECT * SZB_CHAR)

	# then add in/remove frames
	channels = iispack(frames)

	for ( q = 1 ; quad[q] != IDS_EOD ; q = q + 1 ) {
	    mq = mapquad(quad[q])
	    if ( ! off ) {
	        for ( c =1 ; color[c] != IDS_EOD ; c = c + 1 ) {
		    switch ( color[c] ) {
		        case IDS_RED:
			    index = mq + 8

		        case IDS_GREEN:
			    index = mq + 4

		        case IDS_BLUE:
			    index = mq
		    }
		    select[index] = or ( int(channels), int(select[index]) )
		}
	    } else {
	        for ( c =1 ; color[c] != IDS_EOD ; c = c + 1 ) {
		    switch ( color[c] ) {
		        case IDS_RED:
			    index = mq + 8

		        case IDS_GREEN:
			    index = mq + 4

		        case IDS_BLUE:
			    index = mq
		    }
		    select[index] = and ( xor ( 177777B, int(channels)),
		   			 int(select[index]))
		}
	    }
	}

	# Record which frame is being displayed for cursor readback.
	temp = 0
	do q = 1, LEN_SELECT
	    temp = or (temp, int(select[q]))

	if ( temp == 0)
	    i_frame_on = ERR
	else {
	    do q = 1, i_maxframes {
		if (and (temp, 2**(q-1)) != 0) {
		    i_frame_on = q
		    break
		}
	    }
	}
	call iishdr(IWRITE+VRETRACE, LEN_SELECT, COMMAND+LUT, ADVXONTC, 0,0,0)
	call iisio (select, LEN_SELECT * SZB_CHAR)
end


# MAPQUAD -- map user quadrant to device ... returns ONE-based quadrant
# if prefer ZERO-based, add one to "index" computation above.

int procedure mapquad (quadrant)

short quadrant

int	mq

begin
	switch ( quadrant ) {
	    case 1:
		mq = 2

	    case 2:
		mq = 1

	    case 3:
		mq = 3
	    
	    case 4:
		mq = 4
	    
	    default:
		mq = 1		# should never happen
	}
	return (mq)
end
