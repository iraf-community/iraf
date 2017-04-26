# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctotok.h>
include	<ctype.h>
include	"../lib/ids.h"

# DISPLAY -- Turn frames on or off

procedure display(command)

char	command[ARB]

int	tok
char	token[SZ_LINE]
short	color[IDS_MAXGCOLOR+1]
short	frames[IDS_MAXIMPL+1]
short	quad[5]
short	instruction
int	escape
include "cv.com"

begin
	if (command[1] == 'i')
	    escape = IDS_DISPLAY_I
	else if (command[1] == 'g')
	    escape = IDS_DISPLAY_G
	else {
	    call eprintf ("Only 'di' or 'dg' are understood\n")
	    return
	}

	instruction = ERR
	frames[1] = ERR
	color[1] = ERR
	quad[1] = IDS_EOD

	repeat {
	    call gargtok (tok, token, SZ_LINE)
	    call strlwr (token)
	    if ( tok == TOK_IDENTIFIER) {
	        switch (token[1]) {
		    case 'c':
		        call cv_color (token[2], color)
		        if (color[1] == ERR)
		            return
	    	
		    case 'f':
		        call cv_frame (token[2], frames)
		        if (frames[1] == ERR)
			    return

			
		    case 'o':
			if (token[2] == 'n')
			    instruction = IDS_ON
			else if (token[2] == 'f')
			    instruction = IDS_OFF
		
		    case 'q':
		        call cv_quad (token[2], quad)
		        if (quad[1] == ERR)
			    return
	        }
	    } else if (tok == TOK_NUMBER) {
		call cv_frame (token[1], frames)
		if (frames[1] == ERR)
		    return
	    }
	} until ( tok == TOK_NEWLINE )


	# Require a frame number, but allow default of color and quad to "all".
	# But, for graphics, default the frame and require a color.
	# In either case, for OFF, allow all defaults.
	if (escape == IDS_DISPLAY_I) {
	    if ((instruction == IDS_OFF) && (frames[1] == ERR))
		frames[1] = IDS_EOD
	    if ( color[1] == ERR)
	        color[1] = IDS_EOD
	} else {
	    if ((instruction == IDS_OFF) && ( color[1] == ERR) )
		color[1] = IDS_EOD
	    if ( frames[1] == ERR)
	        frames[1] = IDS_EOD
	}

	if (frames[1] == ERR) {
	    call eprintf ("Frame specification required\n")
	    return
	}
	if (color[1] == ERR) {
	    call eprintf ("Color specification required\n")
	    return
	}

	# if neither "on" nor "off", then turn off all, and turn
	# on the specified frames
	if (instruction == ERR) {
	    call cvdisplay (IDS_OFF , escape, short(IDS_EOD),
	      short(IDS_EOD), short(IDS_EOD))
	    instruction = IDS_ON
	}
	call cvdisplay (instruction, escape, frames, color, quad)
end
