# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctotok.h>
include	<ctype.h>
include <imhdr.h>
include <gki.h>
include	"../lib/ids.h"

# SNAP -- Take a picture!!

procedure snap()

char	token[SZ_LINE]
int	tok
char	fname[SZ_FNAME]
int	snap_color

include "cv.com"

begin
	snap_color = IDS_SNAP_MONO		# default color for snap
	call gargtok (tok, token, SZ_LINE)
	call strlwr (token)
	if (tok == TOK_IDENTIFIER) {
	    if (token[1] != 'c') {
	        call eprintf ("unknown snap argument: %s\n")
	           call pargstr (token)
	        return
	    } else {
		# snap colors: r, g, b, rgb, m (monochrome) == bw (black/white)
		switch (token[2]) {
		    case 'm':
			snap_color = IDS_SNAP_MONO

		    case 'r':
			if ((token[3] == 'g') && (token[4] == 'b') )
			    snap_color = IDS_SNAP_RGB
			else
			    snap_color = IDS_SNAP_RED

		    case 'g':
			snap_color = IDS_SNAP_GREEN

		    case 'b':
			if (token[3] == 'w')
			    snap_color = IDS_SNAP_MONO
			else
			    snap_color = IDS_SNAP_BLUE
		    
		    default:
			call eprintf ("Unknown snap color: %c\n")
			    call pargc (token[2])
			return
		}
	    }
	} else if (tok != TOK_NEWLINE) {
	    call eprintf ("unexpected argument to snap: %s\n")
		call pargstr (token)
	    return
	}
	
	call clgstr("snap_file", fname, SZ_FNAME)
	call cvsnap (fname, snap_color)
end
