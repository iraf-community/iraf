# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"../lib/ids.h"

# IDS_GETCURSOR -- Get the position of a cursor.  This is the low level
# cursor read procedure.  Reading the image cursor is only possible when
# the ids kernel is run interactively, i.e., when the kernel is linked
# into the CL process, which owns the terminal.  A raw binary read is required.
# The cursor value is returned as a GKI structure on the stream "i_in",
# i.e., it is sent back to the process which requested it.

procedure ids_getcursor (cursor)

int	cursor

int	cur
int	x, y, key

include	"../lib/ids.com"

begin
	cur = cursor
	if ( cur > IDS_CSPECIAL ) {
	    switch( cur ) {
		case IDS_BUT_RD, IDS_BUT_WT:
	            call iisbutton( cur, x, y, key)
	    }
	} else
	    call zcursor_read (cur, x, y, key)

	call gki_retcursorvalue (i_in, x, y, key, cur)
	call flush (i_in)
end
