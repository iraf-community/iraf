# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"stdgraph.h"

# STG_GETCURSOR -- Get the position of a cursor.  The cursor value is returned
# as a GKI structure on the graphics metacode stream.

procedure stg_getcursor (cursor)

int	cursor			# cursor to be read or 0
int	cur
int	x, y, key
include	"stdgraph.com"

begin
	# If cursor=0 read the last cursor referenced, e.g., in a write.
	if (cursor > 0) {
	    SG_CURSOR(g_sg) = cursor
	    cur = cursor
	} else
	    cur = max (1, SG_CURSOR(g_sg))

	# Restore graphics mode in case the user has forgotten the \n while
	# writing to the status line.

	if (g_enable == NO)
	    call stg_genab()

	# If the user has locked the logical cursor override runtime selection.
	if (g_cursor > 0)
	    cur = g_cursor

	# Restore the software cursor position before reading?
	if (SG_UPDCURSOR(g_sg) == YES) {
	    x = SG_CURSOR_X(g_sg)
	    y = SG_CURSOR_Y(g_sg)
	    if (x != 0 && y != 0)
		call stg_setcursor (x, y, cur)
	}

	# Physically read the cursor and return value to caller.
	call stg_readcursor (cur, x, y, key)
	call gki_retcursorvalue (g_stream, x, y, key, cur)
	call flush (g_stream)

	# Save the new position of the cursor for next time.
	if (SG_UPDCURSOR(g_sg) == YES) {
	    SG_CURSOR_X(g_sg) = x
	    SG_CURSOR_Y(g_sg) = y
	}
end
