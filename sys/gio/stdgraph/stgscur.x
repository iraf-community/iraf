# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"stdgraph.h"

# STG_SETCURSOR -- Set the position of a cursor.

procedure stg_setcursor (x, y, cursor)

int	x, y			# new position of cursor
int	cursor			# cursor to be set
int	mx, my, cur
include	"stdgraph.com"

begin
	# If cursor=0, write the cursor last referenced.
	if (cursor > 0) {
	    SG_CURSOR(g_sg) = cursor
	    cur = cursor
	} else
	    cur = max (1, SG_CURSOR(g_sg))

	# If the user has locked the logical cursor override runtime selection.
	if (g_cursor > 0)
	    cur = g_cursor

	# Restore the software cursor position before reading?
	if (SG_UPDCURSOR(g_sg) == YES) {
	    SG_CURSOR_X(g_sg) = x
	    SG_CURSOR_Y(g_sg) = y
	}

	mx = max(g_x1, min(g_x2, nint (x * g_dx) + g_x1))
	my = max(g_y1, min(g_y2, nint (y * g_dy) + g_y1))

	call stg_ctrl3 ("WC", mx, my, cur)
end
