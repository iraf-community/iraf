# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"gtools.h"

define	HELP	"lib$scr/gtwindow.key"
define	PROMPT	"window options"

# GT_WINDOW -- Set graph window with the cursor.

procedure gt_window (gt, gp, cursor, redraw)

pointer	gt		# GTOOLS pointer
pointer	gp		# GIO pointer
char	cursor[ARB]	# Cursor
int	redraw		# Redraw flag

char	cmd[1]
int	i, wcs, key
real	wx, wy, wx1, wy1, x1, x2, y1, y2, dx, dy

int	clgcur()

begin
	redraw = YES
	call ggwind (gp, x1, x2, y1, y2)
	dx = x2 - x1
	dy = y2 - y1

	call printf ( "window: \n")
	i = clgcur (cursor, wx, wy, wcs, key, cmd, SZ_LINE)
	switch (key) {
	case '?':	# Print help text
	    call gpagefile (gp, HELP, PROMPT)
	    redraw = NO
	case 'a':	# Autoscale x and y axes
	    call gt_setr (gt, GTXMIN, INDEF)
	    call gt_setr (gt, GTXMAX, INDEF)
	    call gt_setr (gt, GTYMIN, INDEF)
	    call gt_setr (gt, GTYMAX, INDEF)
	case 'b':	# Bottom edge
	    call gt_setr (gt, GTYMIN, wy)
	case 'c':
	    call gt_setr (gt, GTXMIN, wx - dx / 2)
	    call gt_setr (gt, GTXMAX, wx + dx / 2)
	    call gt_setr (gt, GTYMIN, wy - dy / 2)
	    call gt_setr (gt, GTYMAX, wy + dy / 2)
	case 'd':	# Shift down
	    call gt_setr (gt, GTYMIN, y1 - 0.75 * dy)
	    call gt_setr (gt, GTYMAX, y2 - 0.75 * dy)
	case 'e':	# Expand window
	    call printf ("again: \n")
	    i = clgcur (cursor, wx1, wy1, wcs, key, cmd, SZ_LINE)
	    if (abs (wx1 - wx) > 0.001 * abs (dx)) {
	        call gt_setr (gt, GTXMIN, wx)
	        call gt_setr (gt, GTXMAX, wx1)
	    }
	    if (abs (wy1 - wy) > 0.001 * abs (dy)) {
	        call gt_setr (gt, GTYMIN, wy)
	        call gt_setr (gt, GTYMAX, wy1)
	    }
	case 'f':	# Flip x axis
	    call gt_setr (gt, GTXMIN, x2)
	    call gt_setr (gt, GTXMAX, x1)
	case 'g':	# Flip y axis
	    call gt_setr (gt, GTYMIN, y2)
	    call gt_setr (gt, GTYMAX, y1)
	case 'j':	# Left edge
	    call gt_setr (gt, GTXMIN, wx)
	case 'k':	# Right edge
	    call gt_setr (gt, GTXMAX, wx)
	case 'l':	# Shift left
	    call gt_setr (gt, GTXMIN, x1 - 0.75 * dx)
	    call gt_setr (gt, GTXMAX, x2 - 0.75 * dx)
	case 'm':	# Autoscale x axis
	    call gt_setr (gt, GTXMIN, INDEF)
	    call gt_setr (gt, GTXMAX, INDEF)
	case 'n':	# Autoscale y axis
	    call gt_setr (gt, GTYMIN, INDEF)
	    call gt_setr (gt, GTYMAX, INDEF)
	case 'p':	# Pan
	    call gt_setr (gt, GTXMIN, wx - dx)
	    call gt_setr (gt, GTXMAX, wx + dx)
	    call gt_setr (gt, GTYMIN, wy - dy)
	    call gt_setr (gt, GTYMAX, wy + dy)
	case 'r':	# Shift right
	    call gt_setr (gt, GTXMIN, x1 + 0.75 * dx)
	    call gt_setr (gt, GTXMAX, x2 + 0.75 * dx)
	case 't':	# Top edge
	    call gt_setr (gt, GTYMAX, wy)
	case 'u':	# Shift up
	    call gt_setr (gt, GTYMIN, y1 + 0.75 * dy)
	    call gt_setr (gt, GTYMAX, y2 + 0.75 * dy)
	case 'x':	# Zoom x axis
	    call gt_setr (gt, GTXMIN, wx - dx / 4)
	    call gt_setr (gt, GTXMAX, wx + dx / 4)
	case 'y':	# Zoom y axis
	    call gt_setr (gt, GTYMIN, wy - dy / 4)
	    call gt_setr (gt, GTYMAX, wy + dy / 4)
	case 'z':	# Zoom x and y axis
	    call gt_setr (gt, GTXMIN, wx - dx / 4)
	    call gt_setr (gt, GTXMAX, wx + dx / 4)
	    call gt_setr (gt, GTYMIN, wy - dy / 4)
	    call gt_setr (gt, GTYMAX, wy + dy / 4)
	case 'I':
	    call fatal (0, "Interrupt")
	default:
	    call printf ("\07\n")
	    redraw = NO
	}
end
