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
int	wcs1, key1, wcs2, key2, clgcur()
real	wx1, wy1, wx2, wy2

begin
	call printf ( "window:")
	if (clgcur (cursor, wx1, wy1, wcs1, key1, cmd, SZ_LINE) == EOF)
	    return
	switch (key1) {
	case 'e':
	    call printf ("again:")
	    if (clgcur (cursor, wx2, wy2, wcs2, key2, cmd, SZ_LINE) == EOF)
		return
	    call gt_window2 (gt, gp, wx1, wy1, wcs1, key1, cmd,
		wx2, wy2, wcs2, key2, cmd, redraw)
	default:
	    call gt_window1 (gt, gp, wx1, wy1, wcs1, key1, cmd, redraw)
	}
	call printf ("")
end


# GT_WINDOW1 -- Act on window command.

procedure gt_window1 (gt, gp, wx, wy, wcs, key, cmd, redraw)

pointer	gt			#I GTOOLS pointer
pointer	gp			#I GIO pointer
real	wx			#I X Coordinate
real	wy			#I Y Coordinate
int	wcs			#I WCS
int	key			#I Key
char	cmd[ARB]		#I Command
int	redraw			#O Redraw flag

int	gt_geti()
real	x1, x2, y1, y2, dx, dy, wx1, wy1

begin
	redraw = YES
	call ggwind (gp, x1, x2, y1, y2)
	dx = x2 - x1
	dy = y2 - y1

	wx1 = wx
	wy1 = wy
	if (IS_INDEF(wx1))
	    wx1 = (x1 + x2) / 2.
	if (IS_INDEF(wy1))
	    wy1 = (y1 + y2) / 2.
	    

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
	    call gt_setr (gt, GTYMIN, wy1)
	case 'c':
	    call gt_setr (gt, GTXMIN, wx1 - dx / 2)
	    call gt_setr (gt, GTXMAX, wx1 + dx / 2)
	    call gt_setr (gt, GTYMIN, wy1 - dy / 2)
	    call gt_setr (gt, GTYMAX, wy1 + dy / 2)
	case 'd':	# Shift down
	    call gt_setr (gt, GTYMIN, y1 - 0.75 * dy)
	    call gt_setr (gt, GTYMAX, y2 - 0.75 * dy)
	case 'f':	# Flip x axis
	    if (gt_geti (gt, GTXFLIP) == NO)
		call gt_seti (gt, GTXFLIP, YES)
	    else
		call gt_seti (gt, GTXFLIP, NO)
	case 'g':	# Flip y axis
	    if (gt_geti (gt, GTYFLIP) == NO)
		call gt_seti (gt, GTYFLIP, YES)
	    else
		call gt_seti (gt, GTYFLIP, NO)
	case 'j':	# Left edge
	    call gt_setr (gt, GTXMIN, wx1)
	case 'k':	# Right edge
	    call gt_setr (gt, GTXMAX, wx1)
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
	    call gt_setr (gt, GTXMIN, wx1 - dx)
	    call gt_setr (gt, GTXMAX, wx1 + dx)
	    call gt_setr (gt, GTYMIN, wy1 - dy)
	    call gt_setr (gt, GTYMAX, wy1 + dy)
	case 'r':	# Shift right
	    call gt_setr (gt, GTXMIN, x1 + 0.75 * dx)
	    call gt_setr (gt, GTXMAX, x2 + 0.75 * dx)
	case 't':	# Top edge
	    call gt_setr (gt, GTYMAX, wy1)
	case 'u':	# Shift up
	    call gt_setr (gt, GTYMIN, y1 + 0.75 * dy)
	    call gt_setr (gt, GTYMAX, y2 + 0.75 * dy)
	case 'x':	# Zoom x axis
	    call gt_setr (gt, GTXMIN, wx1 - dx / 4)
	    call gt_setr (gt, GTXMAX, wx1 + dx / 4)
	case 'y':	# Zoom y axis
	    call gt_setr (gt, GTYMIN, wy1 - dy / 4)
	    call gt_setr (gt, GTYMAX, wy1 + dy / 4)
	case 'z':	# Zoom x and y axis
	    call gt_setr (gt, GTXMIN, wx1 - dx / 4)
	    call gt_setr (gt, GTXMAX, wx1 + dx / 4)
	    call gt_setr (gt, GTYMIN, wy1 - dy / 4)
	    call gt_setr (gt, GTYMAX, wy1 + dy / 4)
	case 'I':
	    call fatal (0, "Interrupt")
	default:
	    call printf ("\07")
	    redraw = NO
	}
end


# GT_WINDOW2 -- Act on window command.

procedure gt_window2 (gt, gp, wx1, wy1, wcs1, key1, cmd1,
	wx2, wy2, wcs2, key2, cmd2, redraw)

pointer	gt			#I GTOOLS pointer
pointer	gp			#I GIO pointer
real	wx1, wx2		#I X Coordinate
real	wy1, wy2		#I Y Coordinate
int	wcs1, wcs2		#I WCS
int	key1, key2		#I Key
char	cmd1[ARB], cmd2[ARB]	#I Command
int	redraw			#O Redraw flag

real	x1, x2, y1, y2, dx, dy

begin
	redraw = YES
	call ggwind (gp, x1, x2, y1, y2)
	dx = x2 - x1
	dy = y2 - y1

	switch (key1) {
	case 'e':	# Expand window
	    if (abs (wx2 - wx1) > 0.001 * abs (dx)) {
	        call gt_setr (gt, GTXMIN, wx1)
	        call gt_setr (gt, GTXMAX, wx2)
	    }
	    if (abs (wy2 - wy1) > 0.001 * abs (dy)) {
	        call gt_setr (gt, GTYMIN, wy1)
	        call gt_setr (gt, GTYMAX, wy2)
	    }
	default:
	    call printf ("\07\n")
	    redraw = NO
	}
end
