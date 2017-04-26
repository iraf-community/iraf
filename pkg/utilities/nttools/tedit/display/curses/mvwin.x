include	"../curses.h"
include	"window.h"

# MVWIN -- Move home position of a window
#
# B.Simon	28-Sep-90	Original

procedure mvwin (win, row, col)

int	win		# i: Window descriptor
int	row		# i: New top row of window
int	col		# i: New left column of window
#--
include	"window.com"

bool	shown
int	rect[RSIZE]
int	maxrow, maxcol, drow, dcol
pointer	pwin

int	ps_width(), ps_height()

begin
	pwin = warray[win]

	# Compute new rectangle containing window
	# Make sure it is confined to the current screen

	maxrow = ps_height ()
	maxcol = ps_width ()

	drow = WIN_HEIGHT(pwin) - 1
	dcol = WIN_WIDTH(pwin) - 1

	RTOP(rect) = max (1, row)
	if (RTOP(rect) + drow > maxrow)
	    RTOP(rect) = maxrow - drow

	RLEFT(rect) = max (1, col)
	if (RLEFT(rect) + dcol > maxcol)
	    RLEFT(rect) = maxcol - dcol

	RBOT(rect) = RTOP(rect) + drow
	RRIGHT(rect) = RLEFT(rect) + dcol

	# Move the window by hiding it at its old location
	# and showing it at the new location

	if (RTOP(rect) != WIN_TOP(pwin) || RLEFT(rect) != WIN_LEFT(pwin)) {
	    shown = WIN_HIDDEN(pwin) == NO
	    if (shown)
		call hidewin (win)

	    WIN_TOP(pwin) = RTOP(rect)
	    WIN_LEFT(pwin) = RLEFT(rect)
	    WIN_BOT(pwin) = RBOT(rect)
	    WIN_RIGHT(pwin) = RRIGHT(rect)

	    if (shown)
		call showwin (win)
	}

end
