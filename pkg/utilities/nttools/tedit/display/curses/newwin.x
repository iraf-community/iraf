include "../curses.h"
include "window.h"

# NEWWIN -- Create a new window
#
# B.Simon	28-Sep-90	Original

int procedure newwin (nrows, ncols, row, col)

int	nrows		# i: Window height
int	ncols		# i: Window width
int	row		# i: Top row of window
int	col		# i: Leftmost column of window
#--
include "window.com"

int	win, maxrow, maxcol
pointer	pwin

int	ps_height(), ps_width()

begin
	# Find an empty slot in the window array and allocate a window

	for (win = 1; win <= MAXWIN; win = win + 1) {
	   if (warray[win] == NULL)
		break
	}

	if (win > MAXWIN)
	    call error (1, "Cannot create window")

	call malloc (pwin, LEN_WINSTRUCT, TY_STRUCT)
	warray[win] = pwin

	# Compute the window's rectangle, making sure it is on the screen

	maxrow = ps_height ()
	maxcol = ps_width ()

	if (row + nrows - 1 > maxrow)
	    WIN_TOP(pwin) = maxrow - nrows + 1
	else
	    WIN_TOP(pwin) = row
	WIN_TOP(pwin) = max (1, WIN_TOP(pwin))

	if (col + ncols - 1 > maxcol)
	    WIN_LEFT(pwin) = maxcol - ncols + 1
	else
	    WIN_LEFT(pwin) = col
	WIN_LEFT(pwin) = max (1, WIN_LEFT(pwin))

	WIN_BOT(pwin) = min (maxrow, WIN_TOP(pwin) + nrows - 1)
	WIN_RIGHT(pwin) = min (maxcol, WIN_LEFT(pwin) + ncols - 1)

	# Set the remaining fields of the window

	WIN_CURROW(pwin) = 1
	WIN_CURCOL(pwin) = 1
	WIN_CLEAR(pwin) = NO
	WIN_LEAVE(pwin) = NO
	WIN_SCROLL(pwin) = NO
	WIN_HIDDEN(pwin) = NO
	WIN_BOXED(pwin) = NO
	WIN_ATRIB(pwin) = A_NORM

	if (saved == NO) {
	    WIN_BUFFER(pwin) = NULL
	} else {
	    call getscreen (WIN_RECT(pwin), WIN_BUFFER(pwin))
	}

	WIN_FUNC(pwin) = NULL
	WIN_DATA(pwin) = NULL

	# Erase the window

	call werase (win)

	# Return window number

	return (win)
end
