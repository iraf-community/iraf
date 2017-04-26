include "../curses.h"
include "window.h"

# MOVE -- Move the cursor coordinates
#
# B.Simon	02-Oct-90	Original

procedure move (row, col)

int	row		# i: Cursor row
int	col		# i: Cursor column
#--

begin
	call wmove (STDSCR, row, col)
end

procedure wmove (win, row, col)

int	win		# i: Window descriptor
int	row		# i: Cursor row
int	col		# i: Cursor column
#--
include "window.com"

pointer	pwin

begin
	pwin = warray[win]

	WIN_CURROW(pwin) = max (1, row)
	WIN_CURROW(pwin) = min (WIN_CURROW(pwin), WIN_HEIGHT(pwin))

	WIN_CURCOL(pwin) = max (1, col)
	WIN_CURCOL(pwin) = min (WIN_CURCOL(pwin), WIN_WIDTH(pwin))

	call ps_setcur (WIN_TOP(pwin)+WIN_CURROW(pwin)-1,
			WIN_LEFT(pwin)+WIN_CURCOL(pwin)-1)
end
