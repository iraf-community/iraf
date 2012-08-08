include "../curses.h"
include "window.h"

# CLRTOEOL -- Clear window to end of current line
#
# B.Simon	01-Oct-90	Original

procedure clrtoeol ()

#--

begin
	call wclrtoeol (STDSCR)
end

procedure wclrtoeol (win)

int	win		# i: Window descriptor
#--
include "window.com"

int	blank, rect[RSIZE]
pointer	pwin

data	blank	/ ' ' /

begin
	pwin = warray[win]

	# Construct the rectangle consisting of the remainder of the
	# current line and fill with blanks

	RTOP(rect) = WIN_TOP(pwin) + WIN_CURROW(pwin) - 1
	RLEFT(rect) = WIN_LEFT(pwin) + WIN_CURCOL(pwin) - 1
	RBOT(rect) = RTOP(rect)
	RRIGHT(rect) = WIN_RIGHT(pwin)
	call ps_fill (rect, blank, WIN_ATRIB(pwin))

	# Move the cursor to the new end of the line

	if (WIN_LEAVE(pwin) == NO)
	    call ps_setcur (WIN_TOP(pwin)+WIN_CURROW(pwin)-1,
			    WIN_LEFT(pwin)+WIN_CURCOL(pwin)-1)

end
