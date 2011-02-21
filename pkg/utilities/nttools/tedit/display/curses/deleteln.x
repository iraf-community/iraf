include "../curses.h"
include "window.h"

# DELETELN -- Delete a line from the window
#
# B.Simon	01-Oct-90	Original

procedure deleteln ()

#--

begin
	call wdeleteln (STDSCR)
end

procedure wdeleteln (win)

pointer	win		# i: Window descriptor
#--
include "window.com"

int	rect[RSIZE]
pointer pwin

begin
	pwin = warray[win]

	# Construct rectangle to slide

	RTOP(rect) = WIN_TOP(pwin) + WIN_CURROW(pwin) - 1
	RLEFT(rect) = WIN_LEFT(pwin)
	RBOT(rect) = WIN_BOT(pwin)
	RRIGHT(rect) = WIN_RIGHT(pwin)

	call wslide (rect, DIR_UP, 1)

	if (WIN_LEAVE(pwin) == NO)
	    call ps_setcur (WIN_TOP(pwin)+WIN_CURROW(pwin)-1,
			    WIN_LEFT(pwin)+WIN_CURCOL(pwin)-1)

end
