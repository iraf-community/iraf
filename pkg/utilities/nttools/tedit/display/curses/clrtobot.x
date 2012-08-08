include "../curses.h"
include "window.h"

# CLRTOBOT -- Clear window to bottom
#
# B.Simon	28-Sep-90	Original

procedure clrtobot ()

#--

begin
	call wclrtobot (STDSCR)
end

procedure wclrtobot (win)

int	win		# i: Window descriptor
#--
include "window.com"

int	blank, rect[RSIZE]
pointer	pwin

data	blank	/ ' ' /

begin
	pwin = warray[win]

	# First line may be partial, so it must be handled separately

	if (WIN_CURCOL(pwin) == 1) {
	    RTOP(rect) = WIN_TOP(pwin) + WIN_CURROW(pwin) - 1
	} else {
	    RTOP(rect) = WIN_TOP(pwin) + WIN_CURROW(pwin) - 1
	    RLEFT(rect) = WIN_LEFT(pwin) + WIN_CURCOL(pwin) - 1
	    RBOT(rect) = RTOP(rect)
	    RRIGHT(rect) = WIN_RIGHT(pwin)
	    call ps_fill (rect, blank, WIN_ATRIB(pwin))

	    RTOP(rect) = min (RBOT(rect), RTOP(rect)+1)
	}

	# Remaining lines form a rectangle

	RLEFT(rect) = WIN_LEFT(pwin)
	RBOT(rect) = WIN_BOT(pwin)
	RRIGHT(rect) =  WIN_RIGHT(pwin)

	call ps_fill (rect, blank, WIN_ATRIB(pwin))
	if (WIN_LEAVE(pwin) == NO) {
	    call ps_setcur (WIN_TOP(pwin)+WIN_CURROW(pwin)-1,
			    WIN_LEFT(pwin)+WIN_CURCOL(pwin)-1)
	}

end
