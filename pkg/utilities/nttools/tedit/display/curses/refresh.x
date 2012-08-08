include "../curses.h"
include "window.h"

# REFRESH -- Bring the terminal screen up to date
#
# B.Simon	02-Oct-90	Original

procedure refresh ()

#--

begin
	call wrefresh (STDSCR)
end

procedure wrefresh (win)

int	win		# i: Window descriptor
#--
include "window.com"

int	rect[RSIZE]
pointer	pwin

begin
	pwin = warray[win]

	# If the clear flag is set, redraw the contents of the window

	if (WIN_CLEAR(pwin) == YES) {
	    WIN_CLEAR(pwin) = NO
	    call wrect (win, YES, rect)
	    call putscreen (rect, NULL)

	    if (WIN_LEAVE(pwin) == NO) {
		call ps_setcur (WIN_TOP(pwin)+WIN_CURROW(pwin)-1,
				WIN_LEFT(pwin)+WIN_CURCOL(pwin)-1)
	    }
	}

	call ps_synch
end
