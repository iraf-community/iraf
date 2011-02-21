include "../curses.h"
include "window.h"

# CLEAR -- Clear window and force a redraw of the screen
#
# B.Simon	01-Oct-90	Original

procedure clear ()

#--
include "window.com"

pointer	pwin

begin
	pwin = warray[STDSCR]
	WIN_CLEAR(pwin) = YES

	call werase (STDSCR)
end

procedure wclear (win)

int	win		# i: Window descriptor
#--
include "window.com"

pointer	pwin

begin
	pwin = warray[win]
	WIN_CLEAR(pwin) = YES

	call werase (win)
end
