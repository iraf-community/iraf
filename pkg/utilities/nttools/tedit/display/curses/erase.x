include "../curses.h"
include "window.h"

# ERASE -- Erase window
#
# B.Simon	28-Sep-90	Original

procedure erase ()

#--

begin
	call werase (STDSCR)
end

procedure werase (win)

int	win		# i: Window descriptor
#--
include "window.com"

int	blank
pointer	pwin

data	blank	/ ' ' /

begin
	pwin = warray[win]
	call ps_fill (WIN_RECT(pwin), blank, WIN_ATRIB(pwin))

	if (WIN_LEAVE(pwin) == NO) {
	    call ps_setcur (WIN_TOP(pwin), WIN_LEFT(pwin))
	    WIN_CURROW(pwin) = 1
	    WIN_CURCOL(pwin) = 1
	}

end
