include "../curses.h"
include "window.h"

# STANDOUT -- Put the window in standout mode
#
# B.Simon	02-Oct-90	Original

procedure standout ()

#--

begin
	call wstandout (STDSCR)
end

procedure wstandout (win)

int	win		# i: Window descriptor
#--
include "window.com"

pointer	pwin

begin
	pwin = warray[win]
	WIN_ATRIB(pwin) = A_STANDOUT
end

procedure standend ()

#--

begin
	call wstandend (STDSCR)
end

procedure wstandend (win)

int	win		# i: Window descriptor
#--
include "window.com"

pointer	pwin

begin
	pwin = warray[win]
	WIN_ATRIB(pwin) = A_NORM
end
