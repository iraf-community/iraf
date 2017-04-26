include "../curses.h"
include "window.h"

# GETSTRUCT -- Get the data structure associated with a window

procedure getstruct (structure)

pointer	structure	# o: Data structure
#--

begin
	call wgetstruct (STDSCR, structure)
end

procedure wgetstruct (win, structure)

int	win		# i: Window descriptor
pointer	structure	# o: Data structure
#--
include "window.com"

pointer	pwin

begin
	pwin = warray[win]
	structure = WIN_DATA(pwin)
end
