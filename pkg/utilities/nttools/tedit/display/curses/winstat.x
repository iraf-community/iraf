include	"../curses.h"
include "window.h"

# WINSTAT -- Retrieve a field from a window structure

int procedure winstat (win, what)

int	win		# i: Window descriptor
int	what		# i: Field to retrieve
#--
include "window.com"

int	value
pointer	pwin

string	badcode  "Unrecognized argument to winstat"

begin
	pwin = warray[win]

	switch (what) {
	case W_TOP:
	    value = WIN_TOP(pwin)
	case W_LEFT:
	    value = WIN_LEFT(pwin)
	case W_BOT:
	    value = WIN_BOT(pwin)
	case W_RIGHT:
	    value = WIN_RIGHT(pwin)
	case W_CURROW:
	    value = WIN_CURROW(pwin)
	case W_CURCOL:
	    value = WIN_CURCOL(pwin)
	case W_CLEAR:
	    value = WIN_CLEAR(pwin)
	case W_LEAVE:
	    value = WIN_LEAVE(pwin)
	case W_SCROLL:
	    value = WIN_SCROLL(pwin)
	case W_HIDDEN:
	    value = WIN_HIDDEN(pwin)
	case W_BOXED:
	    value = WIN_BOXED(pwin)
	case W_ATRIB:
	    value = WIN_ATRIB(pwin)
	default:
	    call error (1, badcode)
	}

	return (value)
end
