include "../curses.h"
include "window.h"

# BINDSTRUCT -- Bind a data structure and function to a window

procedure bindstruct (func, structure)

extern	func		# i: Input function
pointer	structure	# i: Data structure
#--

begin
	call wbindstruct (STDSCR, func, structure)
end

procedure wbindstruct (win, func, structure)

int	win		# i: Window descriptor
extern	func		# i: Input function
pointer	structure	# i: Data structure
#--
include "window.com"

pointer	pwin
pointer	locpr()

begin
	pwin = warray[win]
	if (WIN_DATA(pwin) != NULL)
	    call mfree (WIN_DATA(pwin), TY_STRUCT)

	WIN_FUNC(pwin) = locpr (func)
	WIN_DATA(pwin) = structure

end
