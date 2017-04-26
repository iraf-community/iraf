include "../curses.h"
include "window.h"

# DELWIN --Delete a window
#
# B.Simon	28-Sep-90	Original

procedure delwin (win)

int	win		# i: Window descriptor
#--
include	"window.com"

int	rect[RSIZE]
pointer pwin

begin
	# Can't free the standard screen

	if (win == STDSCR)
	    return

	pwin = warray[win]

	# Copy the screen under the window back to the terminal
	# and then free the buffer which held it

	if (WIN_BUFFER(pwin) != NULL) {
	    call wrect (win, YES, rect)
	    call putscreen (rect, WIN_BUFFER(pwin))
	    call freescreen (WIN_BUFFER(pwin))
	}

	# Free any data structure associated with the window

	if (WIN_DATA(pwin) != NULL)
	    call mfree (WIN_DATA(pwin), TY_STRUCT)

	call mfree (pwin, TY_STRUCT)
	warray[win] = NULL

end
