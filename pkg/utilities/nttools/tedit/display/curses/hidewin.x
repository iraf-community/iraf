include	"../curses.h"
include "window.h"

# HIDEWIN -- Hide a window
#
# B.Simon	28-Sep-90	Original

procedure hidewin (win)

int	win		# i: Window descriptor
#--
include "window.com"

int	rect[RSIZE]
pointer	pwin, buffer

begin
	pwin = warray[win]

	# Don't do anything if the screen under the window wasn't
	# saved or the window is already hidden

	if (WIN_BUFFER(pwin) == NULL || WIN_HIDDEN(pwin) == YES)
	    return

	# Save the current window contents in a buffer,
	# and restore the screen under the window

	call wrect (win, YES, rect)
	call getscreen (rect, buffer)
	call putscreen (rect, WIN_BUFFER(pwin))

	# Place the window contents in its own buffer and
	# mark the window as hidden

	call freescreen (WIN_BUFFER(pwin))
	WIN_BUFFER(pwin) = buffer
	WIN_HIDDEN(pwin) = YES

end
