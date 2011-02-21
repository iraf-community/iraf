include "../curses.h"
include "window.h"

# SHOWWIN -- Show a previously hidden window
#
# B.Simon	28-Sep-90	Original

procedure showwin (win)

int	win		# i: Window descriptor
#--
include "window.com"

int	rect[RSIZE]
pointer	pwin, buffer

begin
	pwin = warray[win]

	# Don't do anything if the window is already visible

	if (WIN_HIDDEN(pwin) == NO)
	    return

	# Save the screen under the window in a buffer
	# and display the window's contents

	call wrect (win, YES, rect)
	call getscreen (rect, buffer)
	call putscreen (rect, WIN_BUFFER(pwin))

	# Copy the screen buffer into the window's buffer and
	# mark the window as visible

	call freescreen (WIN_BUFFER(pwin))
	WIN_BUFFER(pwin) = buffer
	WIN_HIDDEN(pwin) = NO

end
