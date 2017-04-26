include "../curses.h"
include "window.h"

# INCH -- Get character at current cursor position
#
# B.Simon	02-Oct-90	Original

char procedure inch ()

#--
char	winch()

begin
	return (winch (STDSCR))
end

char procedure winch (win)

pointer	win		# i: Window descriptor
#--
include "window.com"

char	ch
int	rect[RSIZE]
pointer	pwin, buf

begin
	pwin = warray[win]

	# Create a box containing the character

	if (WIN_BOXED(pwin) == NO) {
	    RTOP(rect) = WIN_TOP(pwin) + WIN_CURROW(pwin) - 1
	    RLEFT(rect) = WIN_LEFT(pwin) + WIN_CURCOL(pwin) - 1
	} else {
	    RTOP(rect) = WIN_TOP(pwin) + WIN_CURROW(pwin)
	    RLEFT(rect) = WIN_LEFT(pwin) + WIN_CURCOL(pwin)
	}
	RBOT(rect) = RTOP(rect)
	RRIGHT(rect) = RLEFT(rect)

	# Get the character under the cursor

	call getscreen (rect, buf)
	ch = Memc[buf]
	call freescreen (buf)

	return (ch)
end
