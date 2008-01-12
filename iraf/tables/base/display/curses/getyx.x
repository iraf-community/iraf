include "window.h"

# GETYX -- Get the current cursor position
#
# B.Simon	02-Oct-90	Original

procedure getyx (win, row, col)

int	win		# i: Window descriptor
int	row		# o: Cursor row
int	col		# o: Cursor column
#--
include "window.com"

pointer	pwin

begin
	pwin = warray[win]

	row = WIN_CURROW(pwin)
	col = WIN_CURCOL(pwin)
end
