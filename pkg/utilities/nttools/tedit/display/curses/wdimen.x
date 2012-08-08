include "../curses.h"
include "window.h"

# WDIMEN -- Return dimensions of a window
#
# B.Simon	11-Oct-90	Original

procedure wdimen (win, nrows, ncols)

int	win		# i: Window descriptor
int	nrows		# o: Window height
int	ncols		# o: Window width
#--
int	rect[RSIZE]

begin
	call wrect (win, NO, rect)

	nrows = RHEIGHT(rect)
	ncols = RWIDTH(rect)
end

# WRECT -- Get the rectangle containing the window

procedure wrect (win, border, rect)

int	win		# i: Window descriptor
int	border		# i: Include border in dimensions?
int	rect[RSIZE]	# o: Rectangle containing window dimensions
#--
include "window.com"

pointer	pwin

begin
	pwin = warray[win]
	if (border == NO || WIN_BOXED(pwin) == NO) {
	    RASG(rect, WIN_TOP(pwin), WIN_LEFT(pwin), 
		 WIN_BOT(pwin), WIN_RIGHT(pwin))

	} else {
	    RASG(rect, WIN_TOP(pwin)-1, WIN_LEFT(pwin)-1, 
		 WIN_BOT(pwin)+1, WIN_RIGHT(pwin)+1)
	}
end
