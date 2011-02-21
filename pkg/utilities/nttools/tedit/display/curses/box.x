include "../curses.h"
include "window.h"

# BOX -- Draw a box around a window
#
# B.Simon	01-Oct-90	Original

procedure box (win, vert, hor)

int	win		# i: Window descriptor
char	vert		# i: Character used for vertical side of window
char	hor		# i: Character used for horizontal side of window
#--
include "window.com"

int	vcode, hcode, rect[RSIZE]
pointer	pwin

begin
	# Don't box window if either dimension < 3 or window is already boxed

	pwin = warray[win]
	if (WIN_HEIGHT(pwin) < 3 || WIN_WIDTH(pwin) < 3 || 
	    WIN_BOXED(pwin) == YES)
	    return

	# Draw box

	vcode = vert
	hcode = hor

	RASG(rect, WIN_TOP(pwin), WIN_LEFT(pwin), 
	     WIN_TOP(pwin), WIN_RIGHT(pwin))
	call ps_fill (rect, hcode, WIN_ATRIB(pwin))

	RASG(rect, WIN_BOT(pwin), WIN_LEFT(pwin), 
	     WIN_BOT(pwin), WIN_RIGHT(pwin))
	call ps_fill (rect, hcode, WIN_ATRIB(pwin))

	RASG(rect, WIN_TOP(pwin), WIN_LEFT(pwin), 
	     WIN_BOT(pwin), WIN_LEFT(pwin))
	call ps_fill (rect, vcode, WIN_ATRIB(pwin))

	RASG(rect, WIN_TOP(pwin), WIN_RIGHT(pwin), 
	     WIN_BOT(pwin), WIN_RIGHT(pwin))
	call ps_fill (rect, vcode, WIN_ATRIB(pwin))

	# Reduce size of window's rectangle and mark as boxed

	WIN_TOP(pwin) = WIN_TOP(pwin) + 1
	WIN_LEFT(pwin) = WIN_LEFT(pwin) + 1
	WIN_BOT(pwin) = WIN_BOT(pwin) - 1
	WIN_RIGHT(pwin) = WIN_RIGHT(pwin) - 1
	WIN_BOXED(pwin) = YES

end
