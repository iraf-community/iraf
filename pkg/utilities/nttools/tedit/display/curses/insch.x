include "../curses.h"
include "window.h"

# INSCH -- Insert a character in the window
#
# B.Simon	02-Oct-90	Original

procedure insch (ch)

char	ch		# i: Character to insert
#--

begin
	call winsch (STDSCR, ch)
end

procedure winsch (win, ch)

pointer	win		# i: Window descriptor
char	ch		# i: Character to insert
#--
include "window.com"

char	str[1]
int	rect[RSIZE]
pointer	pwin

begin

	pwin = warray[win]

	# Construct rectangle to slide

	RTOP(rect) = WIN_TOP(pwin) + WIN_CURROW(pwin) - 1
	RLEFT(rect) = WIN_LEFT(pwin) + WIN_CURCOL(pwin) - 1
	RBOT(rect) = RTOP(rect)
	RRIGHT(rect) = WIN_RIGHT(pwin)

	# Slide rectangle

	call wslide (rect, DIR_RIGHT, 1)
	call ps_setcur (WIN_TOP(pwin)+WIN_CURROW(pwin)-1,
			WIN_LEFT(pwin)+WIN_CURCOL(pwin)-1)

	# Write new character

	str[1] = ch
	str[2] = EOS
	call waddstr (win, str)

end
