include <ctype.h>
include "../curses.h"
include "window.h"

define	TABSTOP		8

# ADDSTR -- Add a string to a window
#
# B.Simon	02-Oct-90	Original

procedure addstr (str)

char	str[ARB]	# i: String to add to window
#--

begin
	call waddstr (STDSCR, str)
end

procedure waddstr (win, str)

int	win		# i: Window descriptor
char	str[ARB]	# i: String to add to window
#--
include "window.com"

bool	moved
int	row, col, nrows, ncols, ic, jc, mxchar, itab
pointer	sp, line, pwin

begin
	pwin = warray[win]

	# Don't print anything if echoing is turned off

	if (echoed == NO)
	    return

	# Allocate an array to hold the string to print

	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)

	# Get the current cursor position and window size

	row = WIN_CURROW(pwin)
	col = WIN_CURCOL(pwin)
	ncols = WIN_WIDTH(pwin)
	nrows = WIN_HEIGHT(pwin)

	ic = 0
	jc = 0
	moved = false

	repeat {
	    ic = ic + 1

	    # If the character is printable, just copy it to the array
	    # The array will be printed when the EOS is read

	    if (IS_PRINT(str[ic])) {
		col = col + 1
		Memc[line+jc] = str[ic]
		jc = jc + 1

	    } else {
		# Print any characters in the array before handling
		# the non-printing character

		Memc[line+jc] = EOS
		if (jc > 0) {
		    jc = 0

		    mxchar = ncols - WIN_CURCOL(pwin) + 1
		    call ps_write (WIN_TOP(pwin)+WIN_CURROW(pwin)-1, 
				   WIN_LEFT(pwin)+WIN_CURCOL(pwin)-1, 
				   mxchar, Memc[line], WIN_ATRIB(pwin))

		    WIN_CURCOL(pwin) = col
		    WIN_CURROW(pwin) = row

		    # Check for writing past the edge of the window
		    # and scrolling

		    if (col > ncols) {
			col = 1
			row = row + 1
			moved = true

			if (row > nrows) {
			    row = nrows
			    if (WIN_SCROLL(pwin) == YES) 
				call wslide (WIN_RECT(pwin), DIR_UP, 1)
			}
		    }
		}

		# Some non-printing characters require special action
		# Others are mapped to printing characters and written
		# to the array

		switch (str[ic]) {
		case EOS:
		    ;

		case '\n':
		    call wclrtoeol (win)
		    col = 1
		    row = row + 1
		    moved = true

		    # Scroll window if we've hit the last row
		   
		    if (row > nrows) {
			row = nrows
			if (WIN_SCROLL(pwin) == YES)
			    call wslide (WIN_RECT(pwin), DIR_UP, 1)
		    }

		case '\t':
		    do itab = 1, TABSTOP - mod (col, TABSTOP) {
			Memc[line+jc] = ' '
			jc = jc + 1
			col = col + 1
		    }

		case '\r':
		    if (col > 1) {
			col = 1
			moved = true
		    }

		case '\010':
		    if (col > 1) {
			col = col - 1
			moved = true
		    }

		default:
		    Memc[line+jc] = ' '
		    col = col + 1
		    jc = jc + 1
		}
	    }

	    # Move cursor, as required

	    if (moved) {
		moved = false
		call wmove (win, row, col)
	    }

	} until (str[ic] == EOS)

	call sfree (sp)

end
