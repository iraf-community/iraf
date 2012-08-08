include "../curses.h"
include "promptfn.h"

# PROMPTFN -- Function that processes the prompt input

procedure promptfn (win, ch, maxch, str, data, done)

int	win		#  i: Window descriptor
int	ch		#  i: Keyboard character
int	maxch		#  i: Maximum line length
char	str[ARB]	# io: String containing line
pointer	data		# io: Line data structure
bool	done		#  o: Flag indicating line is done
#--
int	nrows, ncols, ic, jc, nc
int	strlen()

begin
	# Check for carriage return

	if (ch == '\r') {
	    done = true
	    return
	} else {
	    done = false
	}

	# Initialize data structure on first pass through

	if (PR_START(data) == YES) {
	    call wdimen (win, nrows, ncols)
	    if (nrows == 1)
		call werase (win)

	    PR_START(data) = NO
	    PR_ROW(data) = min (2, nrows)
	    PR_COL(data) = 1
	}

	if (ch < K_BASE) {
	    ic = PR_COL(data)

	    if (str[ic] == EOS) {
		if (ic <= maxch) {
		    str[ic] = ch
		    str[ic+1] = EOS
		    call waddstr (win, str[ic])
		}
	    } else {
		nc = strlen (str)
		if (nc < maxch) {
		    do jc = nc+1, ic, -1
			str[jc+1] = str[jc]

		    str[ic] = ch
		    call winsch (win, str[ic])
		}
	    }
	    PR_COL(data) = PR_COL(data) + 1

	} else {
	    ic = PR_COL(data)
	    switch (ch) {
	    case K_RIGHT:  # Move right one column
		if (str[ic] != EOS) {
		    PR_COL(data) = PR_COL(data) + 1
		    call wmove (win, PR_ROW(data), PR_COL(data))
		}

	    case K_LEFT:  # Move left one column
		if (ic != 1) {
		    PR_COL(data) = PR_COL(data) - 1
		    call wmove (win, PR_ROW(data), PR_COL(data))
		}

	    case K_BOL:  # Move to first column in line
		if (ic > 1) {
		    PR_COL(data) = 1
		    call wmove (win, PR_ROW(data), PR_COL(data))
		}

	    case K_EOL:  # Move to last column in line
		if (str[ic] != EOS) {
		    PR_COL(data) = strlen (str) + 1
		    call wmove (win, PR_ROW(data), PR_COL(data))
		}

	    case K_DEL:  # Delete character underneath cursor
		if (str[ic] != EOS) {
		    while (str[ic] != EOS) {
			str[ic] = str[ic+1]
			ic = ic + 1
		    }

		    call wdelch (win)
		}

	    case K_BS:  # Delete character to left of cursor
		if (ic > 1) {
		    PR_COL(data) = PR_COL(data) - 1

		    ic = ic - 1
		    while (str[ic] != EOS) {
			str[ic] = str[ic+1]
			ic = ic + 1
		    }

		    call wmove (win, PR_ROW(data), PR_COL(data))
		    call wdelch (win)
		}

	    case K_DWORD:  # Delete entire line
		if (str[1] != EOS) {
		    PR_COL(data) = 1

		    for (ic = 1; str[ic] != EOS; ic = ic + 1)
			str[ic] = ' '
		    call addstr (win, str)
		    str[1] = EOS
		}

	    case K_HELP:  # Display help screen
		call fm_help

	    case K_PAINT:  # Redraw the screen
		call clearok (STDSCR, true)
		call wrefresh (STDSCR)
		call wmove (win, PR_ROW(data), PR_COL(data))

	    default:
		call ps_beep
	    }
	}
end
