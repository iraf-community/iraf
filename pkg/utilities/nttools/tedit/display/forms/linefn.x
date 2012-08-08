include "../curses.h"
include "linefn.h"

# LINEFN -- Function that processes a line of input in a window

procedure linefn (win, ch, maxch, str, data, done)

int	win		#  i: Window descriptor
int	ch		#  i: Keyboard character
int	maxch		#  i: Maximum line length
char	str[ARB]	# io: String containing line
pointer	data		# io: Line data structure
bool	done		#  o: Flag indicating line is done
#--
int	ic, jc, nc
int	strlen()

begin
	done = false
	if (LIN_ROW(data) == 0 || LIN_COL(data) == 0)
	    call getyx (win, LIN_ROW(data), LIN_COL(data))

	if (ch < K_BASE) {
	    LIN_ICHAR(data) = LIN_ICHAR(data) + 1
	    LIN_COL(data) = LIN_COL(data) + 1
	    ic = LIN_ICHAR(data)

	    if (str[ic] == EOS) {
		if (ic > maxch) {
		    done = true
		    LIN_LAST(data) = EOS
		} else {
		    str[ic] = ch
		    str[ic+1] = EOS
		    call waddstr (win, str[ic])
		}
	    } else {
		nc = strlen (str)
		if (nc >= maxch) {
		    done = true
		    LIN_LAST(data) = EOS
		} else {
		    do jc = nc+1, ic, -1
			str[jc+1] = str[jc]

		    str[ic] = ch
		    call winsch (win, str[ic])
		}
	    }

	    if (ch == '\r') {
		done = true
		LIN_LAST(data) = '\n'
	    }

	} else {
	    ic = LIN_ICHAR(data)
	    switch (ch) {
	    case K_RIGHT:  # Move right one column
		if (str[ic] == EOS) {
		    done = true
		    LIN_LAST(data) = EOS
		} else {
		    LIN_ICHAR(data) = LIN_ICHAR(data) + 1
		    LIN_COL(data) = LIN_COL(data) + 1
		    call wmove (win, LIN_ROW(data), LIN_COL(data))
		}

	    case K_LEFT:  # Move left one column
		if (ic == 1) {
		    done = true
		    LIN_LAST(data) = EOS
		} else {
		    LIN_ICHAR(data) = LIN_ICHAR(data) - 1
		    LIN_COL(data) = LIN_COL(data) - 1
		    call wmove (win, LIN_ROW(data), LIN_COL(data))
		}

	    case K_BOL:  # Move to first column in line
		if (ic > 1) {
		    LIN_ICHAR(data) = 1
		    LIN_COL(data) = LIN_COL(data) - ic + 1
		    call wmove (win, LIN_ROW(data), LIN_COL(data))
		}

	    case K_EOL:  # Move to last column in line
		if (str[ic] != EOS) {
		    LIN_ICHAR(data) = strlen (str) + 1
		    LIN_COL(data) = LIN_COL(data) + LIN_ICHAR(data) - ic
		    call wmove (win, LIN_ROW(data), LIN_COL(data))
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
		    LIN_ICHAR(data) = LIN_ICHAR(data) - 1
		    LIN_COL(data) = LIN_COL(data) - 1

		    ic = ic - 1
		    while (str[ic] != EOS) {
			str[ic] = str[ic+1]
			ic = ic + 1
		    }

		    call wmove (win, LIN_ROW(data), LIN_COL(data))
		    call wdelch (win)
		}

	    case K_DWORD:  # Delete entire line
		if (str[1] != EOS) {
		    LIN_ICHAR(data) = 1
		    LIN_COL(data) = LIN_COL(data) - ic + 1

		    for (ic = 1; str[ic] != EOS; ic = ic + 1)
			str[ic] = ' '
		    call addstr (win, str)
		    str[1] = EOS
		}

	    default:
		done = true
		LIN_LAST(data) = ch
	    }
	}
end
