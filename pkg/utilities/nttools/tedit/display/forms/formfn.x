include <ctype.h>
include "../curses.h"
include	"formfn.h"

# FORMFN -- Function that processes form input
#
# B.Simon	12-Dec-90	Original

procedure formfn (win, str, maxch)

int	win		# i: Window descriptor
char	str[ARB]	# u: String containing line
int	maxch		# i: Maximum line length
#--
int	row, col, ch, ic, jc, mc, nc
pointer	sp, data, buffer

int	strlen(), k_get(), winstat()

begin
	ic = 0
	nc = strlen (str)

	call wgetstruct (win, data)
	row = winstat (win, W_CURROW)
	col = winstat (win, W_CURCOL)

	call smark (sp)
	call salloc (buffer, SZ_LINE, TY_CHAR)
	Memc[buffer] = EOS

	while (nc < maxch) {

	    # Read character from keyboard

	    call ps_synch
	    ch = k_get ()

	    # Check for carriage return

	    if (ch == '\r') {
		if (FM_FIELD(data) < FM_NFIELD(data)) {
		    FM_FIELD(data) = FM_FIELD(data) + 1
		    break
		}
	    }

	    if (IS_PRINT(ch)) {
		ic = ic + 1
		nc = nc + 1
		FM_CHANGE(data) = YES

		if (ic == nc) {
		    str[ic] = ch
		    str[ic+1] = EOS
		    call waddstr (win, str[ic])

		} else {
		    do jc = nc-1, ic, -1
			str[jc+1] = str[jc]

		    str[ic] = ch
		    call winsch (win, str[ic])
		}

	    } else {
		switch (ch) {
		case K_UP:  # Move up one field
		    if (FM_FIELD(data) > 1) {
			FM_FIELD(data) = FM_FIELD(data) - 1
			break
		    }

		case K_DOWN:  # Move down one field
		    if (FM_FIELD(data) < FM_NFIELD(data)) {
			FM_FIELD(data) = FM_FIELD(data) + 1
			break
		    }

		case K_RIGHT:  # Move right one column
		    if (ic < nc) {
			ic = ic + 1
			call wmove (win, row, col+ic)
		    }

		case K_LEFT:  # Move left one column
		    if (ic > 0) {
			ic = ic - 1
			call wmove (win, row, col+ic)
		    }

		case K_NEXTW:  # Move forwards one word
		    call mvword_next (str, ic, jc)

		    if (jc > ic) {
			ic = jc
			call wmove (win, row, col+ic)
		    }


		case K_PREVW:  # Move backwards one word
		    call mvword_prev (str, ic, jc)

		    if (jc < ic) {
			ic = jc
			call wmove (win, row, col+ic)
		    }


		case K_NEXTP:  # Move forwards one screen
		    if (FM_FIELD(data) < FM_NFIELD(data)) {
			FM_FIELD(data) = min (FM_FIELD(data) + FM_NPAGE(data),
					      FM_NFIELD(data))
			break
		    }

		case K_PREVP:  # Move backwards one screen
		    if (FM_FIELD(data) > 1) {
			FM_FIELD(data) = max (FM_FIELD(data) - FM_NPAGE(data),
					      1)
			break
		    }

		case K_HOME:  # Move to first field
		    if (FM_FIELD(data) > 1) {
			FM_FIELD(data) = 1
			break
		    }

		case K_END:  # Move to last field
		    if (FM_FIELD(data) < FM_NFIELD(data)) {
			FM_FIELD(data) = FM_NFIELD(data)
			break
		    }

		case K_BOL:  # Move to first column in line
		    if (ic > 0) {
			ic = 0
			call wmove (win, row, col)
		    }

		case K_EOL:  # Move to last column in line
		    if (ic < nc) {
			ic = nc
			call wmove (win, row, col+ic)
		    }

		case K_DEL:  # Delete character underneath cursor
		    if (ic < nc) {
			FM_CHANGE(data) = YES
			mc = strlen (Memc[buffer])

			Memc[buffer+mc] = str[ic+1]
			Memc[buffer+mc+1] = EOS

			call amovc (str[ic+2], str[ic+1], nc-ic)

			call wdelch (win)
			nc = nc - 1
		    }

		case K_BS:  # Delete character to left of cursor
		    if (ic > 0) {
			FM_CHANGE(data) = YES
			mc = strlen (Memc[buffer])

			call amovc (Memc[buffer], Memc[buffer+1], mc+1)
			Memc[buffer] = str[ic]

			ic = ic - 1
			call amovc (str[ic+2], str[ic+1], nc-ic)

			call wmove (win, row, col+ic)
			call wdelch (win)
			nc = nc - 1
		    }

		case K_DWORD:  # Delete one word
		    call mvword_next (str, ic, jc)

		    if (jc > ic) {
			FM_CHANGE(data) = YES
			mc = strlen (Memc[buffer])

			call strcpy (str[ic+1], Memc[buffer+mc], jc-ic)
			call amovc (str[jc+1], str[ic+1], nc-jc+1)

			call wclrtoeol (win)
			call waddstr (win, str[ic+1])
			call wmove (win, row, col+ic)
			nc = nc - (jc - ic)
		    }

		case K_DLINE:  # Delete entire line
		    if (nc > 0) {
			FM_CHANGE(data) = YES

			call strcpy (str[ic+1], Memc[buffer], nc-ic)
			str[ic+1] = EOS

			call wclrtoeol (win)
			nc = ic
		    }

		case K_UNDCHR:  # Undelete a character
		    mc = strlen (Memc[buffer])
		    if (mc > 0) {
			call amovc (str[ic+1], str[ic+2], nc-ic+1)
			str[ic+1] = Memc[buffer+mc-1]

			Memc[buffer+mc-1] = EOS
			call winsch (win, str[ic+1])

			ic = ic + 1
			nc = nc + 1
		    }

		case K_UNDWRD:  # Undelete a word
		    mc = strlen (Memc[buffer])
		    call mvword_prev (Memc[buffer], mc, jc)

		    mc = mc - jc
		    if (mc > 0) {
			call amovc (str[ic+1], str[ic+mc+1], nc-ic+1)
			call amovc (Memc[buffer+jc], str[ic+1], mc)

			Memc[buffer+jc] = EOS
			call wclrtoeol (win)
			call waddstr (win, str[ic+1])

			ic = ic + mc
			nc = nc + mc
			call wmove (win, row, col+ic)
		    }

		case K_UNDLIN:  # Undelete a line
		    mc = strlen (Memc[buffer])
		    if (mc > 0) {
			call amovc (str[ic+1], str[ic+mc+1], nc-ic+1)
			call amovc (Memc[buffer], str[ic+1], mc)

			Memc[buffer] = EOS
			call wclrtoeol (win)
			call waddstr (win, str[ic+1])

			ic = ic + mc
			nc = nc + mc
			call wmove (win, row, col+ic)
		    }

		case K_HELP:  # Display help screen
		    call fm_help (win)

		case K_PAINT:  # Redraw the screen
		    call clearok (STDSCR, true)
		    call wrefresh (STDSCR)
		    call wmove (win, row, col+ic)

		case K_EXIT:  # Exit procedure
		    break

		default: # Any other character
		    call ps_beep
		}
	    }
	}

	# Terminate string with EOS and push back character 
	# that terminated input

	if (nc >= maxch)
	    ch = EOS

	str[nc+1] = EOS
	call k_pushbk (ch)

	call sfree (sp)
end
