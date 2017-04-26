include <ctype.h>
include "../curses.h"
include "window.h"

# GETSTR -- Get a string from the keyboard and display it in the window
#
# B.Simon	12-Dec-90	Original
# B.Simon	21-Mar-91	Add several new commands

procedure getstr (str, maxch)

char	str[ARB]	# o: String that was read from the keyboard
int	maxch		# i: Maximum string length
#--
int	ch

int	k_get()

begin
	str[1] = EOS
	call weditstr (STDSCR, str, maxch)

	ch = k_get ()	# discard pushed back character
end

procedure wgetstr (win, str, maxch)

pointer	win		# i: Window descriptor
char	str[ARB]	# o: String that was read from the keyboard
int	maxch		# i: Maximum string length
#--
int	ch

int	k_get()

begin
	str[1] = EOS
	call weditstr (win, str, maxch)

	ch = k_get ()	# discard pushed back character
end

# EDITSTR -- Edit a string while displaying it in the window

procedure editstr (str, maxch)

char	str[ARB]	# u: String to edit
int	maxch		# i: Maximum string length
#--

begin
	call weditstr (STDSCR, str, maxch)
end

procedure weditstr (win, str, maxch)

pointer	win		# i: Window descriptor
char	str[ARB]	# u: String to edit
int	maxch		# i: Maximum string length
#--
include "window.com"

pointer pwin

begin
	# NOTE: It is the reponsibility of the calling program
	# to make sure that the current window contents and the
	# string passed to this procedure are in agreement before
	# this procedure is called.

	pwin = warray[win]

	if (WIN_FUNC(pwin) == NULL) {
	    call editfn (win, str, maxch)
	} else {
	    call zcall3 (WIN_FUNC(pwin), win, str, maxch)
	}

end

# EDITFN -- Default function to process window input

procedure editfn (win, str, maxch)

int	win		# i: Window descriptor
char	str[ARB]	# u: String containing line
int	maxch		# i: Maximum line length
#--
int	row, col, ch, ic, jc, mc, nc
pointer	sp, buffer

int	strlen(), k_get(), winstat()

begin
	ic = 0
	nc = strlen (str)

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

	    if (ch == '\r')
		break

	    if (IS_PRINT(ch)) {
		ic = ic + 1
		nc = nc + 1

		if (ic == nc) {
		    str[ic] = ch
		    str[ic+1] = EOS
		    call waddstr (win, str[ic])

		} else {
		    call amovc (str[ic], str[ic+1], nc-ic+1)

		    str[ic] = ch
		    call winsch (win, str[ic])
		}

	    } else {
		switch (ch) {
		case K_UP:  # Move up one field
		    break

		case K_DOWN:  # Move down one field
		    break

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
		    break

		case K_PREVP:  # Move backwards one screen
		    break

		case K_HOME:  # Move to first field
		    break

		case K_END:  # Move to last field
		    break

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
			mc = strlen (Memc[buffer])

			Memc[buffer+mc] = str[ic+1]
			Memc[buffer+mc+1] = EOS

			call amovc (str[ic+2], str[ic+1], nc-ic)

			call wdelch (win)
			nc = nc - 1
		    }

		case K_BS:  # Delete character to left of cursor
		    if (ic > 0) {
			mc = strlen (Memc[buffer])

			call amovc (Memc[buffer], Memc[buffer+1], mc+1)
			Memc[buffer] = str[ic]

			ic = ic - 1
			call amovc (str[ic+2], str[ic+1], nc-ic)

			call wmove (win, row, col+ic)
			call wdelch (win)
			nc = nc - 1
		    }

		case K_DWORD:  # Delete next word
		    call mvword_next (str, ic, jc)

		    if (jc > ic) {
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
			call strcpy (str[ic+1], Memc[buffer], nc-ic)
			str[ic+1] = EOS

			call wclrtoeol (win)
			nc = ic
		    }

		case K_UNDCHR: # Undelete a character
		    mc = strlen (Memc[buffer])
		    if (mc > 0) {
			call amovc (str[ic+1], str[ic+2], nc-ic+1)
			str[ic+1] = Memc[buffer+mc-1]

			Memc[buffer+mc-1] = EOS
			call winsch (win, str[ic+1])

			ic = ic + 1
			nc = nc + 1
		    }

		case K_UNDWRD: # Undelete a word
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

		case K_UNDLIN: # Undelete a line
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
		    break

		case K_PAINT:  # Redraw the screen
		    call clearok (STDSCR, true)
		    call wrefresh (STDSCR)
		    call wmove (win, row, col+ic)

		case K_EXIT:  # Exit procedure
		    break

		default: # Any other character
		    break
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
