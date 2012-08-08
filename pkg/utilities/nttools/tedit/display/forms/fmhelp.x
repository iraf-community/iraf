include "../curses.h"

define	COLWIDTH	40
define	LABEL_FLAG	1
define	NAME_FLAG	2

# FM_HELP -- Display help window for function key sequences

procedure fm_help (win)

int	win		# i: Window which currently is active
#--
include	"forms.com"

int	key, row, col
int	k_get(), winstat()

begin
	# If the help screen was created on a previous call, 
	# display it, otherwise make a new help screen

	if (helpwin != 0) {
	    call showwin (helpwin)
	} else {
	    call fm_hmake (helpwin)
	}

	# Display help screen and wait for keystroke to hide window

	call refresh
	key = k_get ()

	# Hide the help window and restore cursor to current window

	call hidewin (helpwin)

	row = winstat (win, W_CURROW)
	col = winstat (win, W_CURCOL)
	call wmove (win, row, col)

end

procedure fm_hmake (hwin)

pointer	hwin		# o: help window 
#--
int	ic, nrows, ncols, irow, icol, flag
pointer	sp, label, name, hline, text, ch

string	htitle     "Editing Commands"
string	hfooter	   "(Press any key to continue)"
string	hformat    "%4w%-12.12s = %-12.12s"

int	newwin(), strlen()

begin
	# Allocate dynamic memory for strings

	call smark (sp)
	call salloc (label, COLWIDTH, TY_CHAR)
	call salloc (name, COLWIDTH, TY_CHAR)
	call salloc (hline, COLWIDTH, TY_CHAR)

	# Create help message

	hwin = newwin (GIANT, GIANT, 1, 1)

	# Write help screen title

	call wdimen (hwin, nrows, ncols)
	icol = (ncols - strlen(htitle)) / 2
	call wmove (hwin, 1, icol)
	call wstandout (hwin)
	call waddstr (hwin, htitle)
	call wstandend (hwin)

	ic = 0
	icol = 0
	irow = 3
	flag = LABEL_FLAG
	call k_help (text)

	# Write each (label=name) pair to the help screen

	for (ch = text; Memc[ch] != EOS; ch = ch + 1) {
	    switch (flag) {	
	    case LABEL_FLAG:
		if (Memc[ch] != '=') {
		    Memc[label+ic] = Memc[ch]
		    ic = ic + 1
		} else {
		    Memc[label+ic] = EOS
		    flag = NAME_FLAG
		    ic = 0
		}
	    case NAME_FLAG:
		if (Memc[ch] != '\n') {
		    Memc[name+ic] = Memc[ch]
		    ic = ic + 1
		} else {
		    Memc[name+ic] = EOS
		    flag = LABEL_FLAG
		    ic = 0

		    # Reformat label/name pair for window

		    call sprintf (Memc[hline], COLWIDTH, hformat)
		    call pargstr (Memc[label])
		    call pargstr (Memc[name])

		    # Write string to window

		    call wmove (hwin, irow, icol * COLWIDTH + 1)
		    call waddstr (hwin, Memc[hline])

		    # Calculate next string position

		    icol = icol + 1
		    if (icol == 2) {
			icol = 0
			irow = irow + 1
		    }
		}
	    }
	}

	# Write help screen footer

	call wmove (hwin, nrows, 1)
	call waddstr (hwin, hfooter)
	call sfree (sp)
end
