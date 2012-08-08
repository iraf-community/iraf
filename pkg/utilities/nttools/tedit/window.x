include	"display/curses.h"
include "screen.h"

define	MAXSCR		10
define	MIN_HEIGHT	 4

# COUNT_WINDOW -- Return number of windows

int procedure count_window ()

#--
include "window.com"

begin
	return (nscreens)
end

# FOCUS_WINDOW -- Move the cursor to its current position in a window

procedure focus_window (win)

pointer	win		# i: window descriptor
#--
int	row, col

int	winstat()

begin
	row = winstat (win, W_CURROW)
	col = winstat (win, W_CURCOL)

	call wmove (win, row, col)
end

# GET_WINDOW -- Get a window from the list of screens

int procedure get_window (iscr, scr)

int	iscr		# i: index into list of screens
pointer	scr		# o: screen descriptor
#--
include "window.com"

begin
	# Return EOF if this is the last screen on the list

	if (iscr > nscreens) {
	    scr = scrlist[1]
	    return (EOF)
	} else {
	    scr = scrlist[iscr]
	    return (OK)
	}
end
	
# INIT_WINDOW -- Create the initial set of windows

procedure init_window (scr)

pointer	scr		# o: initial screen
#--
include "window.com"

int	nrows, ncols

int	newwin()

begin
	# Get size of terminal screen

	call wdimen (STDSCR, nrows, ncols)

	# Create the initial screen

	call malloc (scr, TED_SCRLEN, TY_STRUCT)
	TED_WINDOW(scr) = newwin (nrows-2, ncols, 1, 1)
	TED_TABLE(scr) = NULL

	# Create the prompt window

	wprompt = newwin (2, ncols, nrows-1, 1)

	# Initialze the global variables

	nscreens = 1
	scrlist[1] = scr

end

# JOIN_WINDOW -- Join screen window with the adjacent window

procedure join_window (scr)

pointer	scr		# i: screen descriptor
#--
include "window.com"

int	win1, win2
int	iscr, jscr, top, left, row1, col1, row2, col2
pointer	scr2

string	notnull  "join_window: table was not closed"
string	notfound "join_window: could not find screen"

int	winstat(), newwin()

begin
	# Cannot join single screen or screen that is still open

	if (nscreens == 1 || TED_TABLE(scr) != NULL)
	    call err1_prompt (notnull)

	# Find the screen and its adjacent screen
	# Pay attention to which screen is on top

	jscr = 0
	do iscr = 1, nscreens {
	    if (scrlist[iscr] == scr) {
		jscr = iscr
		if (jscr == 1) {
		    win1 = TED_WINDOW(scrlist[1])
		    win2 = TED_WINDOW(scrlist[2])
		    scr2 = scrlist[2]
		} else {
		    win1 = TED_WINDOW(scrlist[jscr-1])
		    win2 = TED_WINDOW(scrlist[jscr])
		    scr2 = scrlist[jscr-1]
		}
		break
	    }
	}

	if (jscr == 0)  {
	    call err1_prompt (notfound)

	} else {
	    # Get dimensions of windows, delete them

	    top = winstat (win1, W_TOP)
	    left = winstat (win1, W_LEFT)

	    call wdimen (win1, row1, col1)
	    call delwin (win1)

	    call wdimen (win2, row2, col2)
	    call delwin (win2)

	    # Create new window, assign to adjacent screen

	    TED_WINDOW(scr2) = newwin (row1+row2, col2, top, left)

	    # Delete screen and remove from list of screens

	    call mfree (scr, TY_STRUCT)
	    nscreens = nscreens - 1

	    do iscr= jscr, nscreens
		scrlist[iscr] = scrlist[iscr+1]
	}
end

# PROMPT_WINDOW -- Return the prompt window

int procedure prompt_window ()

#--
include "window.com"

begin
	return (wprompt)
end

# SPLIT_WINDOW -- Split window into two windows

procedure split_window (scr1, scr2)

pointer	scr1		# u: current screen
pointer	scr2		# o: new screen (or NULL)
#--
include "window.com"

int	win1
int	iscr, jscr, top, left, row1, col1, row2, col2

string	noroom   "Screen is too small to split"
string	notfound "split_window: could not find screen"

int	winstat(), newwin()

begin
	# Find screen in list of screens

	jscr = 0
	do iscr = 1, nscreens {
	    if (scr1 == scrlist[iscr]) {
		jscr = iscr
		break
	    }
	}

	if (jscr == 0) {
	    call err1_prompt (notfound)

	} else {
	    # Get dimensions of current window

	    win1 = TED_WINDOW(scr1)

	    top = winstat (win1, W_TOP)
	    left = winstat (win1, W_LEFT)

	    call wdimen (win1, row1, col1)

	    row2 = row1 / 2
	    col2 = col1
	    row1 = row1 - row2

	    # Don't split window if it is too small

	    if (row2 <= MIN_HEIGHT) {
		call warn1_prompt (scr1, noroom)
		scr2 = NULL

	    } else {
		# Delete current window and create new half-size window

		call delwin (win1)
		TED_WINDOW(scr1) = newwin (row1, col1, top, left)

		# Create new screen and its window

		call malloc (scr2, TED_SCRLEN, TY_STRUCT)
		TED_WINDOW(scr2) = newwin (row2, col2, top+row1, left)
		TED_TABLE(scr2) = NULL

		# Add to list of screens

		do iscr = jscr+1, nscreens
		    scrlist[iscr+1] = scrlist[iscr]

		scrlist[jscr+1] = scr2
		nscreens = nscreens + 1
	    }
	}

end
