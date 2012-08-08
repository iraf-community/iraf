include	<ctype.h>
include	"../curses.h"

# PS_FILL -- Fill a rectangle with a single character and attribute
#
# B.Simon	19-Jan-89	Original
# B.Simon	26-Sep-90	Updated to use screen buffer
# B.Simon	31-Oct-90	Changed to skip correct lines

procedure ps_fill (source, ch, att)

int	source[RSIZE]	# i: Rectangle
int	ch		# i: Fill character
int	att		# i: Fill attribute
#--
include	"screen.com"

bool	canclear, bufflag
char	blank, fill, cell
int	dest[RSIZE]
int	ncols, nrows, irow, icol
pointer	sp, buf, scr

data	blank	/ ' ' /

bool	ps_intersect(), ttygetb()
pointer	ps_screen()

begin
	# Clip the rectangle to the screen boundary
	# If the rectangle is entirely off the screen, return

	if (! ps_intersect (source, lines, cols, dest))
	    return


	# Check for the special cases:
	# Clear entire screen (cl) and clear to end of screen (cd)

	canclear = (ch == ' ' && att == A_NORM)
	ncols = RWIDTH(dest)
	nrows = RHEIGHT(dest)

	if (canclear && ncols == cols && nrows == lines) {
	    if (ttygetb (term, "cl")) {
		call ps_updcur (1, 1)
		call ps_sendcap ("cl", nrows)
		call amovkc (blank, Memc[termscr], lines*cols) 
		return
	    }
	}
		
	if (canclear && ncols == cols && RBOT(dest) == lines) {
	    if (ttygetb (term, "cd")) {
		call ps_setcur (RTOP(dest), 1)
		call ps_sendcap ("cd", nrows)
		scr = ps_screen (RTOP(dest), 1)
		call amovkc (blank, Memc[scr], nrows*cols)
		return
	    }
	}

	# Write the rectangle a line at a time

	call smark (sp)
	call salloc (buf, cols, TY_CHAR)

	if (IS_PRINT(ch))
	    fill = ch
	else
	    fill = blank

	cell = fill + att
	bufflag = false

	canclear = canclear && ttygetb (term, "ce")

	do irow = RTOP(dest), RBOT(dest) {

	    # Check to see if line is already correct
	    # If so, skip to the next line

	    scr = ps_screen (irow, RLEFT(dest))

	    for (icol = 1; icol <= ncols; icol = icol + 1) {
		if (Memc[scr+icol-1] != cell)
		    break
	    }

	    if (icol > ncols)
		next

	    # Move cursor to beginning of line and set attribute

	    call ps_setcur (irow, RLEFT(dest))

	    if (att != A_NORM)
		call ps_sendcap ("so", 1)

	    # Special case: clear to end of line

	    if (canclear && RRIGHT(dest) == cols) {
		call ps_sendcap ("ce", 1)
		call amovkc (blank, Memc[scr], ncols)

	    } else {

		# Fill buffer with character and write to terminal

		if (! bufflag) {
		    bufflag = true
		    call amovkc (fill, Memc[buf], ncols)
		}

		# Don't write to lower right corner if screen will scroll

		if (irow == lines && RRIGHT(dest) == cols) {
		    if (ttygetb (term, "am"))
			ncols = ncols - 1
		}

		call write (ttyout, Memc[buf], ncols)
		call amovkc (cell, Memc[scr], ncols)
		call ps_updcur (irow, RRIGHT(dest) + 1)
	    }

	    # Set attribute back to normal

	    if (att != A_NORM)
		call ps_sendcap ("se", 1)

	}

	call sfree (sp)
end
