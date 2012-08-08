include	<ctype.h>
include	"../curses.h"

# PS_WRITE -- Write a string on the physical screen
#
# B.Simon	18-Jan-89	Original
# B.Simon	26-Sep-90	Updated to use screen buffer

procedure ps_write (row, col, ncols, str, att)

int	row		# i: Starting row
int	col		# i: Starting column
int	ncols		# i: Number of columns to write
char	str[ARB]	# i: String to write
int	att		# i: Attribute
#--
include	"screen.com"

char	blank
int	colstart, colend, idxstart, idxend, idx
pointer	scr

data	blank	/ ' ' /

bool	ttygetb()
int	strlen()
pointer	ps_screen()

begin
	# Don't try to print if string is entirely off the screen

	if (row < 1 || row > lines || col > cols)
	    return

	# Compute the portion of the string that is on the screen

	colstart = max (col, 1)
	colend = min (ncols, strlen(str)) + col - 1
	colend = min (colend, cols)

	if (colend == cols && row == lines) {
	    if (ttygetb (term, "am"))
		colend = colend - 1
	}

	if (colend < colstart)
	    return

	idxstart = colstart - col + 1
	idxend = colend - col + 1

	# Move the cursor to the position of the first printed character

	call ps_setcur (row, colstart)

	# Print the string with the proper attribute
	# All non-printing characters are printed as blanks

	if (att != A_NORM)
	    call ps_sendcap ("so", 1)

	scr = ps_screen (row, colstart)
	do idx = idxstart, idxend {
	    if (IS_PRINT(str[idx])) {
		call putc (ttyout, str[idx])
		Memc[scr] = str[idx] + att
	    } else {
		call putc (ttyout, blank)
		Memc[scr] = blank + att
	    }
	    scr = scr + 1
	}

	if (att != A_NORM)
	    call ps_sendcap ("se", 1)

	call ps_updcur (row, colend + 1)

end
