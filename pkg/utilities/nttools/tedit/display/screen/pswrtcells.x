include	<ctype.h>
include	"../curses.h"

# PS_WRTCELLS -- Write a vector of cells
#
# B.Simon	19-Jan-89	Original
# B.Simon	26-Sep-90	Updated to use screen buffer

procedure ps_wrtcells (row, col, vector, ncols)

int	row		# i: Starting row
int	col		# i: Starting column
char	vector[ARB]	# i: Vector to write
int	ncols		# i: Number of columns to write
#--
include	"screen.com"

char	blank, att, ch
int	colstart, colend, idxstart, idxend, idx
pointer	scr

data	blank	/ ' ' /

bool	ttygetb()
pointer	ps_screen()

begin
	# Don't try to print if vector is entirely off the screen

	if (row < 1 || row > lines || col > cols)
	    return

	# Compute the portion of the vector that is on the screen

	colstart = max (col, 1)
	colend = min (ncols + col - 1, cols)

	if (colend == cols && row == lines) {
	    if (ttygetb (term, "am"))
		colend = colend - 1
	}

	idxstart = colstart - col + 1
	idxend = colend - col + 1

	# Adjust string start and end so that portions that
	# duplicate the current screen contents are not printed

	scr = ps_screen (row, colend)
	while (idxend >= idxstart) {
	    if (vector[idxend] != Memc[scr])
		break

	    colend = colend - 1
	    idxend = idxend - 1
	    scr = scr - 1
	}

	scr = ps_screen (row, colstart)
	while (idxstart <= idxend) {
	    if (vector[idxstart] != Memc[scr])
		break

	    colstart = colstart + 1
	    idxstart = idxstart + 1
	    scr = scr + 1
	}

	if (colend < colstart)
	    return

	# Move the cursor to the position of the first printed character

	call ps_setcur (row, colstart)

	# Print the vector

	att = 0
	do idx = idxstart, idxend {

	    # Set the proper attribute

	    if (vector[idx] < A_STANDOUT) {
		ch = vector[idx]
		if (att != 0) {
		    att = 0
		    call ps_sendcap ("se", 1)
		}
	    } else {
		ch = vector[idx] - A_STANDOUT
		if (att == 0) {
		    att = vector[idx] - ch
		    call ps_sendcap ("so", 1)
		}
	    }

	    # Print non-printing character as blank

	    if (IS_PRINT(ch)) {
		call putc (ttyout, ch)
		Memc[scr] = ch + att
	    } else {
		call putc (ttyout, blank)
		Memc[scr] = blank + att
	    }
	    scr = scr + 1
	}

	if (att != 0)
	    call ps_sendcap ("se", 1)

	call ps_updcur (row, colend + 1)

end
