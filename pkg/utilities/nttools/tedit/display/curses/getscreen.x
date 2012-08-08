include	"../curses.h"

# GETSCREEN -- Retrieve screen contents into a window's buffer
#
# B.Simon	26-Sep-90	Original

procedure getscreen (source, buffer)

int	source[RSIZE]	# i: Rectangle to be retrieved
pointer	buffer		# o: Buffer (allocated by this routine)
#--

int	dest[RSIZE]
int	maxcol, maxrow, ncols, nrows, irow
pointer	buf, scr

bool	ps_intersect()
int	ps_width(), ps_height()
pointer	ps_screen()

begin
	# Clip the rectangle to the screen boundary
	# If the rectangle is entirely off the screen, return

	buffer = NULL
	maxcol = ps_width ()
	maxrow = ps_height ()
	if (! ps_intersect (source, maxrow, maxcol, dest))
	    return

	# Allocate buffer to hold screen contents

	ncols = RWIDTH(dest)
	nrows = RHEIGHT(dest)
	call malloc (buffer, ncols*nrows, TY_CHAR)

	# Copy screen contents to buffer

	buf = buffer
	scr = ps_screen (RTOP(dest), RLEFT(dest))

	do irow = RTOP(dest), RBOT(dest) {
	    call amovc (Memc[scr], Memc[buf], ncols)
	    scr = scr + maxcol
	    buf = buf + ncols
	}

end
