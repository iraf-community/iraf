include	"../curses.h"

# PS_SLIDE -- Slide a rectangle on the screen
#
# B.Simon	19-Jan-89	Original
# B.Simon	26-Sep-90	Updated to use screen buffer
# B.Simon	22-Mar-91	Fix error in left & right slides

bool procedure ps_slide (source, dir, dist)

int	source[RSIZE]	# i: Rectangle
int	dir		# i: Direction (from curses.h)
int	dist		# i: Distance (> 0)
#--
include	"screen.com"

char	blank
int	dest[RSIZE]
int	nrows, ncols, irow, icol
pointer	oldscr, newscr, linscr

data	blank	/ ' ' /

bool	ps_intersect(), ttygetb()
pointer	ps_screen()

begin
	if (! ps_intersect (source, lines, cols, dest))
	    return (true)

	ncols = RWIDTH(dest)
	nrows = RHEIGHT(dest)

	switch (dir) {
	case DIR_UP:
	    if (RLEFT(dest) != 1 || RRIGHT(dest) != cols)
		return (false)	    
	    if ( !(ttygetb (term, "al") && ttygetb (term, "dl")))
		return (false)

	    call ps_setcur (max (1, RTOP(dest)-dist), 1)
	    do irow = 1, dist
		call ps_sendcap ("dl", lines - RTOP(dest) + dist)

	    if (RBOT(dest) < lines) {
		call ps_setcur (RBOT(dest) + 1, 1)
		do irow = 1, dist
		    call ps_sendcap ("al", lines - RBOT(dest))
	    }

	    oldscr = ps_screen (RTOP(dest), 1)
	    newscr = ps_screen (RTOP(dest)-dist, 1)
	    do irow = 1, nrows-dist {
		if (newscr >= termscr)
		    call amovc (Memc[oldscr], Memc[newscr], ncols)
		oldscr = oldscr + cols
		newscr = newscr + cols
	    }
	    do irow = 1, dist {
		if (newscr < (termscr + lines * cols))
		    call amovkc (blank, Memc[newscr], ncols)
		newscr = newscr + cols
	    }

	case DIR_DOWN:
	    if (RLEFT(dest) != 1 || RRIGHT(dest) != cols)
		return (false)	    
	    if ( !(ttygetb (term, "al") && ttygetb (term, "dl")))
		return (false)

	    call ps_setcur (min (lines, RBOT(dest)+1), 1)
	    do irow = 1, dist
		call ps_sendcap ("dl", lines - RBOT(dest))

	    call ps_setcur (RTOP(dest), 1)
	    do irow = 1, dist
		call ps_sendcap ("al", lines - RTOP(dest))

	    oldscr = ps_screen (RBOT(dest), 1)
	    newscr = ps_screen (RBOT(dest)+dist, 1)
	    do irow = 1, nrows-dist {
		if (newscr < (termscr + lines * cols))
		    call amovc (Memc[oldscr], Memc[newscr], ncols)
		oldscr = oldscr - cols
		newscr = newscr - cols
	    }
	    do irow = 1, dist {
		if (newscr >= termscr)
		    call amovkc (blank, Memc[newscr], ncols)
		newscr = newscr - cols
	    }

	case DIR_LEFT:
	    if (! ((ttygetb (term, "ic") || ttygetb (term, "im")) &&
		    ttygetb (term, "dc")))
		return (false)

	    do irow = RTOP(dest), RBOT(dest) {

		call ps_setcur (irow, max (1, RLEFT(dest)-dist))
		call ps_sendcap ("dm", 1)
		do icol = 1, dist
		    call ps_sendcap ("dc", 1)
		call ps_sendcap ("ed", 1)

		if (RRIGHT(dest) < cols) {
		    call ps_setcur (irow, RRIGHT(dest) - dist + 1)
		    call ps_sendcap ("im", 1)
		    do icol = 1, dist {
			call ps_sendcap ("ic", 1)
			call ps_sendcap ("ip", 1)
			call putc (ttyout, blank)
		    }
		    call ps_sendcap ("ei", 1)
		    call ps_updcur (irow, RRIGHT(dest) + 1)
		}

		linscr = ps_screen (irow,1)
		oldscr = ps_screen (irow,RLEFT(dest))
		newscr = oldscr - dist

		do icol = 1, ncols-dist+1 {
		    if (newscr >= linscr)
			Memc[newscr] = Memc[oldscr]
		    oldscr = oldscr + 1
		    newscr = newscr + 1
		}

		do icol = 1, dist {
		    if (newscr < linscr + cols)
			Memc[newscr] = blank
		    newscr = newscr + 1
		}
	    }
	case DIR_RIGHT:
	    if (! ((ttygetb (term, "ic") || ttygetb (term, "im")) &&
		    ttygetb (term, "dc")))
		return (false)

	    do irow = RTOP(dest), RBOT(dest) {

		if (RRIGHT(dest) < cols - 1) {
		    call ps_setcur (irow, RRIGHT(dest) + 1)
		    call ps_sendcap ("dm", 1)
		    do icol = 1, dist
			call ps_sendcap ("dc", 1)
		    call ps_sendcap ("ed", 1)
		}

		call ps_setcur (irow, RLEFT(dest))
		call ps_sendcap ("im", 1)
		do icol = 1, dist {
		    call ps_sendcap ("ic", 1)
		    call ps_sendcap ("ip", 1)
		    call putc (ttyout, blank)
		}
		call ps_sendcap ("ei", 1)
		call ps_updcur (irow, RLEFT(dest) + dist)

		linscr = ps_screen (irow,1)
		oldscr = ps_screen (irow,RRIGHT(dest))
		newscr = oldscr + dist

		do icol = 1, ncols-dist+1 {
		    if (newscr < linscr + cols)
			    Memc[newscr] = Memc[oldscr]
		    oldscr = oldscr - 1
		    newscr = newscr - 1
		}

		do icol = 1, dist {
		    if (newscr >= linscr)
			Memc[newscr] = blank
		    newscr = newscr - 1
		}
	    }
	default:
	    return (false)
	}

	return (true)
end
