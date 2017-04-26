include "../curses.h"

# WSLIDE -- Slide a window's rectangle on the screen
#
# B.Simon	25-Sep-90	Original

procedure wslide (source, dir, dist)

int	source[RSIZE]	# i: Rectangle
int	dir		# i: Direction (from display.h)
int	dist		# i: Distance (> 0)
#--
int	blank, dest[RSIZE], rect[RSIZE]
int	maxcol, maxrow, ncols, irow, icol
pointer	sp, buffer, oldscr

data	blank	/ ' ' /

bool	ps_slide(), ps_intersect()
int	ps_width(), ps_height()
pointer	ps_screen()

begin
	# First try to slide the rectangle with ps_slide
	# (move by using insert and delete control sequences)

	if (ps_slide (source, dir, dist))
	    return

	# If this doesn't work, redraw the rectangle from
	# the screen buffer using ps_wrtcells and ps_fill

	# The left and write scrolls must first be written
	# to a buffer to avoid the problem with array overlap
	# when updating the screen buffer

	maxcol = ps_width ()
	maxrow = ps_height ()
	if (! ps_intersect (source, maxrow, maxcol, dest))
	    return

	call smark(sp)
	ncols = RWIDTH(dest)
	call salloc (buffer, ncols+dist, TY_CHAR)

	switch (dir) {
	case DIR_UP:
	    oldscr = ps_screen (RTOP(dest), RLEFT(dest))
	    do irow = RTOP(dest), RBOT(dest)-dist {
		call ps_wrtcells (irow-dist, RLEFT(dest), Memc[oldscr], ncols)
		oldscr = oldscr + maxcol
	    }

	    RASG (rect, max(RTOP(dest), RBOT(dest)-dist+1), RLEFT(dest),
		  RBOT(dest), RRIGHT(dest))
	    call ps_fill (rect, blank, A_NORM)

	case DIR_DOWN:
	    oldscr = ps_screen (RBOT(dest), RLEFT(dest))
	    do irow = RBOT(dest), RTOP(dest)+dist, -1 {
		call ps_wrtcells (irow+dist, RLEFT(dest), Memc[oldscr], ncols)
		oldscr = oldscr - maxcol
	    }

	    RASG (rect, RTOP(dest), RLEFT(dest),
		  min(RBOT(dest), RTOP(dest)+dist-1), RRIGHT(dest))
	    call ps_fill (rect, blank, A_NORM)

	case DIR_LEFT:
	    icol = RLEFT(dest) - dist
	    oldscr = ps_screen (RTOP(dest), RLEFT(dest))
	    do irow = RTOP(dest), RBOT(dest) {
		call amovc (Memc[oldscr], Memc[buffer], ncols)
		call amovkc (blank, Memc[buffer+ncols], ncols-dist)

		call ps_wrtcells (irow, icol, Memc[buffer], ncols)
		oldscr = oldscr + maxcol
	    }
	case DIR_RIGHT:
	    icol = RLEFT(dest) + dist
	    oldscr = ps_screen (RTOP(dest), RLEFT(dest))
	    do irow = RTOP(dest), RBOT(dest) {
		call amovc (Memc[oldscr], Memc[buffer], ncols)

		call ps_wrtcells (irow, icol, Memc[buffer], ncols)
		oldscr = oldscr + maxcol
	    }
	}

	call sfree (sp)
end
