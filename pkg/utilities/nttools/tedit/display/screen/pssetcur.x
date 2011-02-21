define	ABSOLUTE	YES	# disable relative cursor motion if YES

# PS_SETCUR -- Set the cursor position
#
# B.Simon	19-Jan-89	Original
# B.Simon	19-Dec-90	Rewritten to speed cursor motion
# B.Simon	10-Apr-91	Add switch to disable relative motion

procedure ps_setcur (row, col)

int	row		# i: Cursor row
int	col		# i: Cursor column
#--
include "screen.com"

bool	moved
int	newrow, newcol, dr, dc

bool	ttygetb()

begin
	newrow = min (lines, max (1, row))
	newcol = min (cols, max (1, col))

	moved = true
	dr = newrow - currow
	dc = newcol - curcol

	if (ABSOLUTE == YES) {
	    if (dr != 0 || dc != 0)
		moved = false

	} else if (dr == 0) {
	    if (dc == 0) { # no move
		return

	    } else if (dc == 1) { # move right by one
		if (ttygetb (term, "nd"))
		    call ps_sendcap ("nd", 1)
		else
		    moved = false

	    } else if (dc == -1) { # move left by one
		if (ttygetb (term, "le"))
		    call ps_sendcap ("le", 1)
		else
		    moved = false

	    } else {
		moved = false
	    }

	} else if (dr == 1) {
	    if (dc == 0) { # move down by one
		if (ttygetb (term, "do"))
		    call ps_sendcap ("do", 1)
		else
		    moved = false

	    } else {
		moved = false
	    }

	} else if (dr == -1) {
	    if (dc == 0) { # move up by one
		if (ttygetb (term, "up"))
		    call ps_sendcap ("up", 1)
		else
		    moved = false
	    } else {
		moved = false
	    }

	} else {
	    moved = false
	}

	if (! moved) # must use absolute move
	    call ttygoto (ttyout, term, newcol, newrow)

	# Update current cursor position

	currow = newrow
	curcol = newcol
end

# PS_UPDCUR -- Update the cursor position

procedure ps_updcur (row, col)

int	row		# i: New cursor row
int	col		# i: New cursor column
#--
include "screen.com"

bool	ttygetb()

begin
	if (row >= lines) {
	    currow = lines 
	} else {
	    currow = row
	}

	if (col >= cols) {
	    if (ttygetb (term, "am")) {
		currow = min (row + 1, lines)
		curcol = 1
	    } else {
		curcol = cols
	    }

	} else {
	   curcol = col
	}

end
