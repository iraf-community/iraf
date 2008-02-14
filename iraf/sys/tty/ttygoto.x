# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<chars.h>
include	"tty.h"

# TTYGOTO -- Position the cursor to the given absolute screen coordinates.
# We do not require any knowledge of the current screen position; many programs
# will wish to do cursor positioning at a lower level to take advantage of
# knowledge of the current position.

procedure ttygoto (fd, tty, col, line)

int	fd			# output stream
pointer	tty			# termcap descriptor
int	col			# destination column
int	line			# destination line

pointer	sp, cm_in, cm_out
int	coords[2], nchars, i
int	ttygets(), ttysubi()
errchk	salloc, ttygets, ttyputs, ttywrite

begin
	call smark (sp)
	call salloc (cm_in, SZ_CTRLSTR, TY_CHAR)
	call salloc (cm_out, SZ_LINE, TY_CHAR)

	# Use the cursor motion control string to position cursor, if the
	# terminal has such a capability.  Otherwise try to use primitive
	# motion commands.

	if (ttygets (tty, "cm", Memc[cm_in], SZ_CTRLSTR) > 0) {
	    # Coordinates are ordered line,col in the termcap entry,
	    # whereas our args are ordered col,line (e.g., x,y).  Store
	    # in "coords" in the termcap order.

	    coords[1] = line
	    coords[2] = col
	    nchars = ttysubi (Memc[cm_in], Memc[cm_out], SZ_LINE, coords, 2)

	    # Output cursor motion control string to file.
	    call ttywrite (fd, tty, Memc[cm_out], nchars, 1)

	    # If unable to position to the exact coordinates, use primitive
	    # motion commands to step the rest of the way in.  Do not abort
	    # if unable to do so.

	    if (coords[2] > 0) {
		# Presumably we do not have to step in very often, so we do
		# nothing special to buffer the backspace control sequence.
		# Most terminals recognize BS in any case.

		if (T_BSOK(tty) == YES) {
		    Memc[cm_out] = BS
		    Memc[cm_out+1] = EOS
		} else if (ttygets (tty, "bc", Memc[cm_out], SZ_CTRLSTR) == 0)
		    Memc[cm_out] = EOS

		for (i=coords[2];  i > 0;  i=i-1)
		    call ttyputs (fd, tty, Memc[cm_out], 1)
	    }

	    # Now adjust vertical position if necessary.
	    if (coords[1] > 0) {
		if (ttygets (tty, "up", Memc[cm_out], SZ_CTRLSTR) > 0)
		    for (i=coords[1];  i > 0;  i=i-1)
			call ttyputs (fd, tty, Memc[cm_out], 1)
	    }

	} else {
	    # Terminal has no nifty cursor addressing capability; add code here
	    # to position cursor by generating a sequence of more primitive
	    # codes.  Not going to bother with this for now.
	    ;
	}

	call sfree (sp)
end
