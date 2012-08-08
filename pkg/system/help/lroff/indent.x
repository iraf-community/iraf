# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"lroff.h"

# INDENT_LEFT_MARGIN -- Execute a relative indent of the left margin.

procedure indent_left_margin (in, out, number_of_spaces)

extern	in(), out()
int	in(), number_of_spaces
include	"lroff.com"

begin
	call breakline (out, NJ)
	left_margin = max (perm_left_margin, min (right_margin,
	    left_margin + number_of_spaces))
end
