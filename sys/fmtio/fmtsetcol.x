# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	<printf.h>

# FMT_SETCOL -- Called when a control character is output, to keep track of
# the column index during output, for the "%nt" (tabulate) format.  Columns
# are indexed from the start of the printf, rather than in absolute units on
# the output, unless a \r or \n is output during the print.

procedure fmt_setcol (ch, col)

char	ch
int	col

begin
	switch (ch) {
	case '\t':					# next tab stop
	    col = ((col + TABSTOP-1) / TABSTOP) * TABSTOP + 1
	case '\n', '\r', '\f':
	    col = 1
	case '\b':
	    col = col - 1
	default:
	    if (IS_PRINT (ch))
		col = col + 1
	}
end
