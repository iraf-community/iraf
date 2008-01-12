# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"lroff.h"

# SKIPLINES -- Skip one or more lines on the output, i.e., break the current
# output line and add a few empty lines.

procedure skiplines (out, nlines)	

extern	out()
int	nlines, i
errchk	breakline, outc

begin
	call breakline (out, NJ)

	do i = 1, nlines
	    call outc (out, '\n')
end
