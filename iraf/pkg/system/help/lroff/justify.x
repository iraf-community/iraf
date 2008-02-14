# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<chars.h>
include	"lroff.h"

# RIGHT_JUSTIFY -- Right justify the text string argument in LINEBUF on the
# next input line, and write to the output.

procedure right_justify (in, out, linebuf, ip)

extern	in(), out()
char	linebuf[ARB]
int	ip
pointer	sp, rjbuf
int	in(), input()
errchk	salloc, breakline, input, rjline
include	"lroff.com"

begin
	call smark (sp)
	call salloc (rjbuf, SZ_IBUF, TY_CHAR)

	call breakline (out, NJ)
	if (input (in, Memc[rjbuf]) != EOF)
	    call rjline (out, Memc[rjbuf], linebuf[ip])

	call sfree (sp)
end



# RJLINE -- Right justify a text string on an unfilled input line and
# send it out.

procedure rjline (out, input_line, rjtext)

extern	out()
char	input_line[ARB]		# unfilled input line
char	rjtext[ARB]		# string to be right justified on same line

int	i, nblanks, len_rjtext, next_output_column
int	textlen()
errchk	outstr, outc, outline
include	"lroff.com"

begin
	# Breakline should already have been called by the time we get here.
	# Output the input line at the left margin without filling.

	call outstr (out, input_line)

	# Determine the (printable) length of the rjtext string, and space
	# over so that it comes out right justified.  Always output at least
	# one space.  Exceed the right margin if necessary.

	call getoutcol (next_output_column)
	len_rjtext = textlen (rjtext)
	nblanks = max (1, right_margin - (next_output_column + len_rjtext) + 1)
	do i = 1, nblanks
	    call outc (out, BLANK)

	call outline (out, rjtext)
end
