# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<chars.h>
include	"lroff.h"

# CENTER_TEXT -- Center and output the next input line within the current
# left and right margins.  The optional argument specifies the column
# (measured from the left margin) at which the text is to be centered.

procedure center_text (in, out, linebuf, ip)

extern	in(), out()
char	linebuf[ARB]
int	ip

int	len_inputline, center_column, nblanks, i
int	in(), input(), lgetarg()
errchk	breakline, input, outc, outline
include	"lroff.com"

begin
	call breakline (out, NJ)
	center_column = lgetarg (linebuf, ip, (left_margin + right_margin) / 2)
	len_inputline = input (in, linebuf) - 1

	if (len_inputline != EOF) {
	    nblanks = center_column - (len_inputline / 2) - left_margin
	    for (i=1;  i <= nblanks;  i=i+1)
		call outc (out, BLANK)
	    call outline (out, linebuf)
	}
end
