# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<printf.h>

# PARGSTR -- Pass a string type operand to printf.

procedure pargstr (str)

char	str[ARB]
int	maxch
include "fmt.com"

begin
	call fmt_read()					# get format

	if (decpl == USE_DEFAULT)
	    maxch = SZ_OBUF
	else
	    maxch = abs (decpl)

	if (width == USE_DEFAULT)
	    width = 0

	call fmtstr (fd, str, col, fill_char, left_justify, maxch, width)
	call fpradv ()
end
