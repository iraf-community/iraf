# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>

# FMTSTR -- Place a string in a field of the given width, left or right
# justifying as indicated, and output to the named file.  The length of
# the text string may exceed the field width, in which case there is no
# filling.

procedure fmtstr (fd, str, col, fill_char, left_justify, maxch, width)
		    
int	fd			# output file
char	str[ARB]		# string to be output
int	col			# column: both input and output parameter
char	fill_char		# fill character, if right justify
int	left_justify		# YES or NO
int	maxch			# maximum string chars to output
int	width			# field width
int	nchars, nfill, ip
int	strlen()
errchk	putc, putci

begin
	if (fd <= 0)
	    return

	if (width > 0) {
	    nchars = min (maxch, strlen(str))
	    nfill = max (0, width - nchars)
	} else {
	    nchars = maxch
	    nfill = 0						# free format
	}

	if (left_justify == NO)					# fill at left
	    for (col=col+nfill;  nfill > 0;  nfill=nfill-1)
		call putc (fd, fill_char)

	for (ip=1;  str[ip] != EOS && ip <= nchars;  ip=ip+1) {	# put string
	    call putc (fd, str[ip])
	    if (IS_PRINT (str[ip]))
		col = col + 1
	    else
		call fmt_setcol (str[ip], col)
	}

	for (col=col+nfill;  nfill > 0;  nfill=nfill-1)		# fill at right
	    call putci (fd, ' ')
end
