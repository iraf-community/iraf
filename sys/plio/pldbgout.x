# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# PL_DEBUGOUT -- Output formatter for PL package debug output.  Output as
# many text items as will fit on a line, inserting one space between each
# text item.  A call with BUF=EOS terminates the sequence with a newline.

procedure pl_debugout (fd, buf, col, firstcol, maxcol)

int	fd			#I output stream
char	buf[ARB]		#I text to be output
int	col			#I next column of output
int	firstcol		#I first column to write to
int	maxcol			#I last column to write to

int	nchars
int	strlen()

begin
	nchars = min (maxcol-firstcol+1, strlen(buf))

	if (nchars == 0) {
	    # Terminate the sequence with a newline.
	    call fprintf (fd, "\n")
	    col = 1
	    return

	} else if (col + nchars > maxcol) {
	    # Break line and output token.
	    call fprintf (fd, "\n")

	    for (col=1;  col < firstcol;  col=col+1)
		call putci (fd, ' ')

	} else {
	    # Append to the current line.
	    if (col <= firstcol) {
		for (;  col < firstcol;  col=col+1)
		    call putci (fd, ' ')
	    } else {
		call putci (fd, ' ')
		col = col + 1
	    }
	}

	call putline (fd, buf)
	col = col + nchars
end
