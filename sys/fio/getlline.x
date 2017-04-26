# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GETLLINE -- Get a logical line of text, i.e., an arbitrarily long line
# possibly broken up into multiple segments of size SZ_LINE.  Accumulation
# stops when the indicated number of chars have been read, or newline is
# detected.  MAXCH must be at least SZ_LINE characters greater than the
# longest line to be read.

int procedure getlline (fd, obuf, maxch)

int	fd			#I input file
char	obuf[ARB]		#O output buffer
int	maxch			#I max chars out, >= SZ_LINE

int	op, status
int	getline()
errchk	getline

begin
	op = 1

	while (maxch - op + 1 >= SZ_LINE) {
	    # Get next physical line from the file.
	    status = getline (fd, obuf[op])
	    if (status == EOF) {
		if (op == 1)
		    return (EOF)
		else
		    return (op - 1)
	    } else
		op = op + status

	    # If the last physical line read ends in a newline we are done.
	    # If no newline we get another line, thereby reconstructing long
	    # lines broken by the SZ_LINE limit of getline().

	    if (obuf[op-1] == '\n')
		break
	}

	return (op - 1)
end
