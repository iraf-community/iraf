# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GETLONGLINE -- Get a long line, i.e., a logical line possibly spanning
# several physical lines with the newlines escaped at the ends.  Skip
# comment lines and .help sections.  Blank lines are not skipped.
# Lines not terminated by newlines are joined to form a longer line.
# MAXCH must be at least SZ_LINE characters greater than the size of the
# longest line to be read.

int procedure getlongline (fd, obuf, maxch, linenum)

int	fd			#I input file
char	obuf[ARB]		#O output buffer
int	maxch			#I max chars out
int	linenum			#U line number counter

int	op, status
int	getline(), strncmp()
errchk	getline

begin
	op = 1

	while (maxch - op + 1 >= SZ_LINE) {
	    # Get next non-comment line.
	    repeat {
		status = getline (fd, obuf[op])
		if (status > 0 && obuf[op+status-1] == '\n')
		    linenum = linenum + 1

		if (status == EOF) {
		    break
		} else if (obuf[op] == '#') {
		    next
		} else if (obuf[op] == '.') {
		    # Skip help sections.
		    if (strncmp (obuf[op], ".help", 5) == 0) {
			repeat {
			    status = getline (fd, obuf[op])
			    linenum = linenum + 1
			    if (status == EOF)
				break
			    if (strncmp (obuf[op], ".endhelp", 8) == 0)
				break
			}
		    } else
			break
		} else
		    break
	    }

	    if (status == EOF) {
		if (op == 1)
		    return (EOF)
		else
		    return (op - 1)
	    } else
		op = op + status

	    # If the last physical line read ends in a newline we are done,
	    # unless the newline is escaped.  If there is no newline we get
	    # another line, thereby reconstructing long lines broken by the
	    # SZ_LINE limit of getline().

	    if (obuf[op-1] == '\n')
		if (obuf[op-2] == '\\')
		    op = op - 2
		else
		    break
	}

	return (op - 1)
end
