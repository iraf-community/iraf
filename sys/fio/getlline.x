# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GETLONGLINE -- Get a long line, i.e., a logical line possibly spanning
# several physical lines with the newlines escaped at the ends.  Skip
# comment lines and .help sections.  Blank lines are not skipped.

int procedure getlongline (fd, obuf, maxch, linenum)

int	fd			# input file
char	obuf[ARB]		# output buffer
int	maxch
int	linenum

int	op, status
int	getline(), strncmp()
errchk	getline

begin
	op = 1

	while (maxch - op + 1 >= SZ_LINE) {
	    # Get next non-comment line.
	    repeat {
		status = getline (fd, obuf[op])
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

	    if (obuf[op-2] == '\\' && obuf[op-1] == '\n')
		op = op - 2
	    else
		break
	}

	return (op - 1)
end
