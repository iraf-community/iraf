# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# STRTBL -- Print a list of strings on the named file.  If NCOL is zero,
# the maximum number of columns is calculated based on the maximum
# string length.  If NCOL is nonzero, it is taken to be the maximum
# number of columns (the actual number may be less, depending on the
# maximum string length).  FIRST_COL and LAST_COL define where on the
# page the table will be placed.

procedure strtbl (fd, buf, strp, nstr, first_col, last_col, maxch, ncol)

int	fd			# output file
char	buf[ARB]		# buffer containing the strings
int	strp[ARB]		# array of string pointers
int	nstr			# number of strings
int	first_col, last_col	# where to place table on a line
int	maxch			# maximum chars to print from a string
int	ncol			# desired number of columns (0 to autoscale)

pointer	sp, obuf, op
int	row, i, j, p, nspaces, maxlen, colwidth, numcol, numrow, str
int	strlen()

begin
	call smark (sp)
	call salloc (obuf, last_col + 1, TY_CHAR)

	maxlen = 0
	do i = 1, nstr
	    maxlen = max (maxlen, strlen(buf[strp[i]]))
	if (maxch > 0)
	    maxlen = min (maxch, maxlen)
	numcol = max (1, (last_col - first_col + 1) / (maxlen + 2))

	if (ncol > 0)
	    numcol = min (numcol, ncol)
	colwidth = (last_col - first_col + 1) / numcol
	numrow = (nstr + numcol-1) / numcol 

	# For each row in the table:
	do row = 1, numrow {
	    op = obuf

	    # Space to the first column.
	    do i = 2, first_col {
		Memc[op] = ' '
		op = op + 1
	    }

	    # For each string in the row:
	    do i = 1, numcol {
		str = row + (i-1) * numrow
		if (str > nstr)
		    next
		p = strp[str]

		# Output the string.
		for (j=0;  buf[p+j] != EOS && j < maxlen;  j=j+1) {
		    Memc[op] = buf[p+j]
		    op = op + 1
		}

		# Advance to the next column.
		if (i < numcol) {
		    nspaces = max (2, colwidth - j)
		    for (j=1;  j <= nspaces;  j=j+1) {
			Memc[op] = ' '
			op = op + 1
		    }
		}
	    }

	    # Terminate this row of the table.
	    Memc[op] = '\n'
	    op = op + 1
	    Memc[op] = EOS
	    call putline (fd, Memc[obuf])
	}

	call sfree (sp)
end
