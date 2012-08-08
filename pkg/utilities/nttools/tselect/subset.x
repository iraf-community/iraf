include <tbset.h>
define	SYNTAX		1

# SUBSET -- Select subset of table rows
#
# This procedure evaluates a boolean expession for selected rows in a table.
# If the expression is true, it is written to the output table
#
# B.Simon	 7-Oct-87	First Code
# B.Simon	16-Dec-87	Changed to handle table subsets
# B.Simon	06-Jan-93	Changed to use ftnexpr
# B.Simon	25-Aug-98	Changed to write directly to output table

procedure subset (itp, otp, expr)

pointer	itp		# i: Input table descriptor
pointer	otp		# o: Output table descriptor
char	expr[ARB]	# i: Algebraic expression used in subset
#--
char	nl
pointer	sp, newexp, ch
int	fd, sd, ic, irow, orow, first, last

int	open(), stropen(), stridx(), tbpsta(), tbl_search()

data	nl	/ '\n' /
string	badtype	"Expression is not valid"

errchk	open, stropen, tbl_search

begin
	# Allocate dynamic memory for strings

	call smark (sp)
	call salloc (newexp, SZ_COMMAND, TY_CHAR)

	# Check to see if the expression is a file name

	if (expr[1] != '@') {
	    # Copy the expression into string

	    call strcpy (expr, Memc[newexp], SZ_COMMAND)

	} else {
	    # Copy the file into a string

	    fd = open (expr[2], READ_ONLY, TEXT_FILE)
	    sd = stropen (Memc[newexp], SZ_COMMAND, WRITE_ONLY)
	    call fcopyo (fd, sd)
	    call close (fd)
	    call strclose (sd)

	    # Replace the newlines with blanks

	    ch = newexp
	    repeat {
		ic = stridx (nl, Memc[ch])
		if (ic == 0)
		    break
		ch = ch + ic
		Memc[ch-1] = ' '
	    }
	}

	orow = 1
	first = 1
	last = tbpsta (itp, TBL_NROWS)

	while (first <= last) {
	    irow = tbl_search (itp, Memc[newexp], first, last)
	    if (irow < 1)
		break

	    call tbrcpy (itp, otp, irow, orow)
	    first = irow + 1
	    orow = orow + 1
	}

	if (irow == ERR)
	    call error (SYNTAX, badtype)

	call sfree (sp)
end
