include	"reloperr.h"

# SELECT -- Select table rows according to expression
#
# This procedure evaluates a boolean expession for selected rows in a table.
# If the expression is true and does not involve null elements, the index
# of that row is kept in the index array.
#
# B.Simon	 7-Oct-87	First Code
# B.Simon	16-Dec-87	Changed to handle table subsets
# B.Simon	06-Jan-93	Changed to use ftnexpr

procedure select (tp, expr, nindex, index)

pointer	tp		#  i: Table descriptor
char	expr[ARB]	#  i: Algebraic expression used in selection
int	nindex		# io: Number of rows selected
int	index[ARB]	# io: Indices of selected rows
#--
char	ch
pointer	sp, oldexp, newexp, ic, aryptr, nulptr
int	fd, sd, jc, dtype, nary, iary

int	open(), stropen(), stridx()

errchk	open, stropen, tbl_eval

string	badtype	"Expression is not boolean"

begin
	# Allocate dynamic memory for strings

	call smark (sp)
	call salloc (oldexp, SZ_COMMAND, TY_CHAR)
	call salloc (newexp, SZ_COMMAND, TY_CHAR)

	# Check to see if the expression is a file name

	if (expr[1] == '@') {

	    # Copy the file into a string

	    fd = open (expr[2], READ_ONLY, TEXT_FILE)
	    sd = stropen (Memc[oldexp], SZ_COMMAND, WRITE_ONLY)
	    call fcopyo (fd, sd)
	    call close (fd)
	    call strclose (sd)

	    # Replace the newlines with blanks

	    ic = oldexp
	    ch = '\n'
	    repeat {
		jc = stridx (ch, Memc[ic])
		if (jc == 0)
		    break
		ic = ic + jc
		Memc[ic-1] = ' '
	    }

	    # Convert Fortran relational operators to SPP

	    call ftnexpr (Memc[oldexp], Memc[newexp], SZ_COMMAND)

	} else {

	    # Convert Fortran relational operators to SPP

	    call ftnexpr (expr, Memc[newexp], SZ_COMMAND)
	}
	    
	# Evaluate the expression

	dtype = TY_BOOL
	call tbl_eval (tp, nindex, index, Memc[newexp], dtype, aryptr, nulptr)

	# Check to see if result is boolean

	if (dtype != TY_BOOL) {
	    call mfree (aryptr, dtype)
	    call mfree (nulptr, TY_BOOL)
	    call error (SYNTAX, badtype)
	}

	# Put indices of true, non-null rows in index array

	nary = nindex
	nindex = 0
	do iary = 1, nary

	    if (Memb[aryptr+iary-1] && ! Memb[nulptr+iary-1]) {
		nindex = nindex + 1
		index[nindex] = index[iary]
	    }

	call mfree (aryptr, dtype)
	call mfree (nulptr, TY_BOOL)
	call sfree (sp)
end
