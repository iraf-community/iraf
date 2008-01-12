define	SYNTAX		   1

# DOQUERY -- Perform a query on a table and return row and column arrays
#
# B.Simon	18-Dec-1987	First Code
# B.Simon	10-Aug-1992	Fixed calling sequence to tbl_sort
# Phil Hodge	18-Aug-2003	Call select before calling unique.

procedure doquery (tp, expr, columns, sort, uniq, ascend, casesens,
		   numcol, colptr, nindex, index)

int	tp		#  i: Input table descriptor
char	expr[ARB]	#  i: Expression used to select rows
char	columns[ARB]	#  i: Table column template
char	sort[ARB]	#  i: Sort columns template
bool	uniq		#  i: Should output rows be unique?
bool	ascend		#  i: Ascending sort flag
bool	casesens	#  i: Case sensitivity flag
int	numcol		# io: Number of column pointers
pointer	colptr[ARB]	# io: Array of column pointers
int	nindex		# io: Number of row indices
int	index[ARB]	# io: Array of row indices
#--
int	numptr, numsort
pointer	sortptr

string	nocolumn "Column names not found in table"
string	nosort "Sort column not found in table"

bool	isblank()

begin

	# Create an array of column pointers from the column template

	call tctexp (tp, columns, numcol, numptr, colptr)

	if (numptr == 0)
	    call error (SYNTAX, nocolumn)

	# Select rows according to expression

	if (! isblank(expr)) {
	    call select (tp, expr, nindex, index)
	}

	# Remove duplicate rows from table

	if (uniq)
	    call unique (tp, numptr, colptr, nindex, index)

	# Sort the array of indices

	if (! isblank(sort)) {

	    # Create an array of sort column pointers from the sort template

	    call malloc (sortptr, numcol, TY_INT)
	    call tctexp (tp, sort, numcol, numsort, Memi[sortptr])

	    if (numsort == 0)
		call error (SYNTAX, nosort)

	    call tbl_sort (ascend, casesens, tp, numsort, Memi[sortptr], 
			   nindex, index)

	    call mfree (sortptr, TY_INT)
	}

	numcol = numptr

end
