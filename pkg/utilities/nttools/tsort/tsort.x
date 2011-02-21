include <tbset.h>
define	SYNTAX		1

# TSORT -- Sort a table on several columns at once
#
# This task sorts an SDAS format table. The column or columns to sort on are
# given by the parameter columns, which is a list of column names or column
# name patterns separated by commas. The most significant column name is the
# first in the list, subsequent columns are used to break ties. There are two
# flags, ascend and casesens. If ascend is true, the first row in the output
# table holds the smallest value if the sorted column is numeric or the first
# string in alpahabetic order if the sorted column is a character string. If
# casesens is true, upper case characters precede lower case charaters in sort
# order. Otherwise, case is not significant in determining the sort order.
#
# B.Simon	25-Sep-87	First Code
# B.Simon	15-Jul-88	Set buffer size to max
# Phil Hodge	 7-Sep-88	Change parameter name for table.
# Phil Hodge	14-Nov-88	Set advice to sequential instead of
#					setting buffer size.
# B.Simon	05-Feb-90	Use new internal sort routine
# B.Simon	21-Feb-91	Use external sort routine
# Phil Hodge	 4-Oct-95	Use table name template routines tbnopenp, etc.

procedure t_tsort()

pointer	table			# Names of the tables to be sorted
pointer	columns			# Column name template
int	maxrow			# Maximum number of rows to sort internally
bool	ascend			# Ascending sort flag
bool	casesens		# Case sensitivity flag
#--
int	numcol, nrow, numptr
pointer sp, tp, colptr, list

bool	clgetb()
int	tbnget()
pointer tbnopenp()
int	tbpsta()
int	tbl_maxrow()
pointer	tbtopn()

begin
	# Allocate stack memory for strings

	call smark (sp)
	call salloc (table, SZ_FNAME, TY_CHAR)
	call salloc (columns, SZ_LINE, TY_CHAR)

	# Read the task parameters

	list = tbnopenp ("table")
	call clgstr ("columns", Memc[columns], SZ_LINE)
	ascend = clgetb ("ascend")
	casesens = clgetb ("casesens")

	# Loop over all table names in the file name template

	while (tbnget (list, Memc[table], SZ_FNAME) != EOF) {

	    # Open the table

	    tp = tbtopn (Memc[table], READ_WRITE, NULL)

	    # Set buffer size to a larger value.

	    call tbpset (tp, TBL_ADVICE, SEQUENTIAL)

	    # Create an array of column pointers from the column template

	    numcol = tbpsta (tp, TBL_NCOLS)
	    nrow = tbpsta (tp, TBL_NROWS)

	    call malloc (colptr, numcol, TY_INT)
	    call tctexp (tp, Memc[columns], numcol, numptr, Memi[colptr])

	    if (numptr == 0)
		call error (SYNTAX, "Column(s) not found in table")

	    # Choose between the internal and external sort routines
	    # Table is closed by sort routine

	    maxrow = tbl_maxrow (tp)
	    if (nrow <= maxrow)
		call tbl_intsort (tp, numptr, Memi[colptr], casesens, ascend)
	    else
		call tbl_extsort (tp, numptr, Memi[colptr], maxrow, 
				  casesens, ascend)

	    call mfree (colptr, TY_INT)

	}

	# Close the filename template list

	call tbnclose (list)
	call sfree (sp)
end
