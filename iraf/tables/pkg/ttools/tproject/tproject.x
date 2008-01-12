include <fset.h>	# for F_REDIR
include <tbset.h>

# T_PROJECT -- Create a new table from selected columns of an old table
#
# B.Simon	20-Oct-1987	First Code
# Phil Hodge	07-Sep-1988	Change parameter names for tables.
# B.Simon	31-Mar-1992	Set output table type from input table
# Phil Hodge	 4-Oct-1995	Use table name template routines tbnopenp, etc.
# Phil Hodge	 8-Apr-1999	Call tbfpri.
# B.Simon	30-Apr-1999	Replace call to unique with nextuniq
# Phil Hodge	 9-Jun-1999	Set input/output to STDIN/STDOUT if redirected.

procedure t_project()

pointer ilist			# Input table name template
pointer olist			# Output table name template
pointer	columns			# Table column template
bool	uniq			# Should output rows be unique?
#--
int	junk, numcol, numptr, type
int	phu_copied		# set by tbfpri and ignored
pointer sp, itp, otp, intable, outtable, colptr

string	nomatch		"Number of input tables must match output tables"
string	notfound	"Column(s) not found in table"

bool	clgetb()
int	fstati()
int	tbnget(), tbnlen(), tbpsta()
pointer	tbtopn(), tbnopenp(), tbnopen()

begin
	# Allocate stack memory for strings

	call smark (sp)
	call salloc (intable, SZ_FNAME, TY_CHAR)
	call salloc (outtable, SZ_FNAME, TY_CHAR)
	call salloc (columns, SZ_LINE, TY_CHAR)

	# Read the task parameters

	if (fstati (STDIN, F_REDIR) == YES)
	    ilist = tbnopen ("STDIN")
	else
	    ilist = tbnopenp ("intable")

	if (fstati (STDOUT, F_REDIR) == YES)
	    olist = tbnopen ("STDOUT")
	else
	    olist = tbnopenp ("outtable")

	call clgstr ("columns", Memc[columns], SZ_LINE)
	uniq = clgetb ("uniq")

	# Loop over all table names in the input file name template

	if (tbnlen (ilist) != tbnlen (olist))
	    call error (1, nomatch)

	while (tbnget (ilist, Memc[intable], SZ_FNAME) != EOF) {

	    junk = tbnget (olist, Memc[outtable], SZ_FNAME)

	    # Open the tables and set output table type

	    itp = tbtopn (Memc[intable], READ_ONLY, NULL)
	    call tbfpri (Memc[intable], Memc[outtable], phu_copied)
	    otp = tbtopn (Memc[outtable], NEW_FILE, NULL)

	    type = tbpsta (itp, TBL_WHTYPE)
	    call tbpset (otp, TBL_WHTYPE, type)

	    # Create an array of column pointers from the column template

	    numcol = tbpsta (itp, TBL_NCOLS)
	    call malloc (colptr, numcol, TY_INT)

	    call tctexp (itp, Memc[columns], numcol, numptr, Memi[colptr])

	    if (numptr == 0)
		call error (1, notfound)

	    # Copy header and selected columns to output table

	    call wproject (itp, otp, numptr, Memi[colptr], uniq)

	    # Close the tables and free dynamic memory

	    call tbtclo (itp)
	    call tbtclo (otp)
	    call mfree (colptr, TY_INT)
	}

	# Close the filename template lists

	call tbnclose (ilist)
	call tbnclose (olist)
	call sfree (sp)
end
