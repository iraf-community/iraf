include <fset.h>		# to check for I/O redirection
include <tbset.h>

define	SYNTAX		   1

# TQUERY -- Create a new table from selected rows and columns of an old table
#
# B.Simon	18-Dec-1987 	First Code
# Phil Hodge	07-Sep-1988 	Change parameter names for tables.
# B.Simon	31-Mar-1992	Set output table type from input table
# Phil Hodge	 4-Oct-1995	Use table name template routines tbnopenp, etc.
# Phil Hodge	 8-Apr-1999	Call tbfpri.
# Phil Hodge	 9-Jun-1999	Set input/output to STDIN/STDOUT if redirected.

procedure t_tquery()

pointer ilist			# Input table name template
pointer olist			# Output table name template
pointer	expr			# Expression used to select rows
pointer	columns			# Table column template
pointer	sort			# Sort columns template
bool	uniq			# Should output rows be unique?
bool	ascend			# Ascending sort flag
bool	casesens		# Case sensitivity flag
#--
int	junk, nindex, numcol, type
int	phu_copied		# set by tbfpri and ignored
pointer sp, itp, otp, intable, outtable, index, colptr

string	nomatch	"Number of input tables must match output tables"

bool	clgetb()
int	fstati()
int	tbnget(), tbnlen(), tbpsta()
pointer	tbtopn(), tbnopenp(), tbnopen()

begin
	# Allocate stack memory for strings

	call smark (sp)
	call salloc (intable, SZ_FNAME, TY_CHAR)
	call salloc (outtable, SZ_FNAME, TY_CHAR)
	call salloc (expr, SZ_LINE, TY_CHAR)
	call salloc (columns, SZ_LINE, TY_CHAR)
	call salloc (sort, SZ_LINE, TY_CHAR)

	# Read the task parameters

	if (fstati (STDIN, F_REDIR) == YES)
	    ilist = tbnopen ("STDIN")
	else
	    ilist = tbnopenp ("intable")

	if (fstati (STDOUT, F_REDIR) == YES)
	    olist = tbnopen ("STDOUT")
	else
	    olist = tbnopenp ("outtable")

	call clgstr ("expr", Memc[expr], SZ_LINE)
	call clgstr ("columns", Memc[columns], SZ_LINE)
	call clgstr ("sort", Memc[sort], SZ_LINE)
	uniq = clgetb ("uniq")
	ascend = clgetb ("ascend")
	casesens = clgetb ("casesens")

	# Loop over all table names in the input file name template

	if (tbnlen (ilist) != tbnlen (olist))
	    call error (SYNTAX, nomatch)

	while (tbnget (ilist, Memc[intable], SZ_FNAME) != EOF) {

	    junk = tbnget (olist, Memc[outtable], SZ_FNAME)

	    # Open the tables and set output table type

	    itp = tbtopn (Memc[intable], READ_ONLY, NULL)
	    call tbfpri (Memc[intable], Memc[outtable], phu_copied)
	    otp = tbtopn (Memc[outtable], NEW_FILE, NULL)

	    type = tbpsta (itp, TBL_WHTYPE)
	    call tbpset (otp, TBL_WHTYPE, type)

	    # Create index arrays

	    call allrows (itp, nindex, index)
	    call allcols (itp, numcol, colptr)

	    # Do the query, returning an array of column pointers
	    # and row indices

	    call doquery (itp, Memc[expr], Memc[columns], Memc[sort],
			  uniq, ascend, casesens,
			  numcol, Memi[colptr], nindex, Memi[index])

	    # Copy header and selected rows and columns to output table

	    call wquery (itp, otp, numcol, Memi[colptr], nindex, Memi[index])

	    # Close the tables and free dynamic memory

	    call tbtclo (itp)
	    call tbtclo (otp)
	    call mfree (colptr, TY_INT)
	    call mfree (index, TY_INT)
	}

	# Close the filename template lists

	call tbnclose (ilist)
	call tbnclose (olist)
	call sfree (sp)
end
