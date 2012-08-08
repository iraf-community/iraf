include <tbset.h>

define	SYNTAX		   1

# TDIFFER -- Create a new table which is the difference of two old tables
#
# B.Simon	14-Jun-1988	First Code
# Phil Hodge	 7-Sep-1988	Change parameter names for tables.
# Phil Hodge	 8-Apr-1999	Call tbfpri.

procedure t_tdiffer()

pointer	tab1			# First table
pointer	tab2			# Second table
pointer	outtab			# Output table
pointer	colnam1			# Columns to match in first table
pointer	colnam2			# Columns to match in second table
#--
int	nptr1, nptr2, ncol1, ncol2
int	phu_copied		# set by tbfpri and ignored
pointer sp, tp1, tp2, otp, col1, col2

string	notfound	"Column(s) not found in table"
string  colnumerr	"Number of columns do not agree"

int	tbpsta()
pointer	tbtopn()

begin
	# Allocate stack memory for strings

	call smark (sp)
	call salloc (tab1, SZ_FNAME, TY_CHAR)
	call salloc (tab2, SZ_FNAME, TY_CHAR)
	call salloc (outtab, SZ_FNAME, TY_CHAR)
	call salloc (colnam1, SZ_LINE, TY_CHAR)
	call salloc (colnam2, SZ_LINE, TY_CHAR)

	# Read the task parameters

	call clgstr ("intable1", Memc[tab1], SZ_LINE)
	call clgstr ("intable2", Memc[tab2], SZ_LINE)
	call clgstr ("outtable", Memc[outtab], SZ_LINE)
	call clgstr ("colnam1", Memc[colnam1], SZ_LINE)
	call clgstr ("colnam2", Memc[colnam2], SZ_LINE)

	# Open the tables and create column arrays

	tp1 = tbtopn (Memc[tab1], READ_ONLY, NULL)
	ncol1 = tbpsta (tp1, TBL_NCOLS)
	call malloc (col1, ncol1, TY_INT)

	tp2 = tbtopn (Memc[tab2], READ_ONLY, NULL)
	ncol2 = tbpsta (tp2, TBL_NCOLS)
	call malloc (col2, ncol2, TY_INT)

	# Open output table and copy header(s) from first table

	call tbfpri (Memc[tab1], Memc[outtab], phu_copied)
	otp = tbtopn (Memc[outtab], NEW_COPY, tp1)
	call tbtcre (otp)
	call tbhcal (tp1, otp)

	# Create two arrays of column pointers from the column templates

	call tctexp (tp1, Memc[colnam1], ncol1, nptr1, Memi[col1])
	if (nptr1 == 0)
	    call error (SYNTAX, notfound)

	call tctexp (tp2, Memc[colnam2], ncol2, nptr2, Memi[col2])
	if (nptr2 == 0)
	    call error (SYNTAX, notfound)

	if (nptr1 != nptr2)
	    call error (SYNTAX, colnumerr)

	# Retrieve the indices of the rows of the first table which are
	# not in the second table

	call tbl_diff (tp1, tp2, otp, nptr1, Memi[col1], Memi[col2])
 
	# Close the tables and free dynamic memory

	call tbtclo (tp1)
	call tbtclo (tp2)
	call tbtclo (otp)

	call mfree (col1, TY_INT)
	call mfree (col2, TY_INT)

	call sfree (sp)
end
