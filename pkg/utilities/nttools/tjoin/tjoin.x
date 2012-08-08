include <tbset.h>
include "tjoin.h"

define	SYNTAX		1
define	BIG_TABLE	5000

# TJOIN -- Join two tables on the basis of equality in a common column
#
# B.Simon	03-Nov-1987	First Code
# Phil Hodge	08-Apr-1999	Call tbfpri.
# B.Simon	16-Apr-1999	Support outer join and multiple join columns
# Phil Hodge	21-Jun-2001	Realloc TOL_PTR before copying tolerance value

procedure t_tjoin()

pointer	intable1		# Names of the first table to be joined
pointer	intable2		# Names of the second table to be joined
pointer	outtable		# Name of output table
pointer	column1			# Name of columns to join in first table
pointer	column2			# Name of columns to join in second table
pointer extrarows		# Include unmatched rows from which table?
pointer	tolerance		# Tolerance used in testing for equality
bool	casesens		# Case sensitivity flag
#--
int	phu_copied		# set by tbfpri and ignored
int	extra, ival
pointer sp, errtxt, tj1, tj2, tjo, tol

string  extraopt  "|neither|first|both|"
string	badextra  "Illegal value for extrarows"
string	badjnum   "Number of join columns do not match"
string	badtolnum "Number of tolereances and join columns do not match"
string	badcolnam "Column name not found in table (%s)"

bool	clgetb()
int	strdic()
pointer	read_tol(), open_itab(), open_otab()

begin
	# Allocate stack memory for strings

	call smark (sp)
	call salloc (intable1, SZ_FNAME, TY_CHAR)
	call salloc (column1, SZ_COLNAME, TY_CHAR)
	call salloc (intable2, SZ_FNAME, TY_CHAR)
	call salloc (column2, SZ_COLNAME, TY_CHAR)
	call salloc (outtable, SZ_FNAME, TY_CHAR)
	call salloc (extrarows, SZ_FNAME, TY_CHAR)
	call salloc (tolerance, SZ_FNAME, TY_CHAR)
	call salloc (errtxt, SZ_LINE, TY_CHAR)

	# Read the task parameters

	call clgstr ("intable1", Memc[intable1], SZ_FNAME)
	call clgstr ("intable2", Memc[intable2], SZ_FNAME)
	call clgstr ("outtable", Memc[outtable], SZ_FNAME)

	call clgstr ("column1", Memc[column1], SZ_COLNAME)
	call clgstr ("column2", Memc[column2], SZ_COLNAME)

	call clgstr ("extrarows", Memc[extrarows], SZ_FNAME)
	call clgstr ("tolerance", Memc[tolerance], SZ_FNAME)
	casesens = clgetb ("casesens")

	# Check value of extrarows

	extra = strdic (Memc[extrarows], Memc[extrarows], SZ_FNAME, extraopt)

	if (extra == 0) {
	    call sprintf (Memc[errtxt], SZ_LINE, badextra)
	    call pargstr (Memc[extrarows])
	    call error (SYNTAX, Memc[errtxt])
	}

	extra = extra - 1

	# Parse the string of tolerance values

	tol = read_tol (Memc[tolerance])

	# Open the input tables and get the column pointers

	tj1 = open_itab (Memc[intable1], Memc[column1])
	tj2 = open_itab (Memc[intable2], Memc[column2])

	# Check the number of join columns and tolerances for agreement

	if (TJ_JNUM(tj1) != TJ_JNUM(tj2))
	    call error (1, badjnum)

	if (TJ_JNUM(tj1) != TOL_NUM(tol)) {
	    if (TOL_NUM(tol) == 1) {
		TOL_NUM(tol) = TJ_JNUM(tj1)
		call realloc (TOL_PTR(tol), TOL_NUM(tol), TY_DOUBLE)
		do ival = 2, TJ_JNUM(tj1) 
		    TOL_VAL(tol,ival) = TOL_VAL(tol,1)
	    } else {
		call error (1, badtolnum)
	    }
	}

	# Remove data columns from second table which are also
	# join columns in the first table

	call remove_jcol (tj2, tol)

	# Create the output table

	call tbfpri (Memc[intable1], Memc[outtable], phu_copied)
	tjo = open_otab (Memc[outtable], tj1, tj2)

	# Compute the join of the two tables

	call dojoin (tj1, tj2, tjo, tol, extra, casesens)

	# Close the tables and free dynamic memory

	call free_tol (tol)

	call close_iotab (tj1)
	call close_iotab (tj2)
	call close_iotab (tjo)
	call sfree (sp)
end
