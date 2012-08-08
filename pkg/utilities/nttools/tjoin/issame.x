include "tjoin.h"

# B.Simon	16-Apr-99	first code

# IS_SAME -- See if two values in different tables are the same

bool procedure is_same (tj1, tj2, irow, jrow, tol, casesens)

pointer	tj1	    # i: Table info descriptor for first input table
pointer	tj2	    # i: Table info descriptor for second input table
int	irow	    # i: Row number of element in first table
int	jrow	    # i: Row number of element in second table
pointer	tol	    # i: Descriptor of vecor of tolerance values
bool    casesens    # i: Join is case sensitive
#--
bool	same
double	dval1, dval2
int	icol, dtype1, dtype2, ival1, ival2
pointer	sp, str1, str2

string	badtol   "Tolerance must be zero for joins on non-numeric columns"
string	badtype  "Type mismatch on join columns"

bool	streq()
int	spp_type()

begin
	# Allocate memory for table strings

	call smark (sp)
	call salloc (str1, SZ_LINE, TY_CHAR)
	call salloc (str2, SZ_LINE, TY_CHAR)

	same = true
	do icol = 1, TJ_JNUM(tj1) {
	    if (! same)
		break

	    # Get column data types

	    dtype1 = spp_type (TJ_JCOL(tj1,icol))
	    dtype2 = spp_type (TJ_JCOL(tj2,icol))

	    # Comparison depends on data type

	    if (dtype1 == TY_CHAR && dtype2 == TY_CHAR) {
		# Nonzero tolerance illegal on string columns

		if (TOL_VAL(tol,icol) != 0.0)
		    call error (1, badtol)

		call tbegtt (TJ_TAB(tj1), TJ_JCOL(tj1,icol), irow, 
			     Memc[str1], SZ_LINE)
		call tbegtt (TJ_TAB(tj2), TJ_JCOL(tj2,icol), jrow, 
			     Memc[str2], SZ_LINE)

		# Convert to lower case for case insensitive match

		if (! casesens) {
		    call strlwr (Memc[str1])
		    call strlwr (Memc[str2])
		}

		# Test for undefined values first, which never match

		if (Memc[str1] == EOS || Memc[str2] == EOS) {
		    same = false
		} else {
		    same = streq (Memc[str1], Memc[str2])
		}

	    } else if (dtype1 == TY_BOOL && dtype2 == TY_BOOL) {
		# Nonzero tolerance illegal on boolean column

		if (TOL_VAL(tol,icol) != 0.0)
		    call error (1, badtol)

		# Read boolean as integer so we can detect undefined values

		call tbegti (TJ_TAB(tj1), TJ_JCOL(tj1,icol), irow, ival1)
		call tbegti (TJ_TAB(tj2), TJ_JCOL(tj2,icol), jrow, ival2)

		# Undefined values never match anything

		if (IS_INDEFI(ival1) || IS_INDEFI(ival2)) {
		    same = false
		} else {
		    same = ival1 == ival2
		}

	    } else if (dtype1 == TY_CHAR || dtype1 == TY_BOOL ||
		       dtype2 == TY_BOOL || dtype2 == TY_BOOL) {

		# Catch comparison of numeric and non-numeric values

		call error (1, badtype)

	    } else {
		# Null column pointer indicates the join is done on row number

		if (TJ_JCOL(tj1,icol) == NULL) {
		    dval1 = irow
		} else {
		    call tbegtd (TJ_TAB(tj1), TJ_JCOL(tj1,icol), irow, dval1)
		}

		if (TJ_JCOL(tj2,icol) == NULL) {
		    dval2 = jrow
		} else {
		    call tbegtd (TJ_TAB(tj2), TJ_JCOL(tj2,icol), jrow, dval2)
		}

		# Undefined values never match
		# Numeric values must be checked to see if the
		# difference is smaller than the tolerance

		if (IS_INDEFD(dval1) || IS_INDEFD(dval2)) {
		    same = false
		} else {
		    same = abs (dval2 - dval1) <= TOL_VAL(tol,icol)
		}
	    }
	}

	call sfree (sp)
	return (same)
end
