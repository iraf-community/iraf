include <error.h>
include <tbset.h>

# tugcol -- get input X values
# Get input independent variable column and check it to make
# sure it is either monotonically increasing or decreasing.
#
# Phil Hodge, 18-Apr-1988  Subroutine created
# Phil Hodge, 30-Jan-1992  Check independent variables more carefully.
# Phil Hodge, 27-Apr-2000  Move most of this routine to tudcol;
#			rewrite to allow either array or scalar column.

procedure tugcol (itp, iv_icp, row, xin, xnelem, padvalue, array)

pointer itp			# i: pointer to input table descriptor
pointer iv_icp			# i: ptr to descr for input indep var column
int	row			# i: row number, if input column contains arrays
double	xin[ARB]		# o: input independent variable values
int	xnelem			# o: actual number of elements in xin array
double	padvalue		# i: ignore this value at end of xin array
bool	array			# i: true if input column contains arrays
#--
pointer sp
pointer temp			# scratch for checking indep var for duplicates
int	nelem			# array size
int	nvals			# number of elements actually gotten
int	nrows			# number of rows in input table
int	i			# loop index
int	op			# index in temp
int	tbcigi(), tbpsta(), tbagtd()
string	NOT_MONOTONIC	"input independent variable is not monotonic"

begin
	if (array) {

	    nelem = tbcigi (iv_icp, TBL_COL_LENDATA)
	    nvals = tbagtd (itp, iv_icp, row, xin, 1, nelem)
	    if (nvals != nelem) {
		call eprintf (
	"Not all input independent variable data were gotten from row %d\n")
		    call pargi (row)
		call error (1, "")
	    }
	    xnelem = nvals

	} else {

	    nrows = tbpsta (itp, TBL_NROWS)
	    do i = 1, nrows
		call tbegtd (itp, iv_icp, i, xin[i])
	    xnelem = nrows
	}

	# Trim trailing INDEF and pad values by reducing xnelem.
	call tu_trim (xin, xnelem, padvalue)

	call smark (sp)
	call salloc (temp, xnelem, TY_DOUBLE)

	# Copy the independent variable data to scratch, skipping embedded
	# INDEF values.
	op = 0
	do i = 1, xnelem {
	    if (!IS_INDEFD(xin[i])) {
		Memd[temp+op] = xin[i]	# op is zero indexed at this point
		op = op + 1
	    }
	}

	if (op > 1) {
	    # Check the independent variable values to make sure they're
	    # monotonically increasing or decreasing.
	    if (Memd[temp+1] > Memd[temp]) {		# increasing
		do i = 2, op {				# one indexed
		    if (Memd[temp+i-1] <= Memd[temp+i-2])
			call error (1, NOT_MONOTONIC)
		}
	    } else {					# decreasing
		do i = 2, op {
		    if (Memd[temp+i-1] >= Memd[temp+i-2])
			call error (1, NOT_MONOTONIC)
		}
	    }
	}

	call sfree (sp)
end
