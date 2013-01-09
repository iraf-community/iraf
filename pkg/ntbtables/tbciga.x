include <tbset.h>
include "tbtables.h"

# This define is here only temporarily.
define	TBL_MAXDIM	7	# maximum dimension of array in table

# This file contains tbciga and tbcisa.  For a column that contains arrays,
# the dimension of the array and the length of each axis may be gotten or
# specified using these routines.
#
# For table types other than FITS, these routines just get or set the
# total array length.
#
# Phil Hodge, 18-Nov-1994  Subroutines created.
# Phil Hodge,  5-Jul-1995  Modify for FITS tables; change calling sequence.
# Phil Hodge,  5-Aug-1999  Use COL_NELEM instead of tbalen to get array length.

# tbciga -- get dimension of array and length of each axis

procedure tbciga (tp, cp, ndim, axlen, maxdim)

pointer tp		# i: pointer to table descriptor
pointer cp		# i: pointer to column descriptor
int	ndim		# o: dimension of array
int	axlen[maxdim]	# o: length of each axis
int	maxdim		# i: size of axlen array
#--
errchk	tbfiga

begin
	if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    call tbfiga (tp, cp, ndim, axlen, maxdim)
	} else {
	    ndim = 1
	    axlen[1] = COL_NELEM(cp)
	}
end

# tbcisa -- set dimension of array and length of each axis

procedure tbcisa (tp, cp, ndim, axlen)

pointer tp		# i: pointer to table descriptor
pointer cp		# i: pointer to column descriptor
int	ndim		# i: dimension of array
int	axlen[ARB]	# i: length of each axis
#--
pointer sp
pointer errmess		# scratch for possible error message
pointer colname		# scratch for column name
int	nelem		# actual total number of elements in array
int	nvals		# total number specified as input
int	i
errchk	tbfisa

begin
	# Compare actual array size of column with the total number of
	# elements specified as input.
	nelem = COL_NELEM(cp)
	nvals = 1
	do i = 1, ndim
	    nvals = nvals * axlen[i]

	if (nelem != nvals) {
	    call smark (sp)
	    call salloc (errmess, SZ_LINE, TY_CHAR)
	    call salloc (colname, SZ_COLNAME, TY_CHAR)
	    call tbcigt (cp, TBL_COL_NAME, Memc[colname], SZ_COLNAME)
	    call sprintf (Memc[errmess], SZ_LINE,
		"tbcisa:  column `%s', actual array size=%d, specified size=%d")
		call pargstr (Memc[colname])
		call pargi (nelem)
		call pargi (nvals)
	    call error (1, Memc[errmess])
	}

	# Check whether dimension is too large.
	if (ndim > TBL_MAXDIM) {
	    call smark (sp)
	    call salloc (errmess, SZ_LINE, TY_CHAR)
	    call salloc (colname, SZ_COLNAME, TY_CHAR)
	    call tbcigt (cp, TBL_COL_NAME, Memc[colname], SZ_COLNAME)
	    call sprintf (Memc[errmess], SZ_LINE,
		"tbcisa:  column `%s', dimension %d is too large")
		call pargstr (Memc[colname])
		call pargi (ndim)
	    call error (1, Memc[errmess])
	}

	# Assign values in column descriptor.
	if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    call tbfisa (tp, cp, ndim, axlen)
	}
	# else nothing
end
