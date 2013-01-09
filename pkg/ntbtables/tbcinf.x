include <tbset.h>
include "tbtables.h"

# tbcinf -- get info about a column
# This procedure finds information about a column.  For numeric and Boolean
# data types the value of datatype will be the SPP data type, but for a
# character string of length N the datatype will be -N.
# For the time being the value of lendata is just one.
#
# Phil Hodge, 10-Aug-87  Set lendata=1 and datatype = -n for char string.
# Phil Hodge,  7-Oct-87  Call tbcig[it] for each item.
# Phil Hodge,  8-Jun-92  Change order of declarations, and get colnum first.

procedure tbcinf (colptr,
		colnum, colname, colunits, colfmt, datatype, lendata, lenfmt)

pointer colptr			# i: Pointer to a column descriptor
int	colnum			# o: Column number
char	colname[ARB]		# o: Column name
char	colunits[ARB]		# o: Units for column
char	colfmt[ARB]		# o: Print format for display of column
int	datatype		# o: Data type of column (SPP type or -n)
int	lendata			# o: Number of elements (=1)
int	lenfmt			# o: Bytes for print format
#--
int	tbcigi()

begin
	colnum   = tbcigi (colptr, TBL_COL_NUMBER)
	call tbcigt (colptr, TBL_COL_NAME,  colname,  SZ_COLNAME)
	call tbcigt (colptr, TBL_COL_UNITS, colunits, SZ_COLUNITS)
	call tbcigt (colptr, TBL_COL_FMT,   colfmt,   SZ_COLFMT)
	datatype = tbcigi (colptr, TBL_COL_DATATYPE)
	lendata  = tbcigi (colptr, TBL_COL_LENDATA)
	lenfmt   = tbcigi (colptr, TBL_COL_FMTLEN)
end
