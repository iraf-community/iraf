include <mach.h>		# for SZB_CHAR
include <tbset.h>
include "tbtables.h"
include "tblerr.h"

define	WIDEST_CHAR_COLUMN	(SZ_LINE / SZB_CHAR * SZB_CHAR)

# tbbaln -- assign data length
# This routine assigns a value for dlen, the number of char required
# for storing one element of type datatype in a table.  This is trivial
# for numerical data types.  Character strings are packed, however, and
# the allocated space is rounded up to a multiple of SZB_CHAR.
# The input datatype is given as -N for a string of length N, and the
# output dtype will be the same with dlen = N/SZB_CHAR (with N rounded up
# as required).  This routine also checks that N is no larger than the
# space provided by the maximum number of SZ_CHARs that fit in SZ_LINE
# (e.g. 160 if SZ_LINE is 161 and SZB_CHAR is 2).
# The output dtype is TBL_TY_REAL if datatype is TY_REAL, etc.  These
# are currently the same.
#
# Phil Hodge, 10-Aug-1987  Replace input lendata with output dtype.
# Phil Hodge, 10-Nov-1987  Include check on width of char column.
# Phil Hodge,  7-Mar-1989  Set dtype = TBL_TY_... (except =datatype for char)
# Phil Hodge, 30-Mar-1993  Include short datatype; allow increments of SZB_CHAR
#			instead of just SZ_REAL*SZB_CHAR for character columns.

procedure tbbaln (datatype, dtype, dlen)

int	datatype	# i: data type of column
int	dtype		# o: table data type (= SPP except -n for char)
int	dlen		# o: number of char for storage in table
#--
begin
	if (datatype >= 0) {

	    # Data type is a numeric or Boolean SPP data type.
	    switch (datatype) {
	    case TY_REAL:
		dtype = TBL_TY_REAL
		dlen = SZ_REAL
	    case TY_DOUBLE:
		dtype = TBL_TY_DOUBLE
		dlen = SZ_DOUBLE
	    case TY_INT:
		dtype = TBL_TY_INT
		dlen = SZ_INT32
	    case TY_BOOL:
		dtype = TBL_TY_BOOL
		dlen = SZ_BOOL
	    case TY_SHORT:
		dtype = TBL_TY_SHORT
		dlen = SZ_SHORT
	    default:
		call error (ER_TBBADTYPE,
			"invalid data type for a table column")
	    }
	} else {

	    # datatype = -N implies a string of length N char.
	    # Check to make sure it's not too long.
	    if (-datatype > WIDEST_CHAR_COLUMN)
		call error (ER_TBBADTYPE,
			"char string column can't be that wide")

	    # Round up to a multiple of SZB_CHAR; the unit for dlen
	    # is SZ_CHAR, even though the string will be packed.
	    # NOTE:  no additional space is reserved for the EOS.
	    dlen = (-datatype + SZB_CHAR-1) / SZB_CHAR * SZ_CHAR
	    dtype = datatype
	}
end
