include <tbset.h>
include "tbtables.h"

# This routine copies the tscal & tzero values to COL_TSCAL & COL_TZERO.
#
# This routine will return without doing anything if:
#     the table type isn't FITS
#     the "true" data type is a floating type, or boolean, or text
#     the input tscal & tzero are 1 & 0 respectively or are INDEFD.
#
# Unless the data type already is a floating type, it will be changed to
# real or double.  If the actual data type in the table file is int, the
# apparent data type will be set to double; otherwise (i.e. short or byte),
# the apparent data type will be set to real.
#
# If the table is open, the TSCALi and TZEROi keywords will be added to
# the header (or updated, if they're already present).
#
# This is the interface routine; tbfscal is lower level.
#
# Phil Hodge, 23-Jun-2000  Subroutine created.

procedure tbcscal (tp, cp, tscal, tzero)

pointer tp		# i: pointer to table struct
pointer cp		# i: pointer to column struct
double	tscal		# i: scale factor for column
double	tzero		# i: zero offset for column
#--
int	tdtype		# "true" data type (we expect short or int)
int	dtype		# "apparent" data type
bool	modify		# true if either tscal or tzero differs from default
errchk	tbfscal

begin
	if (TB_TYPE(tp) != TBL_TYPE_FITS)
	    return			# scaling parameters can't be used

	dtype = COL_DTYPE(cp)
	tdtype = COL_TDTYPE(cp)

	# Scaling is only appropriate if the true data type in the table
	# file is an integer type.
	if (tdtype == TBL_TY_REAL || tdtype == TBL_TY_DOUBLE ||
	    tdtype == TBL_TY_BOOL || tdtype < 0) {
	    return
	}

	modify = false			# initial value

	if (tscal != 1.d0 && !IS_INDEFD(tscal)) {
	    COL_TSCAL(cp) = tscal
	    modify = true
	}

	if (tzero != 1.d0 && !IS_INDEFD(tzero)) {
	    COL_TZERO(cp) = tzero
	    modify = true
	}

	if (!modify)
	    return			# nothing to do

	# Change the data type to a floating type, if it isn't already.
	if (dtype != TBL_TY_REAL && dtype != TBL_TY_DOUBLE) {
	    if (tdtype == TBL_TY_INT)
		COL_DTYPE(cp) = TBL_TY_DOUBLE
	    else
		COL_DTYPE(cp) = TBL_TY_REAL
	}

	# Write these values to keywords TSCALi and TZEROi.
	if (TB_IS_OPEN(tp))
	    call tbfscal (tp, cp)
end
