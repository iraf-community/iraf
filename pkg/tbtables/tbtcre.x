include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# tbtcre -- Create a new table
# This routine is called after calling tbtopn for a new table.
# For binary tables, this routine appends the default extension.
#
# Phil Hodge, 14-Aug-1987  Error if table is already open.
# Phil Hodge, 13-Jan-1992  Add option for text table type.
# Phil Hodge,  5-Mar-1993  Deallocate comment buffer if not a text table.
# Phil Hodge, 15-Dec-1994  Table name is now SZ_LINE instead of SZ_FNAME.
# Phil Hodge, 23-Dec-1994  Add option for CDF or FITS file.
# Phil Hodge,  3-Apr-1995  Set TB_MODIFIED to true.
# Phil Hodge,  7-Jun-1999  Replace TB_F_TYPE by TB_TYPE.
# Phil Hodge, 15-Jun-1999  Reset subtype for stsdas format tables.

procedure tbtcre (tp)

pointer tp			# Pointer to table descriptor
#--
errchk	tbtext, tbvnew, tbwnew, tbfnew, tbxnew, tbynew, tbznew

begin
	if (TB_IS_OPEN(tp))
	    call error (1, "tbtcre:  table is already open")

	if (TB_TYPE(tp) == TBL_TYPE_TEXT) {		# text file
	    call tbznew (tp)

	} else if (TB_TYPE(tp) == TBL_TYPE_FITS) {	# FITS table
	    call tbfnew (tp)

	} else {

	    # For a binary table we need to check that there's an
	    # extension, and if not, append the default extension.
	    call tbtext (TB_NAME(tp), TB_NAME(tp), SZ_LINE)

	    # Will the table be stored in a CDF file?  If it's a CDF file,
	    # we'll also do the initialization for a row-ordered table.
	    if (TB_TYPE(tp) == TBL_TYPE_CDF)		# common data format
		;  # call tbvnew (tp)

	    else if (TB_TYPE(tp) == TBL_TYPE_MI)	# machine independent
		;  # call tbwnew (tp)
	    else if (TB_TYPE(tp) == TBL_TYPE_S_ROW)	# SDAS row-ordered
		call tbxnew (tp)
	    else if (TB_TYPE(tp) == TBL_TYPE_S_COL)	# SDAS column-ordered
		call tbynew (tp)
	    else
		call error (ER_TBCORRUPTED, "tbtcre:  table type is messed up")

	    # We don't need this if it's not a text table.
	    if (TB_COMMENT(tp) != NULL) {
		call mfree (TB_COMMENT(tp), TY_CHAR)
		TB_SZ_COMMENT(tp) = 0
	    }

	    # Subtype is not relevant.
	    TB_SUBTYPE(tp) = TBL_SUBTYPE_UNKNOWN
	}

	TB_IS_OPEN(tp) = true
	TB_MODIFIED(tp) = true
end
