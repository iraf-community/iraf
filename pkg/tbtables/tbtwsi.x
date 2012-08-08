include <tbset.h>
include "tbtables.h"

# tbtwsi -- write size info
# This procedure writes the size information record into a table.
# NOTE:  If the table is row-ordered TB_ALLROWS(tp) will be set to
# TB_NROWS(tp).
# If the table was opened read-write, the version number may be
# increased, since the current version number is written, rather than
# the version originally read from the table.
#
# Phil Hodge, 15-Oct-1987  Seek BOF instead of 1.
# Phil Hodge, 14-Jan-1992  Add option for text table type.
# Phil Hodge,  5-Apr-1993  Add version number.
# Phil Hodge,  8-Jun-1995  Modify for FITS file.

procedure tbtwsi (tp)

pointer tp			# i: pointer to table descriptor
#--
int	sizinfo[LEN_SIZINFO]	# Size information record
errchk	seek, write, tbfwsi

begin
	if (TB_TYPE(tp) == TBL_TYPE_TEXT)
	    return

	# For a table in a FITS file we need to update the number of rows.
	if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    call tbfwsi (tp)
	    return
	}

	call amovki (0, sizinfo, LEN_SIZINFO)	# initialize buffer to zero

	if (TB_TYPE(tp) == TBL_TYPE_S_ROW)
	    TB_ALLROWS(tp) = TB_NROWS(tp)	# appropriate if row-ordered

	S_TYPE(sizinfo)    = TB_TYPE(tp)
	S_NPAR(sizinfo)    = TB_NPAR(tp)
	S_MAXPAR(sizinfo)  = TB_MAXPAR(tp)
	S_NROWS(sizinfo)   = TB_NROWS(tp)
	S_ALLROWS(sizinfo) = TB_ALLROWS(tp)
	S_NCOLS(sizinfo)   = TB_NCOLS(tp)
	S_MAXCOLS(sizinfo) = TB_MAXCOLS(tp)
	S_COLUSED(sizinfo) = TB_COLUSED(tp)
	S_ROWLEN(sizinfo)  = TB_ROWLEN(tp)
	S_VERSION(sizinfo) = TBL_CURRENT_VERSION

	# Write first record of table.
	call seek (TB_FILE(tp), BOF)
        if (SZ_INT != SZ_INT32)
            call ipak32 (sizinfo, sizinfo, SZ_SIZINFO)
	call write (TB_FILE(tp), sizinfo, SZ_SIZINFO)
end
