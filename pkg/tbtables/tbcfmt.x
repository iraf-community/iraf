include <tbset.h>
include "tbtables.h"

# tbcfmt -- change column print format
# This procedure replaces the print format for a column.  The column
# descriptor is updated, and if the table is not read-only the modified
# column descriptor is also written back into the table.
#
# Phil Hodge, 19-Oct-1989  allow changing format for a read-only table
# Phil Hodge,  3-Apr-1995  Set TB_MODIFIED to true.
# Phil Hodge,  9-Apr-1995  Modify for FITS tables.
# Phil Hodge, 14-Apr-1998  Change calling sequence of tbcwcd;
#			just copy to COL_FMT, instead of using tbcftp.

procedure tbcfmt (tp, cp, colfmt)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to a column descriptor
char	colfmt[ARB]		# i: print format for column
#--
char	pformat[SZ_COLFMT]	# local copy of format

errchk	tbcwcd, tbffmt

begin
	# If the format for display is blank, assign a default.
	call tbbadf (colfmt, COL_DTYPE(cp), COL_LEN(cp),
			pformat, SZ_COLFMT)
	call strcpy (pformat, COL_FMT(cp), SZ_COLFMT)

	if (!TB_READONLY(tp)) {
	    # Save modified column descriptor in table file.
	    if (TB_TYPE(tp) == TBL_TYPE_FITS)
		call tbffmt (tp, cp, pformat)
	    else
		call tbcwcd (tp, cp)
	    TB_MODIFIED(tp) = true
	}
end
