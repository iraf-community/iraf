include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# tbtflu -- call flush
# This routine writes the size-information record (in case the number of
# rows or columns has changed) and then flushes the fio buffer for the table.
#
# Phil Hodge,  2-Nov-1988  Subroutine created.
# Phil Hodge, 14-Jan-1992  Add option for text table type.
# Phil Hodge, 20-Jun-1995  Add option for FITS tables.

procedure tbtflu (tp)

pointer tp			# i: pointer to table descriptor
#--

begin
	if (TB_TYPE(tp) == TBL_TYPE_TEXT)
	    return			# don't do anything for text file

	if (TB_TYPE(tp) == TBL_TYPE_FITS)
	    return			# don't do anything for FITS file

	if (TB_FILE(tp) == NULL)
	    call error (ER_TBNOTOPEN, "tbtflu:  table is not open")

	if (TB_READONLY(tp))
	    call error (ER_TBREADONLY, "tbtflu:  table is readonly")

	call tbtwsi (tp)		# write size-info record

	call flush (TB_FILE(tp))
end
