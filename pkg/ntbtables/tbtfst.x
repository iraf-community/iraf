include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# tbtfst -- call fseti
# This procedure calls fseti to set F_ADVICE to either RANDOM or SEQUENTIAL
# for a table file.  The file is first closed and then reopened because
# the buffer size cannot be changed after the first I/O to a file.  (The
# table was read when first opened.)  The value of TB_FILE(tp) might be
# changed.  The file is reopened either readonly or read/write.
#
# Phil Hodge, 30-Sep-1987  Subroutine created.
# Phil Hodge, 15-Nov-1988  Modify error message.
# Phil Hodge,  5-Oct-1995  Check table type.

procedure tbtfst (tp, fset_option, fset_value)

pointer tp			# i: pointer to table descriptor
int	fset_option		# i: specifies what FIO parameter is to be set
int	fset_value		# i: the value that is to be assigned
#--
int	iomode			# I/O mode for reopening the file
int	open()

errchk	close, open

begin
	if (TB_FILE(tp) == NULL)
	    call error (ER_TBNOTOPEN,
			"tbtfst:  table must be open to set FIO option")

	if (TB_TYPE(tp) != TBL_TYPE_S_ROW && TB_TYPE(tp) != TBL_TYPE_S_COL)
	    return

	if (TB_READONLY(tp)) {
	    iomode = READ_ONLY
	} else {
	    call tbtwsi (tp)		# update size information record
	    iomode = READ_WRITE
	}
	call close (TB_FILE(tp))
	TB_FILE(tp) = open (TB_NAME(tp), iomode, BINARY_FILE)

	call fseti (TB_FILE(tp), fset_option, fset_value)
end
