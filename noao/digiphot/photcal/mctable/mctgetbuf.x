include	"../lib/mctable.h"


# MCT_GETBUF - Get pointer to data buffer.

pointer procedure mct_getbuf (table)

pointer	table			# table descriptor

pointer	mct_getrow()

errchk	mct_getrow()

begin
	# Check pointer and magic number.
	if (table == NULL)
	    call error (0, "mct_getbuf: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_getbuf: Bad magic number")

	# Return pointer to data buffer.
	return (mct_getrow (table, 1))
end
