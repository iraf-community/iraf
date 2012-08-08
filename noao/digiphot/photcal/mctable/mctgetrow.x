include	"../lib/mctable.h"


# MCT_GETROW - Get pointer to row data values.

pointer procedure mct_getrow (table, row)

pointer	table			# table descriptor
int	row			# row number

begin
	# Check the pointer and magic number.
	if (table == NULL)
	    call error (0, "mct_getrow: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_getrow: Bad magic number")

	# Return the pointer to a row buffer, or NULL.
	# if row is out of range
	if (row < 1 || row > MCT_NPROWS (table))
	    call error (0, "mct_getrow: Bad row number")

	# Return row pointer.
	return (MCT_DATA (table) + (row - 1) * MCT_MAXCOL (table))
end
