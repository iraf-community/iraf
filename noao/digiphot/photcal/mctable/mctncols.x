include	"../lib/mctable.h"


# MCT_NCOLS - Return the highest column number for the highest row, entered
# into the table

int procedure mct_ncols (table)

pointer	table			# table descriptor

begin
	# Check the pointer and magic number.
	if (table == NULL)
	    call error (0, "mct_ncols: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_ncols: Bad magic number")

	# Return column counter.
	return (MCT_NPCOLS (table))
end
