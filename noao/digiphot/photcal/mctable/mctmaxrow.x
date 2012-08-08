include	"../lib/mctable.h"


# MCT_MAXROW - Return the maximum number of rows of the table.

int procedure mct_maxrow (table)

pointer	table			# table descriptor

begin
	# Check pointer and magic number.
	if (table == NULL)
	    call error (0, "mct_mxrow: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_mxrow: Bad magic number")

	# Return max number of rows.
	return (MCT_MAXROW (table))
end
