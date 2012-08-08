include	"../lib/mctable.h"


# MCT_TYPE - Return table type.

int procedure mct_type (table)

pointer	table			# table descriptor

begin
	# Check pointer and magic number.
	if (table == NULL)
	    call error (0, "mct_type: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_type: Bad magic number")

	# Return type.
	return (MCT_TYPE (table))
end
