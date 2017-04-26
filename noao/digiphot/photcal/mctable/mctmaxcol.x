include	"../lib/mctable.h"


# MCT_MAXCOL - Return the maximum number of columns in the table.

int procedure mct_maxcol (table)

pointer	table			# table descriptor

begin
	# Check pointer and magic number
	if (table == NULL)
	    call error (0, "mct_maxcol: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_maxcol: Bad magic number")

	# Return max number of columns
	return (MCT_MAXCOL (table))
end
