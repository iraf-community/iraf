include	"../lib/mctable.h"


# MCT_NROWS - Return the highest row number entered into the table

int procedure mct_nrows (table)

pointer	table			# table descriptor

begin
	# Check pointer and magic number.
	if (table == NULL)
	    call error (0, "mct_nrows: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_nrows: Bad magic number")

	# Return row counter.
	return (MCT_NPROWS (table))
end
