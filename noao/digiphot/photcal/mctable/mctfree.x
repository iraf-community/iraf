include	"../lib/mctable.h"


# MCT_FREE - Free table structure and data buffer associated with it.

procedure mct_free (table)

pointer	table			# table descriptor

begin
	# Check pointer and magic number.
	if (table == NULL)
	    call error (0, "mct_free: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_free: Bad magic number")
	
	# Free the table.
	call mfree (MCT_DATA (table), MCT_TYPE (table))
	call mfree (table, TY_STRUCT)
end
