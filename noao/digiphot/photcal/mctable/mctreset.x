include	"../lib/mctable.h"


# MCT_RESET - Reset table counters, and set all table values to INDEF.

procedure mct_reset (table)

pointer	table			# table descriptor

errchk	mct_indef()

begin
	# Check pointer and magic number.
	if (table == NULL)
	    call error (0, "mct_reset: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_reset: Bad magic number")

	# Clear table counters.
	MCT_NPCOLS (table) = 0
	MCT_NPROWS (table) = 0
	MCT_NGCOLS (table) = 0
	MCT_NGROWS (table) = 0

	# Clear table buffer.
	call mct_indef (table, MCT_DATA (table), 
	    MCT_NPROWS (table) * MCT_NPCOLS (table))
end
