include	"../lib/mctable.h"


# MCT_SHRINK - Free unused table memory

procedure mct_shrink (table)

pointer	table			# table descriptor

int	lsize, psize

begin
	# Check pointer and magic number.
	if (table == NULL)
	    call error (0, "mct_shrink: Null table pointer")
	if (MCT_MAGIC (table) != MAGIC)
	    call error (0, "mct_shrink: Bad magic number")

	# Compute aproximate logical size, and exact physical
	# sizes. This might produce a little bit of space wasted.

	lsize = MCT_NPROWS (table) * MCT_MAXCOL (table)
	psize = MCT_MAXROW (table) * MCT_MAXCOL (table)

	# Reallocate table sapace and update physical size.
	if (lsize != psize) {
	    call realloc (MCT_DATA (table), lsize, MCT_TYPE (table))
	    MCT_MAXROW  (table) = MCT_NPROWS (table)
	    MCT_INCROWS (table) = GROWFACTOR (MCT_MAXROW (table))
	}
end
