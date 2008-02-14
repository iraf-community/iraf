include <ctype.h>
include "tvmark.h"

# MK_PPARS -- Store the IMMARK parameters.

procedure mk_ppars (mk)

pointer	mk		# pointer to the immark structure

pointer	sp, str
bool	itob()
int	mk_stati()
real	mk_statr()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Store the mark type.
	call mk_stats (mk, MARK, Memc[str], SZ_LINE)
	call clpstr ("mark", Memc[str])

	# Store the circle and rectangles descriptors.
	call mk_stats (mk, CSTRING, Memc[str], SZ_LINE)
	call clpstr ("radii", Memc[str])
	call mk_stats (mk, RSTRING, Memc[str], SZ_LINE)
	call clpstr ("lengths", Memc[str])

	call clputb ("number", itob (mk_stati (mk, NUMBER)))
	call clputb ("label", itob (mk_stati (mk, LABEL)))
	call clputi ("txsize", mk_stati (mk, SIZE))
	call clputi ("pointsize", 2 * mk_stati (mk, SZPOINT) + 1)
	call clputi ("color", mk_stati (mk, GRAYLEVEL))
	call clputi ("nxoffset", mk_stati (mk, NXOFFSET))
	call clputi ("nyoffset", mk_stati (mk, NYOFFSET))
	call clputr ("tolerance", mk_statr (mk, TOLERANCE))

	call sfree (sp)
end
