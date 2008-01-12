include	<mach.h>
include	"ecidentify.h"

# EC_NEAREST -- Find the nearest feature to a given coordinate.

procedure ec_nearest (ec, fitnear)

pointer	ec			# ID pointer
double	fitnear			# Coordinate to find nearest feature

int	i, ec_next()
double	delta, delta1

begin
	EC_CURRENT(ec) = 0

	i = 0
	delta = MAX_REAL
	while (ec_next (ec, i) != EOF) {
	    delta1 = abs (FIT(ec,i) - fitnear)
	    if (delta1 < delta) {
		EC_CURRENT(ec) = i
		delta = delta1
	    }
	}
end
