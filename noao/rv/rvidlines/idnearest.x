include	"identify.h"

# ID_NEAREST -- Find the nearest feature to a given coordinate.

procedure id_nearest (id, fitnear)

pointer	id			# ID pointer
double	fitnear			# Coordinate to find nearest feature

int	i
double	delta, delta1

begin
	if (ID_NFEATURES(id) < 1) {
	    ID_CURRENT(id) = 0
	    return
	}

	ID_CURRENT(id) = 1
	delta = abs (FIT(id,1) - fitnear)

	do i = 2, ID_NFEATURES(id) {
	    delta1 = abs (FIT(id,i) - fitnear)
	    if (delta1 < delta) {
		ID_CURRENT(id) = i
		delta = delta1
	    }
	}
end
