include	"../shdr.h"
include	"identify.h"

# ID_CENTER -- Locate the center of a feature.

double procedure id_center (id, x, width, type)

pointer	id				# ID pointer
double	x				# Initial guess
real	width				# Feature width
int	type				# Feature type

real	value

real	center1d()
double	mw_c1trand()

begin
	value = mw_c1trand (ID_PL(id), x)
 	value = center1d (value, IMDATA(id,1), ID_NPTS(id),
	    width, type, ID_CRADIUS(id), ID_THRESHOLD(id))
	if (IS_INDEF(value))
	    return (INDEFD)
	else
	    return (mw_c1trand (ID_LP(id), double(value)))
end
