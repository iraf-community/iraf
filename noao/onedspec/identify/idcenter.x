include	<smw.h>
include	"identify.h"

# ID_CENTER -- Locate the center of a feature.

double procedure id_center (id, x, width, type)

pointer	id				# ID pointer
double	x				# Initial guess
real	width				# Feature width
int	type				# Feature type

int	np1
real	value
double	dvalue

real	center1d()
double	smw_c1trand()

begin
	if (IS_INDEFD(x))
	    return (x)

	dvalue = smw_c1trand (ID_PL(id), x)
	if (IS_INDEFD(dvalue))
	    return (dvalue)

	np1 = NP1(ID_SH(id)) - 1
	value = dvalue - np1
 	value = center1d (value, IMDATA(id,1), ID_NPTS(id),
	    width, type, ID_CRADIUS(id), ID_THRESHOLD(id))

	if (IS_INDEF(value))
	    return (INDEFD)
	else
	    return (smw_c1trand (ID_LP(id), double(value+np1)))
end
