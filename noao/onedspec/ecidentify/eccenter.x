include	"ecidentify.h"

# EC_CENTER -- Locate the center of a feature.

double procedure ec_center (ec, x, width, type)

pointer	ec				# EC pointer
double	x				# Initial guess
real	width				# Feature width
int	type				# Feature type

real	value

real	center1d()
double	smw_c1trand()

begin
	value = smw_c1trand (EC_PL(ec), x)
 	value = center1d (value, IMDATA(ec,1), EC_NPTS(ec), width,
	    abs (type), EC_CRADIUS(ec), EC_THRESHOLD(ec))
	if (IS_INDEF(value))
	    return (INDEFD)
	else
	    return (smw_c1trand (EC_LP(ec), double(value)))
end
