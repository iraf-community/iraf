include	"ecidentify.h"

# EC_CENTER -- Locate the center of a feature.

double procedure ec_center (ec, x, width, type)

pointer	ec				# ID pointer
double	x				# Initial guess
real	width				# Feature width
int	type				# Feature type

real	value
pointer data

real	center1d()

begin
	call malloc (data, EC_NPTS(ec), TY_REAL)
	call achtdr (IMDATA(ec,1), Memr[data], EC_NPTS(ec))
 	value = center1d (real (x), Memr[data], EC_NPTS(ec), width, type,
	    EC_CRADIUS(ec), EC_THRESHOLD(ec))
	call mfree (data, TY_REAL)
	if (IS_INDEF(value))
	    return (INDEFD)
	else
	    return (double (value))
end
