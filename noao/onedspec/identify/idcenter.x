include	"identify.h"

# ID_CENTER -- Locate the center of a feature.

double procedure id_center (id, x, width, type)

pointer	id				# ID pointer
double	x				# Initial guess
real	width				# Feature width
int	type				# Feature type

real	value
pointer data

real	center1d()

begin
	call malloc (data, ID_NPTS(id), TY_REAL)
	call achtdr (IMDATA(id,1), Memr[data], ID_NPTS(id))
 	value = center1d (real (x), Memr[data], ID_NPTS(id), width, type,
	    ID_CRADIUS(id), ID_THRESHOLD(id))
	call mfree (data, TY_REAL)
	if (IS_INDEF(value))
	    return (INDEFD)
	else
	    return (double (value))
end
