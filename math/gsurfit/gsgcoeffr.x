# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/gsurfit.h>
include "gsurfitdef.h"

# GSGCOEFF -- Procedure to fetch a particular coefficient.
# If the requested coefficient is undefined then INDEF is returned.

real procedure gsgcoeff (sf, xorder, yorder)

pointer	sf		# pointer to the surface fitting descriptor
int	xorder		# X order of desired coefficent
int	yorder		# Y order of desired coefficent

int	i, n, maxorder, xincr

begin
	if ((xorder > GS_XORDER(sf)) || (yorder > GS_YORDER(sf)))
	    return (INDEFR)

	switch (GS_XTERMS(sf)) {
	case GS_XNONE:
	    if (yorder == 1)
		n = xorder
	    else if (xorder == 1)
		n = GS_NXCOEFF(sf) + yorder - 1
	    else
		return (INDEFR)
	case GS_XHALF:
	    maxorder = max (GS_XORDER(sf) + 1, GS_YORDER(sf) + 1)
	    if ((xorder + yorder) > maxorder)
		return (INDEFR)
	    n = xorder
	    xincr = GS_XORDER(sf)
	    do i = 2, yorder {
		n = n + xincr
		if ((i + GS_XORDER(sf) + 1) > maxorder)
		    xincr = xincr - 1
	    }
	case GS_XFULL:
	    n = xorder + (yorder - 1) * GS_NXCOEFF(sf)
	}

	return (COEFF(GS_COEFF(sf) + n - 1))
end
