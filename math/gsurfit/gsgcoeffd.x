# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "dgsurfitdef.h"

# GSGCOEFF -- Procedure to fetch a particular coefficient.
# If the requested coefficient is undefined then INDEF is returned.

double procedure dgsgcoeff (sf, xorder, yorder)

pointer	sf		# pointer to the surface fitting descriptor
int	xorder		# X order of desired coefficent
int	yorder		# Y order of desired coefficent

int	n

begin
	if ((xorder > GS_XORDER(sf)) || (yorder > GS_YORDER(sf)))
	    return (INDEFD)

	switch (GS_XTERMS(sf)) {
	case NO:
	    if (yorder == 1)
		n = xorder
	    else if (xorder == 1)
		n = GS_NXCOEFF(sf) + yorder - 1
	    else
		return (INDEFD)
	case YES:
	    n = xorder + (yorder - 1) * GS_NXCOEFF(sf)
	}

	return (COEFF(GS_COEFF(sf) + n - 1))
end
