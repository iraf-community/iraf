# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/gsurfit.h>
include "dgsurfitdef.h"

# GSGET -- Procedure to fetch a gsurfit parameter
double procedure dgsgetd (sf, parameter)

pointer	sf		# pointer to the surface fit
int	parameter	# parameter to be fetched

begin
	switch (parameter) {
	case GSXMAX:
	    return (GS_XMAX(sf))
	case GSXMIN:
	    return (GS_XMIN(sf))
	case GSYMAX:
	    return (GS_YMAX(sf))
	case GSYMIN:
	    return (GS_YMIN(sf))
	case GSXREF:
	    return (GS_XREF(sf))
	case GSYREF:
	    return (GS_YREF(sf))
	case GSZREF:
	    return (GS_ZREF(sf))
	}
end


# GSSET -- Procedure to set a gsurfit parameter
procedure dgsset (sf, parameter, val)

pointer	sf		# pointer to the surface fit
int	parameter	# parameter to be fetched
double	val		# value to set

begin
	switch (parameter) {
	case GSXREF:
	    GS_XREF(sf) = val
	case GSYREF:
	    GS_YREF(sf) = val
	case GSZREF:
	    GS_ZREF(sf) = val
	}
end


# GSGETI -- Procedure to fetch an integer parameter

int procedure dgsgeti (sf, parameter)

pointer sf		# pointer to the surface fit
int	parameter	# integer parameter

begin
	switch (parameter) {
	case GSTYPE:
	    return (GS_TYPE(sf))
	case GSXORDER:
	    switch (GS_TYPE(sf)) {
	    case GS_LEGENDRE, GS_CHEBYSHEV, GS_POLYNOMIAL:
		return (GS_XORDER(sf))
	    }
	case GSYORDER:
	    switch (GS_TYPE(sf)) {
	    case GS_LEGENDRE, GS_CHEBYSHEV, GS_POLYNOMIAL:
	        return (GS_YORDER(sf))
	    }
	case GSXTERMS:
	    return (GS_XTERMS(sf))
	case GSNXCOEFF:
	    return (GS_NXCOEFF(sf))
	case GSNYCOEFF:
	    return (GS_NYCOEFF(sf))
	case GSNCOEFF:
	    return (GS_NCOEFF(sf))
	case GSNSAVE:
	    return (GS_SAVECOEFF+GS_NCOEFF(sf))
	}
end
