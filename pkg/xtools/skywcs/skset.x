include "skywcsdef.h"
include "skywcs.h"


# SK_SETD -- Set a double precision coordinate parameter.

procedure sk_setd (coo, param, value)

pointer	coo			#I pointer to the coordinate structure
int	param			#I the input parameter
double	value			#I the parameter value

begin
	switch (param) {
	case S_VXOFF:
	    SKY_VXOFF(coo) = value
	case S_VYOFF:
	    SKY_VYOFF(coo) = value
	case S_VXSTEP:
	    SKY_VXSTEP(coo) = value
	case S_VYSTEP:
	    SKY_VYSTEP(coo) = value
	case S_EQUINOX:
	    SKY_EQUINOX(coo) = value
	case S_EPOCH:
	    SKY_EPOCH(coo) = value
	default:
	    call error (0, "SKY_SETD: Unknown coordinate system parameter")
	}
end


# SK_SETI -- Set an integer coordinate parameter.

procedure sk_seti (coo, param, value)

pointer	coo			#I pointer to the coordinate structure
int	param			#I the input parameter
int	value			#I the parameter value

begin
	switch (param) {
	case S_CTYPE:
	    SKY_CTYPE(coo) = value
	case S_RADECSYS:
	    SKY_RADECSYS(coo) = value
	case S_WTYPE:
	    SKY_WTYPE(coo) = value
	case S_PLNGAX:
	    SKY_PLNGAX(coo) = value
	case S_PLATAX:
	    SKY_PLATAX(coo) = value
	case S_XLAX:
	    SKY_XLAX(coo) = value
	case S_YLAX:
	    SKY_YLAX(coo) = value
	case S_PIXTYPE:
	    SKY_PIXTYPE(coo) = value
	case S_NLNGAX:
	    SKY_NLNGAX(coo) = value
	case S_NLATAX:
	    SKY_NLATAX(coo) = value
	case S_NLNGUNITS:
	    SKY_NLNGUNITS(coo) = value
	case S_NLATUNITS:
	    SKY_NLATUNITS(coo) = value
	case S_STATUS:
	    SKY_STATUS(coo) = value
	default:
	    call error (0, "SKY_SETI: Unknown coordinate system parameter")
	}
end


# SK_SETS -- Set a character string coordinate parameter.

procedure sk_sets (coo, param, value)

pointer	coo			#I pointer to the coordinate structure
int	param			#I the input parameter
char	value[ARB]		#I the parameter value

begin
	switch (param) {
	case S_COOSYSTEM:
	    call strcpy (value, SKY_COOSYSTEM(coo), SZ_FNAME)
	default:
	    call error (0, "SKY_SETSTR: Unknown coordinate system parameter")
	}
end
