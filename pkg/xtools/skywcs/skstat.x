include "skywcsdef.h"
include "skywcs.h"


# SK_STATD -- Get a double precision coordinate parameter.

double procedure sk_statd (coo, param)

pointer	coo			#I pointer to the coordinate structure
int	param			#I the input parameter

begin
	switch (param) {
	case S_VXOFF:
	    return (SKY_VXOFF(coo))
	case S_VYOFF:
	    return (SKY_VYOFF(coo))
	case S_VXSTEP:
	    return (SKY_VXSTEP(coo))
	case S_VYSTEP:
	    return (SKY_VYSTEP(coo))
	case S_EQUINOX:
	    return (SKY_EQUINOX(coo))
	case S_EPOCH:
	    return (SKY_EPOCH(coo))
	default:
	    call error (0, "SKY_STATD: Unknown coordinate system parameter")
	}
end


# SK_STATI -- Get an integer coordinate parameter.

int procedure sk_stati (coo, param)

pointer	coo			#I pointer to the coordinate structure
int	param			#I the input parameter

begin
	switch (param) {
	case S_CTYPE:
	    return (SKY_CTYPE(coo))
	case S_RADECSYS:
	    return (SKY_RADECSYS(coo))
	case S_WTYPE:
	    return (SKY_WTYPE(coo))
	case S_PLNGAX:
	    return (SKY_PLNGAX(coo))
	case S_PLATAX:
	    return (SKY_PLATAX(coo))
	case S_XLAX:
	    return (SKY_XLAX(coo))
	case S_YLAX:
	    return (SKY_YLAX(coo))
	case S_PIXTYPE:
	    return (SKY_PIXTYPE(coo))
	case S_NLNGAX:
	    return (SKY_NLNGAX(coo))
	case S_NLATAX:
	    return (SKY_NLATAX(coo))
	case S_NLNGUNITS:
	    return (SKY_NLNGUNITS(coo))
	case S_NLATUNITS:
	    return (SKY_NLATUNITS(coo))
	case S_STATUS:
	    return (SKY_STATUS(coo))
	default:
	    call error (0, "SKY_STATI: Unknown coordinate system parameter")
	}
end



# SK_STATS -- Get a character string coordinate parameter.

procedure sk_stats (coo, param, value, maxch)

pointer	coo			#I pointer to the coordinate structure
int	param			#I the input parameter
char	value			#O the output string
int	maxch			#I the maximum size of the string

begin
	switch (param) {
	case S_COOSYSTEM:
	    call strcpy (SKY_COOSYSTEM(coo), value, maxch)
	default:
	    call error (0, "SKY_GETSTR: Unknown coordinate system parameter")
	}
end
