# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imset.h>
include	<imio.h>

# IMSETR -- Set an IMIO parameter to a real value.  For completeness this
# routine can be used to set integer valued parameters, although if the
# value has a fractional part or requires more than 24 bits of precision
# the results may be unpredictable.

procedure imsetr (im, param, value)

pointer	im			#I image descriptor
int	param			#I parameter to be set
real	value			#I value of parameter

begin
	switch (param) {
	case IM_BNDRYPIXVAL:
	    IM_OOBPIX(im) = value
	default:
	    call imseti (im, param, nint(value))
	}
end
