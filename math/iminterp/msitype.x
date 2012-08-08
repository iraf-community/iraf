include "im2interpdef.h"
include <math/iminterp.h>

# MSITYPE -- Decode the interpolation string input by the user.

procedure msitype (interpstr, interp_type, nsinc, nincr, shift)

char	interpstr[ARB]			# the input interpolation string
int	interp_type			# the interpolation type
int	nsinc				# the sinc interpolation width
int	nincr				# the sinc interpolation lut resolution
real	shift				# the predefined shift / pixfrac

int	ip
pointer	sp, str
int	strdic(), strncmp(), ctoi(), ctor()

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)
	interp_type = strdic (interpstr, Memc[str], SZ_FNAME, II_BFUNCTIONS)

	# Use the default interpolant parameters.
	if (interp_type > 0) {
	    switch (interp_type) {
	    case II_BILSINC:
		nsinc = 2 * NSINC + 1
		nincr = NINCR
		shift = INDEFR
	    case II_BISINC:
		nsinc = 2 * NSINC + 1
		nincr = 0
		shift = INDEFR
	    case II_BIDRIZZLE:
		nsinc = 0
		nincr = 0
		shift = PIXFRAC
	    default:
		nsinc = 0
		nincr = 0
		shift = INDEFR
	    }

	# Try to decode the look-up table sinc parameters.
	} else if (strncmp (interpstr, "lsinc", 5) == 0) {
	    ip = 6
	    interp_type = II_BILSINC
	    if (ctoi (interpstr, ip, nsinc) <= 0) {
		nsinc = 2 * NSINC + 1
		nincr = NINCR
		shift = INDEFR
	    } else {
		if (interpstr[ip] == '[')
		    ip = ip + 1
		if (ctor (interpstr, ip, shift) <= 0)
		    shift = INDEFR
		if (IS_INDEFR(shift) || interpstr[ip] != ']') {
		    nincr = NINCR
		    shift = INDEFR
		} else if (shift >= -0.5 && shift < 0.5) {
		    nincr = 1
		} else {
		    nincr = nint (shift)
		    shift = INDEFR
		}
	    }

	# Try to decode the sinc parameters.
	} else if (strncmp (interpstr, "sinc", 4) == 0) {
	    ip = 5
	    interp_type = II_BISINC
	    if (ctoi (interpstr, ip, nsinc) <= 0)
		nsinc = 2 * NSINC + 1
	    nincr = 0
	    shift = INDEFR
	} else if (strncmp (interpstr, "drizzle", 7) == 0) {
	    ip = 8
	    if (interpstr[ip] == '[')
		ip = ip + 1
	    if (ctor (interpstr, ip, shift) <= 0)
	        shift = PIXFRAC
	    interp_type = II_DRIZZLE
	    nsinc = 0
	    nincr = 0
	    if (interpstr[ip] != ']')
		shift = PIXFRAC
	    else if (shift < 0.0  || shift > 1.0)
		shift = PIXFRAC
	} else {
	    interp_type = 0
	    nsinc = 0
	    nincr = 0
	    shift = INDEFR
	}

	call sfree (sp)
end
