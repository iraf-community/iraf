# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<pkg/rg.h>

# RG_INRANGE -- Is value in the ranges?

int procedure rg_inrange (rg, rval)

pointer	rg				# Ranges
int	rval				# Range value to test

int	i

begin
	# Error check the range pointer.

	if (rg == NULL)
	    call error (0, "Range descriptor undefined")

	do i = 1, RG_NRGS(rg) {
	    if ((RG_X1(rg, i) <= RG_X2(rg, i)) && (rval >= RG_X1(rg, i)) &&
		    (rval <= RG_X2(rg, i)))
		return (YES)
	    else if ((rval >= RG_X2(rg, i)) && (rval <= RG_X1(rg, i)))
		return (YES)
	}

	return (NO)
end
