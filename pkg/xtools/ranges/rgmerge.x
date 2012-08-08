# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<pkg/rg.h>

# RG_MERGE -- Merge overlapping ranges in set of ordered ranges.

procedure rg_merge (rg)

pointer	rg				# Ranges

int	new, old

begin
	# Error check the range pointer.

	if (rg == NULL)
	    call error (0, "Range descriptor undefined")
	if (RG_NRGS(rg) == 0)
	    return

	# Eliminate overlapping ranges and count the number of new ranges.

	new = 1
	do old = 2, RG_NRGS(rg)
	    if (RG_X1(rg, old) > RG_X2(rg, new) + 1) {
		new = new + 1
		RG_X1(rg, new) = RG_X1(rg, old)
		RG_X2(rg, new) = RG_X2(rg, old)
	    } else
		RG_X2(rg, new) = max (RG_X2(rg, old), RG_X2(rg, new))

	call realloc (rg, LEN_RG + 2 * new, TY_STRUCT)

	RG_NPTS(rg) = 0
	RG_NRGS(rg) = new
	do new = 1, RG_NRGS(rg)
	    RG_NPTS(rg) = RG_NPTS(rg) + RG_X2(rg, new) - RG_X1(rg, new) + 1
end
