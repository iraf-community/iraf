# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <pkg/rg.h>

# RG_ORDER -- Make all ranges increasing and order by the starting point.

procedure rg_order (rg)

pointer	rg				# Ranges

int	i, j, temp

begin
	# Error check the range pointer.

	if (rg == NULL)
	    call error (0, "Range descriptor undefined")

	# Make all ranges increasing.

	do i = 1, RG_NRGS(rg) {
	    if (RG_X1(rg, i) > RG_X2(rg, i)) {
		temp = RG_X1(rg, i)
		RG_X1(rg, i) = RG_X2(rg, i)
		RG_X2(rg, i) = temp
	    }
	}

	# Sort the ranges in increasing order.

	do i = 1, RG_NRGS(rg) - 1 {
	    do j = i + 1, RG_NRGS(rg) {
		if (RG_X1(rg, i) > RG_X1(rg, j)) {
		    temp = RG_X1(rg, i)
		    RG_X1(rg, i) = RG_X1(rg, j)
		    RG_X1(rg, j) = temp
		    temp = RG_X2(rg, i)
		    RG_X2(rg, i) = RG_X2(rg, j)
		    RG_X2(rg, j) = temp
		}
	    }
	}
end
