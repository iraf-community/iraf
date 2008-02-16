# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<pkg/rg.h>

# RG_INTERSECT -- Intersect two sets of ordered and merged ranges.

pointer procedure rg_intersect (rg1, rg2)

pointer	rg1				# First set of ranges
pointer	rg2				# Second set of ranges

pointer	rg3				# Pointer to intersection

int	i, j, k

begin
	# Error check the range pointers.

	if ((rg1 == NULL) || (rg2 == NULL))
	    call error (0, "Range descriptor(s) undefined")

	# Allocate the range points array.

	k = RG_NRGS(rg1) + RG_NRGS(rg2) - 1
	call malloc (rg3, LEN_RG + 2 * max (1, k), TY_STRUCT)

	# Set the ranges.

	i = 1
	j = 1
	k = 0

	while (i <= RG_NRGS(rg1) && j <= RG_NRGS(rg2)) {
	    if (RG_X2(rg1, i) < RG_X1(rg2, j))
		i = i + 1
	    else if (RG_X2(rg2, j) < RG_X1(rg1, i))
		j = j + 1
	    else {
		k = k + 1
		RG_X1(rg3, k) = max (RG_X1(rg1, i), RG_X1(rg2, j))
		RG_X2(rg3, k) = min (RG_X2(rg1, i), RG_X2(rg2, j))

	        if (RG_X2(rg1, i) < RG_X2(rg2, j))
		    i = i + 1
	        else
		    j = j + 1
	    }
	}

	call realloc (rg3, LEN_RG + 2 * max (1, k), TY_STRUCT)

	RG_NRGS(rg3) = k
	RG_NPTS(rg3) = 0
	do i = 1, RG_NRGS(rg3)
	    RG_NPTS(rg3) = RG_NPTS(rg3) + RG_X2(rg3, i) - RG_X1(rg3, i) + 1

	return (rg3)
end
