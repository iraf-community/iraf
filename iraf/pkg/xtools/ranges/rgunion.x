# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<pkg/rg.h>

# RG_UNION -- Find the union of two sets of ranges.

pointer procedure rg_union (rg1, rg2)

pointer	rg1				# First set of ranges
pointer	rg2				# Second set of ranges

pointer	rg3				# Pointer to union

int	i, j

begin
	# Error check the range pointers.

	if ((rg1 == NULL) || (rg2 == NULL))
	    call error (0, "Range descriptor(s) undefined")

	# Allocate the range points array.

	i = RG_NRGS(rg1) + RG_NRGS(rg2)
	call malloc (rg3, LEN_RG + 2 * max (1, i), TY_STRUCT)

	# Set the ranges.

	RG_NRGS(rg3) = i
	RG_NPTS(rg3) = RG_NPTS(rg1) + RG_NPTS(rg2)

	j = 1
	do i = 1, RG_NRGS(rg1) {
	    RG_X1(rg3, j) = RG_X1(rg1, i)
	    RG_X2(rg3, j) = RG_X2(rg1, i)
	    j = j + 1
	}
	do i = 1, RG_NRGS(rg2) {
	    RG_X1(rg3, j) = RG_X1(rg2, i)
	    RG_X2(rg3, j) = RG_X2(rg2, i)
	    j = j + 1
	}

	call rgorder (rg3)
	call rgmerge (rg3)

	return (rg3)
end
