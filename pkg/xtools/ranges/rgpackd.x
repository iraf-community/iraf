# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<pkg/rg.h>

# RG_PACK -- Pack input data to include only points in the ranges.
#
# The output array must be large enough to contain the desired points.
# If the ranges are merged then the input and output arrays may be the same.

procedure rg_packd (rg, a, b)

pointer	rg					# Ranges
double	a[ARB]					# Input array
double	b[ARB]					# Output array

int	i, j, k, n

begin
	# Error check the range pointer.

	if (rg == NULL)
	    call error (0, "Range pointer undefined")

	j = 0
	do i = 1, RG_NRGS(rg) {
	    if (RG_X1(rg, i) > RG_X2(rg, i)) {
		do k = RG_X1(rg, i), RG_X2(rg, i), -1 {
		    j = j + 1
		    b[j] = a[k]
		}
	    } else {
		n = RG_X2(rg, i) - RG_X1(rg, i) + 1
	        call amovd (a[RG_X1(rg, i)], b[j + 1], n)
	        j = j + n
	    }
	}
end
