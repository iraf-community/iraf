# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<pkg/rg.h>

# RG_WINDOW -- Intersect a set of ordered and merged ranges with a window.

pointer procedure rg_window (rg, rmin, rmax)

pointer	rg				# Ranges
int	rmin, rmax			# Window

pointer	rgout				# Pointer to windowed ranges

int	i, j

begin
	if (rg == NULL)
	    call error (0, "Range descriptor undefined")

	# Allocate the range points array.

	call malloc (rgout, LEN_RG + 2 * max (1, RG_NRGS(rg)), TY_STRUCT)

	# Set the windowed ranges.

	j = 0
	do i = 1, RG_NRGS(rg) {
	    if ((rmin <= RG_X2(rg, i)) && (rmax >= RG_X1(rg, i))) {
		j = j + 1
		RG_X1(rgout, j) = max (rmin, RG_X1(rg, i))
		RG_X2(rgout, j) = min (rmax, RG_X2(rg, i))
	    }
	}

	call realloc (rgout, LEN_RG + 2 * max (1, j), TY_STRUCT)
	RG_NRGS(rgout) = j
	RG_NPTS(rgout) = 0
	do i = 1, RG_NRGS(rgout)
	    RG_NPTS(rgout) = RG_NPTS(rgout) +
		abs (RG_X1(rgout, i) - RG_X2(rgout, i)) + 1

	return (rgout)
end
