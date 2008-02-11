# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ARGT -- Replace if greater than.  If A[i] is greater than CEIL replace by
# NEWVAL.

procedure argts (a, npix, ceil, newval)

short	a[ARB]
size_t	npix
short	ceil, newval
size_t	i

begin

	do i = 1, npix
	    if (a[i] > ceil)
		a[i] = newval
end
