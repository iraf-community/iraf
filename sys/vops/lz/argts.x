# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ARGT -- Replace if greater than.  If A[i] is greater than CEIL replace by
# NEWVAL.

procedure argts (a, npix, ceil, newval)

short	a[ARB]
int	npix
short	ceil, newval
int	i

begin

	do i = 1, npix
	    if (a[i] > ceil)
		a[i] = newval
end
