# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ARGT -- Replace if greater than.  If A[i] is greater than CEIL replace by
# NEWVAL.

procedure argtl (a, npix, ceil, newval)

long	a[ARB]
int	npix
long	ceil, newval
int	i

begin

	do i = 1, npix
	    if (a[i] > ceil)
		a[i] = newval
end
