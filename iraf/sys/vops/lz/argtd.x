# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ARGT -- Replace if greater than.  If A[i] is greater than CEIL replace by
# NEWVAL.

procedure argtd (a, npix, ceil, newval)

double	a[ARB]
size_t	npix
double	ceil, newval
size_t	i

begin

	do i = 1, npix
	    if (a[i] > ceil)
		a[i] = newval
end
