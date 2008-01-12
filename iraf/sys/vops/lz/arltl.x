# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ARLT -- Replace if less than.  If A[i] is less than FLOOR replace by NEWVAL.

procedure arltl (a, npix, floor, newval)

long	a[ARB]
int	npix
long	floor, newval
int	i

begin

	do i = 1, npix
	    if (a[i] < floor)
		a[i] = newval
end
