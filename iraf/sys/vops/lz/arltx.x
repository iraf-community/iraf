# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ARLT -- Replace if less than.  If A[i] is less than FLOOR replace by NEWVAL.

procedure arltx (a, npix, floor, newval)

complex	a[ARB]
int	npix
complex	floor, newval
int	i
real	abs_floor

begin
	abs_floor = abs (floor)

	do i = 1, npix
	    if (abs (a[i]) < abs_floor)
		a[i] = newval
end
