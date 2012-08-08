# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ARLT -- Replace if less than.  If A[i] is less than FLOOR replace by NEWVAL.

procedure arlti (a, npix, floor, newval)

int	a[ARB]
int	npix
int	floor, newval
int	i

begin

	do i = 1, npix
	    if (a[i] < floor)
		a[i] = newval
end
