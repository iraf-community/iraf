# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ARGT -- Replace if greater than.  If A[i] is greater than CEIL replace by
# NEWVAL.

procedure argtx (a, npix, ceil, newval)

complex	a[ARB]
int	npix
complex	ceil, newval
int	i
real	abs_ceil

begin
	abs_ceil = abs (ceil)

	do i = 1, npix
	    if (abs (a[i]) > abs_ceil)
		a[i] = newval
end
