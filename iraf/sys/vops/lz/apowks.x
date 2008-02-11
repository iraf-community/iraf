# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# APOWK -- Compute a ** b, where b is a constant of type INT (generic).

procedure apowks (a, b, c, npix)

short	a[ARB], c[ARB]
short	c_1
int	b
size_t	npix, i

begin
	# Optimize the code for the various special cases.  We assume that the
	# compiler is intelligent enough to recognize the special cases if the
	# power is expressed as an integer constant.

	c_1 = 1
	switch (b) {
	case 0:
	    call amovks (c_1, c, npix)
	case 1:
	    call amovs (a, c, npix)
	case 2:
	    do i = 1, npix
		c[i] = a[i] ** 2
	case 3:
	    do i = 1, npix
		c[i] = a[i] ** 3
	case 4:
	    do i = 1, npix
		c[i] = a[i] ** 4
	default:
	    do i = 1, npix
		c[i] = a[i] ** b
	}
end
