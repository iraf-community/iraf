# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ABLEK -- Vector boolean less than or equals constant.  C[i], type INT,
# is set to 1 if A[i] is less than or equal to B, else C[i] is set to zero.

procedure ablekx (a, b, c, npix)

complex	a[ARB]
complex	b
int	c[ARB]
int	npix
int	i
real	abs_b

begin
	# The case b==0 is perhaps worth optimizing.  On many machines this
	# will save a memory fetch.

	if (b == (0.0,0.0)) {
	    do i = 1, npix
		if (abs (a[i]) == 0)
		    c[i] = 1
		else
		    c[i] = 0
	} else {
	    abs_b = abs (b)
	    do i = 1, npix
		if (abs (a[i]) <= abs_b)
		    c[i] = 1
		else
		    c[i] = 0
	}
end
