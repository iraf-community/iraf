# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ABGEK -- Vector boolean greater than or equals constant.  C[i], type INT,
# is set to 1 if A[i] is greater than or equal to B, else C[i] is set to zero.

procedure abgekx (a, b, c, npix)

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
	    call amovki (1, c, npix)
	} else {
	    abs_b = abs (b)
	    do i = 1, npix
		if (abs (a[i]) >= abs_b)
		    c[i] = 1
		else
		    c[i] = 0
	}
end
