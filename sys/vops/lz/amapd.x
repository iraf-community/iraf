# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AMAP -- Vector linear transformation.  Map the range of pixel values
# a1,a2 from a into the range b1,b2 in b.  It is assumed that a1 < a2
# and b1 < b2.

procedure amapd (a, b, npix, a1, a2, b1, b2)

double	a[ARB], b[ARB]
double	a1, a2, b1, b2

double	minout, maxout, aoff, boff, pixval

double	scalar

int	npix, i

begin
	    scalar = (double (b2) - double (b1)) / (double (a2) - double (a1))

	minout = min (b1, b2)
	maxout = max (b1, b2)
	aoff = a1
	boff = b1

	do i = 1, npix {
	    pixval = (a[i] - aoff) * scalar
	    b[i] = max(minout, min(maxout, pixval + boff))
	}
end
