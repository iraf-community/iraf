# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AGLT -- Given a list of ranges, replace the value of each input pixel
# which falls within a given range by applying the corresponding linear
# transformation (b = a * kmul + kadd).  If KMUL is identically zero,
# B is replaced by the constant KADD.

procedure agltx (a, b, npix, low, high, kmul, kadd, nrange)

complex	a[ARB], b[ARB], pixval
int	npix, i
complex	low[nrange], high[nrange]	# range limits
real	kmul[nrange], kadd[nrange]
real	abs_pixval
int	nrange, nr

begin
	do i = 1, npix {
	    pixval = a[i]
	    b[i] = pixval
		abs_pixval = abs (pixval)
	    do nr = 1, nrange
		if (abs_pixval >= abs (low[nr]) &&
		abs_pixval <= abs (high[nr])) {
			if (kmul[nr] == 0.0)
			    b[i] = kadd[nr]
			else
			    b[i] = (pixval * kmul[nr]) + kadd[nr]
			break
		}
	}
end
