# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ASELK -- Vector/constant select element.  The output vector is formed by
# taking successive pixels from either of the input vector or a constant, based
# on the value of the integer (boolean) selection vectors.  Used to implement
# vector conditional expressions.

procedure aselkc (a, b, c, sel, npix)

char	a[ARB], b, c[ARB]
int	sel[ARB]			# IF sel[i] THEN a[i] ELSE b
int	npix
int	i

begin
	do i = 1, npix
	    if (sel[i] != 0)
		c[i] = a[i]
	    else
		c[i] = b
end
