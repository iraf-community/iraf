# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AMOV -- Copy a vector (generic).  The operation is carried out in such
# a way that the result is the same whether or not the output vector
# overlaps the input vector.

procedure amovs (a, b, npix)

short	a[ARB], b[ARB]
int	npix, i, a_first, a_last, b_first

begin
	call zlocva (a, a_first)
	call zlocva (b, b_first)

	if (a_first == b_first)
	    return

	call zlocva (a[ARB], a_last)
	if (b_first > a_first && b_first <= a_last) {
	    do i = npix, 1, -1
		b[i] = a[i]
	} else {
	    do i = 1, npix
		b[i] = a[i]
	}
end
