# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMSAMP -- Subsample a vector.

procedure imsamp (a, b, npix, sz_pixel, step)

char	a[ARB], b[ARB]
int	npix, sz_pixel, step, i, j, in, out, delta_in

begin
	switch (sz_pixel) {
	case SZ_SHORT:
	    call imsmps (a, b, npix, step)
	case SZ_LONG:
	    call imsmpl (a, b, npix, step)

	default:				# flip odd sized elements
	    in = 0
	    out	= 0
	    delta_in = sz_pixel * step

	    do j = 1, npix {
		do i = 1, sz_pixel
		    b[out+i] = a[in+i]
		in = in + delta_in
		out = out + sz_pixel
	    }
	}
end
