# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMAFLP -- Flip a vector end for end.  Optimized for the usual pixel types.
# Pretty slow for DOUBLE and COMPLEX on byte machines, but it is not worth
# optimizing for those cases.

procedure imaflp (a, npix, sz_pixel)

char	a[ARB]
size_t	npix
int	sz_pixel

char	temp
long	i, left, right, pixel

begin
	switch (sz_pixel) {
	case SZ_SHORT:
	    call imflps (a, npix)
	case SZ_INT:
	    # arg1: incompatible pointer
	    call imflpi (a, npix)
	case SZ_LONG:
	    # arg1: incompatible pointer
	    call imflpl (a, npix)

	default:				# flip odd sized elements
	    left = 1
	    right = ((npix-1) * sz_pixel) + 1

	    do pixel = 1, (npix + 1) / 2 {
		do i = 0, sz_pixel-1 {
		    temp = a[right+i]
		    a[right+i] = a[left+i]
		    a[left+i] = temp
		}
		left = left + sz_pixel
		right = right - sz_pixel
	    }
	}
end
