# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMAFLP -- Flip a vector end for end.  Optimized for the usual pixel types.
# Pretty slow for DOUBLE and COMPLEX on byte machines, but it is not worth
# optimizing for those cases.

procedure imaflp (a, npix, sz_pixel)

char	a[ARB], temp
int	npix, sz_pixel
int	i, left, right, pixel

begin
	switch (sz_pixel) {
	case SZ_SHORT:
	    call imflps (a, npix)
	case SZ_LONG:
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


# IMFLPS -- Flip an array of SHORT sized elements.

procedure imflps (a, npix)

short	a[npix], temp
int	npix, i, right

begin
	right = npix + 1

	do i = 1, (npix + 1) / 2 {
	    temp = a[right-i]
	    a[right-i] = a[i]
	    a[i] = temp
	}
end


# IMFLPL -- Flip an array of LONG sized elements.

procedure imflpl (a, npix)

long	a[npix], temp
int	npix, i, right

begin
	right = npix + 1

	do i = 1, (npix + 1) / 2 {
	    temp = a[right-i]
	    a[right-i] = a[i]
	    a[i] = temp
	}
end
