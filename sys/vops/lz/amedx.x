# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AMED -- Vector median selection.  The selection is carried out in a temporary
# array, leaving the input vector unmodified.  Especially demanding applications
# may wish to call the asok routine directory to avoid the call to the memory
# allocator.

complex procedure amedx (a, npix)

complex	a[ARB]
int	npix

pointer	sp, aa
complex	median
complex	asokx()		# select the Kth smallest element from A
real	a1, a2, a3

begin
	switch (npix) {
	case 1, 2:
	    return (a[1])

	case 3:
		a1 = abs (a[1])
		a2 = abs (a[2])
		a3 = abs (a[3])
		if (a1 < a2) {
		    if (a2 < a3)
			return (a[2])
		    else if (a1 < a3)
			return (a[3])
		    else
			return (a[1])
		} else {
		    if (a2 > a3)
			return (a[2])
		    else if (a1 < a3)
			return (a[1])
		    else
			return (a[3])
		}

	default:
	    call smark (sp)
	    call salloc (aa, npix, TY_COMPLEX)
	    call amovx (a, Memx[aa], npix)
	    median = asokx (Memx[aa], npix, (npix + 1) / 2)
	    call sfree (sp)

	    return (median)
	}
end
