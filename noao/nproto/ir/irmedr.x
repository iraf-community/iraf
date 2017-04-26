# IRMEDR -- Procedure to compute the median of an array in which some
# elements are undefined.

real procedure irmedr (a, aindex, npts)

real	a[ARB]		# input array
int	aindex[ARB]	# definition array
int	npts		# number of points

int	i, n
pointer	sp, b
real	med
real	asokr()

begin
	call smark (sp)
	call salloc (b, npts, TY_REAL)

	n = 0
	do i = 1, npts {
	    if (aindex[i] > 0) {
		Memr[b+n] = a[i]
		n = n + 1
	    }
	}

	if (n == 0)
	    med = INDEFR
	else
	    med = asokr (Memr[b], n, (n + 1) / 2)

	call sfree (sp)

	return (med)
end
