define	LOGPTR		20			# log2(maxpts) (1e6)

# PT_QSORT -- Vector Quicksort. In this version the index array is
# sorted.

procedure pt_qsortr (data, a, b, npix)

real	data[ARB]		# data array
int	a[ARB], b[ARB]		# index array
int	npix			# number of pixels

int	i, j, lv[LOGPTR], p, uv[LOGPTR], temp
real	pivot

begin
	# Initialize the indices for an inplace sort.
	do i = 1, npix
	    a[i] = i
	call amovi (a, b, npix)

	p = 1
	lv[1] = 1
	uv[1] = npix
	while (p > 0) {

	    # If only one elem in subset pop stack otherwise pivot line.
	    if (lv[p] >= uv[p])
		p = p - 1
	    else {
		i = lv[p] - 1
		j = uv[p]
		pivot = data[b[j]]

		while (i < j) {
		    for (i=i+1;  data[b[i]] < pivot;  i=i+1)
			;
		    for (j=j-1;  j > i;  j=j-1)
			if (data[b[j]] <= pivot)
			    break
		    if (i < j) {		# out of order pair
			temp = b[j]		# interchange elements
			b[j] = b[i]
			b[i] = temp
		    }
		}

		j = uv[p]			# move pivot to position i
		temp = b[j]			# interchange elements
		b[j] = b[i]
		b[i] = temp

		if (i-lv[p] < uv[p] - i) {	# stack so shorter done first
		    lv[p+1] = lv[p]
		    uv[p+1] = i - 1
		    lv[p] = i + 1
		} else {
		    lv[p+1] = i + 1
		    uv[p+1] = uv[p]
		    uv[p] = i - 1
		}

		p = p + 1			# push onto stack
	    }
	}
end


# PT_QSORT -- Vector Quicksort. In this version the index array is
# sorted.

procedure pt_qsorti (data, a, b, npix)

int	data[ARB]		# data array
int	a[ARB], b[ARB]		# index array
int	npix			# number of pixels

int	i, j, lv[LOGPTR], p, uv[LOGPTR], temp
int	pivot

begin
	# Initialize the indices for an inplace sort.
	do i = 1, npix
	    a[i] = i
	call amovi (a, b, npix)

	p = 1
	lv[1] = 1
	uv[1] = npix
	while (p > 0) {

	    # If only one elem in subset pop stack otherwise pivot line.
	    if (lv[p] >= uv[p])
		p = p - 1
	    else {
		i = lv[p] - 1
		j = uv[p]
		pivot = data[b[j]]

		while (i < j) {
		    for (i=i+1;  data[b[i]] < pivot;  i=i+1)
			;
		    for (j=j-1;  j > i;  j=j-1)
			if (data[b[j]] <= pivot)
			    break
		    if (i < j) {		# out of order pair
			temp = b[j]		# interchange elements
			b[j] = b[i]
			b[i] = temp
		    }
		}

		j = uv[p]			# move pivot to position i
		temp = b[j]			# interchange elements
		b[j] = b[i]
		b[i] = temp

		if (i-lv[p] < uv[p] - i) {	# stack so shorter done first
		    lv[p+1] = lv[p]
		    uv[p+1] = i - 1
		    lv[p] = i + 1
		} else {
		    lv[p+1] = i + 1
		    uv[p+1] = uv[p]
		    uv[p] = i - 1
		}

		p = p + 1			# push onto stack
	    }
	}
end


# PT_QSORT -- Vector Quicksort. In this version the index array is
# sorted.

procedure pt_qsortb (data, a, b, npix)

bool	data[ARB]		# data array
int	a[ARB], b[ARB]		# index array
int	npix			# number of pixels

int	i, j, lv[LOGPTR], p, uv[LOGPTR], temp
bool	pivot
int	pt_compareb()

begin
	# Initialize the indices for an inplace sort.
	do i = 1, npix
	    a[i] = i
	call amovi (a, b, npix)

	p = 1
	lv[1] = 1
	uv[1] = npix
	while (p > 0) {

	    # If only one elem in subset pop stack otherwise pivot line.
	    if (lv[p] >= uv[p])
		p = p - 1
	    else {
		i = lv[p] - 1
		j = uv[p]
		pivot = data[b[j]]

		while (i < j) {
		    #for (i=i+1;  data[b[i]] != pivot;  i=i+1)
		    for (i=i+1;  pt_compareb (data[b[i]], pivot) < 0;  i=i+1)
			;
		    for (j=j-1;  j > i;  j=j-1)
			#if (data[b[j]] != pivot)
			if (pt_compareb (data[b[j]], pivot) <= 0)
			    break
		    if (i < j) {		# out of order pair
			temp = b[j]		# interchange elements
			b[j] = b[i]
			b[i] = temp
		    }
		}

		j = uv[p]			# move pivot to position i
		temp = b[j]			# interchange elements
		b[j] = b[i]
		b[i] = temp

		if (i-lv[p] < uv[p] - i) {	# stack so shorter done first
		    lv[p+1] = lv[p]
		    uv[p+1] = i - 1
		    lv[p] = i + 1
		} else {
		    lv[p+1] = i + 1
		    uv[p+1] = uv[p]
		    uv[p] = i - 1
		}

		p = p + 1			# push onto stack
	    }
	}
end


# PT_COMPAREB -- Compare to booleans for the sort routine.

int procedure pt_compareb (a, b)

bool	a		# first boolean
bool	b		# second boolean

begin
	if (! a && b)
	    return (-1)
	else if (a && ! b)
	    return (1)
	else
	    return (0)
end
