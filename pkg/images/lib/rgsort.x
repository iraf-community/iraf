
define	LOGPTR		20			# log2(maxpts) (1e6)

# RG_QSORTR -- Vector quicksort a real array. In this version the index array
# is sorted not the data array. The input and output index arrays may be the
# same.

procedure rg_qsortr (data, a, b, npix)

real	data[ARB]		#I the input data array
int	a[ARB]			#I the input index array
int	b[ARB]			#O the output index array
int	npix			#I the number of pixels

int	i, j, lv[LOGPTR], p, uv[LOGPTR], temp
real	pivot

begin
	# Initialize the indices for an inplace sort.
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


# RG_QSORTI -- Vector quicksort an integer array. In this version the index
# array is actually sorted not the data array. The input and output index
# arrays may be the same.

procedure rg_qsorti (data, a, b, npix)

int	data[ARB]		# data array
int	a[ARB]			# input index array
int	b[ARB]			# output index array
int	npix			# number of pixels

int	i, j, lv[LOGPTR], p, uv[LOGPTR], temp, pivot

begin
	# Initialize the indices for an inplace sort.
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


# RG_SQSORT -- Sort two real arrays of data in increasing order using a
# secondary key. The data is assumed to have been already sorted on
# the primary key. The input and output index arrays may be the same.

procedure rg_sqsort (sdata, pdata, a, b, npix)

real	sdata[npix]		#I the secondary key
real	pdata[npix]		#I the primary key
int	a[npix]			#I the sorted index from the primary key
int	b[npix]			#O the sorted output index
int	npix			#I number of pixels

int	i, ndup, first

begin
	# Copy the index array.
	call amovi (a, b, npix)

	# Initialize.
	ndup = 0
	for (i = 2; i <= npix; i = i + 1) {
	    if (pdata[b[i]] <= pdata[b[i-1]])
		ndup = ndup + 1 
	    else if (ndup > 0) {
		first = i - 1 - ndup
		call rg_qsortr (sdata, b[first], b[first], ndup + 1)
		ndup = 0
	    }
	}
end
