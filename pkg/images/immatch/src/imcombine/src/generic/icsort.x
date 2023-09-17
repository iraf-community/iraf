# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

define	LOGPTR	32			# log2(maxpts) (4e9)


# IC_SORT -- Quicksort.  This is based on the VOPS asrt except that
# the input is an array of pointers to image lines and the sort is done
# across the image lines at each point along the lines.  The number of
# valid pixels at each point is allowed to vary.  The cases of 1, 2, and 3
# pixels per point are treated specially.

procedure ic_sorts (a, b, nvecs, npts)

pointer	a[ARB]			# pointer to input vectors
short	b[ARB]			# work array
int	nvecs[npts]		# number of vectors
int	npts			# number of points in vectors

short	pivot, temp, temp3
int	i, j, k, l, p, npix, lv[LOGPTR], uv[LOGPTR]
define	swap {temp=$1;$1=$2;$2=temp}
define	copy_	10

begin
	do l = 0, npts-1 {
	    npix = nvecs[l+1]
	    if (npix <= 1)
		next

	    do i = 1, npix
		b[i] = Mems[a[i]+l]

	    # Special cases
	    if (npix <= 3) {
		pivot = b[1]
		temp = b[2]
		if (npix == 2) {
		    if (temp < pivot) {
			b[1] = temp
			b[2] = pivot
		    } else
			next
		} else {
		    temp3 = b[3]
		    if (temp < pivot) {				# bac|bca|cba
			if (temp < temp3) {			# bac|bca
			    b[1] = temp
			    if (pivot < temp3)			# bac
				b[2] = pivot
			    else {				# bca
				b[2] = temp3
				b[3] = pivot
			    }
			} else {				# cba
			    b[1] = temp3
			    b[3] = pivot
			}
		    } else if (temp3 < temp) {			# acb|cab
			b[3] = temp
			if (pivot < temp3)			# acb
			    b[2] = temp3
			else {					# cab
			    b[1] = temp3
			    b[2] = pivot
			}
		    } else
			next
		}
		goto copy_
	    }

	    # General case
	    do i = 1, npix
		b[i] = Mems[a[i]+l]

	    lv[1] = 1
	    uv[1] = npix
	    p = 1

	    while (p > 0) {
		if (lv[p] >= uv[p])		# only one elem in this subset
		    p = p - 1			# pop stack
		else {
		    # Dummy do loop to trigger the Fortran optimizer.
		    do p = p, ARB {
			i = lv[p] - 1
			j = uv[p]

			# Select as the pivot the element at the center of the
			# array, to avoid quadratic behavior on an already
			# sorted array.

			k = (lv[p] + uv[p]) / 2
			swap (b[j], b[k])
			pivot = b[j]		   # pivot line

			while (i < j) {
			    for (i=i+1;  b[i] < pivot;  i=i+1)
				;
			    for (j=j-1;  j > i;  j=j-1)
				if (b[j] <= pivot)
				    break
			    if (i < j)		   # out of order pair
				swap (b[i], b[j])  # interchange elements
			}

			j = uv[p]		   # move pivot to position i
			swap (b[i], b[j])	   # interchange elements

			if (i-lv[p] < uv[p] - i) { # stack so shorter done first
			    lv[p+1] = lv[p]
			    uv[p+1] = i - 1
			    lv[p] = i + 1
			} else {
			    lv[p+1] = i + 1
			    uv[p+1] = uv[p]
			    uv[p] = i - 1
			}

			break
		    }
		    p = p + 1			   # push onto stack
		}
	    }

copy_
	    do i = 1, npix
		Mems[a[i]+l] = b[i]
	}
end


# IC_2SORT -- Quicksort.  This is based on the VOPS asrt except that
# the input is an array of pointers to image lines and the sort is done
# across the image lines at each point along the lines.  The number of
# valid pixels at each point is allowed to vary.  The cases of 1, 2, and 3
# pixels per point are treated specially.  A second integer set of
# vectors is sorted.

procedure ic_2sorts (a, b, c, d, nvecs, npts)

pointer	a[ARB]			# pointer to input vectors
short	b[ARB]			# work array
pointer	c[ARB]			# pointer to associated integer vectors
int	d[ARB]			# work array
int	nvecs[npts]		# number of vectors
int	npts			# number of points in vectors

short	pivot, temp, temp3
int	i, j, k, l, p, npix, lv[LOGPTR], uv[LOGPTR], itemp
define	swap {temp=$1;$1=$2;$2=temp}
define	iswap {itemp=$1;$1=$2;$2=itemp}
define	copy_	10

begin
	do l = 0, npts-1 {
	    npix = nvecs[l+1]
	    if (npix <= 1)
		next

	    do i = 1, npix {
		b[i] = Mems[a[i]+l]
		d[i] = Memi[c[i]+l]
	    }

	    # Special cases
	    if (npix <= 3) {
		pivot = b[1]
		temp = b[2]
		if (npix == 2) {
		    if (temp < pivot) {
			b[1] = temp
			b[2] = pivot
			iswap (d[1], d[2])
		    } else
			next
		} else {
		    temp3 = b[3]
		    if (temp < pivot) {				# bac|bca|cba
			if (temp < temp3) {			# bac|bca
			    b[1] = temp
			    if (pivot < temp3) {		# bac
				b[2] = pivot
				iswap (d[1], d[2])
			    } else {				# bca
				b[2] = temp3
				b[3] = pivot
				itemp = d[2]
				d[2] = d[3]
				d[3] = d[1]
				d[1] = itemp
			    }
			} else {				# cba
			    b[1] = temp3
			    b[3] = pivot
			    iswap (d[1], d[3])
			}
		    } else if (temp3 < temp) {			# acb|cab
			b[3] = temp
			if (pivot < temp3) {			# acb
			    b[2] = temp3
			    iswap (d[2], d[3])
			} else {				# cab
			    b[1] = temp3
			    b[2] = pivot
			    itemp = d[2]
			    d[2] = d[1]
			    d[1] = d[3]
			    d[3] = itemp
			}
		    } else
			next
		}
		goto copy_
	    }

	    # General case
	    lv[1] = 1
	    uv[1] = npix
	    p = 1

	    while (p > 0) {
		if (lv[p] >= uv[p])		# only one elem in this subset
		    p = p - 1			# pop stack
		else {
		    # Dummy do loop to trigger the Fortran optimizer.
		    do p = p, ARB {
			i = lv[p] - 1
			j = uv[p]

			# Select as the pivot the element at the center of the
			# array, to avoid quadratic behavior on an already
			# sorted array.

			k = (lv[p] + uv[p]) / 2
			swap (b[j], b[k]); swap (d[j], d[k])
			pivot = b[j]		   # pivot line

			while (i < j) {
			    for (i=i+1;  b[i] < pivot;  i=i+1)
				;
			    for (j=j-1;  j > i;  j=j-1)
				if (b[j] <= pivot)
				    break
			    if (i < j) {	   # out of order pair
				swap (b[i], b[j])  # interchange elements
				swap (d[i], d[j])
			    }
			}

			j = uv[p]		   # move pivot to position i
			swap (b[i], b[j])	   # interchange elements
			swap (d[i], d[j])

			if (i-lv[p] < uv[p] - i) { # stack so shorter done first
			    lv[p+1] = lv[p]
			    uv[p+1] = i - 1
			    lv[p] = i + 1
			} else {
			    lv[p+1] = i + 1
			    uv[p+1] = uv[p]
			    uv[p] = i - 1
			}

			break
		    }
		    p = p + 1			   # push onto stack
		}
	    }

copy_	   
	    do i = 1, npix {
		Mems[a[i]+l] = b[i]
		Memi[c[i]+l] = d[i]
	    }
	}
end

# IC_SORT -- Quicksort.  This is based on the VOPS asrt except that
# the input is an array of pointers to image lines and the sort is done
# across the image lines at each point along the lines.  The number of
# valid pixels at each point is allowed to vary.  The cases of 1, 2, and 3
# pixels per point are treated specially.

procedure ic_sorti (a, b, nvecs, npts)

pointer	a[ARB]			# pointer to input vectors
int	b[ARB]			# work array
int	nvecs[npts]		# number of vectors
int	npts			# number of points in vectors

int	pivot, temp, temp3
int	i, j, k, l, p, npix, lv[LOGPTR], uv[LOGPTR]
define	swap {temp=$1;$1=$2;$2=temp}
define	copy_	10

begin
	do l = 0, npts-1 {
	    npix = nvecs[l+1]
	    if (npix <= 1)
		next

	    do i = 1, npix
		b[i] = Memi[a[i]+l]

	    # Special cases
	    if (npix <= 3) {
		pivot = b[1]
		temp = b[2]
		if (npix == 2) {
		    if (temp < pivot) {
			b[1] = temp
			b[2] = pivot
		    } else
			next
		} else {
		    temp3 = b[3]
		    if (temp < pivot) {				# bac|bca|cba
			if (temp < temp3) {			# bac|bca
			    b[1] = temp
			    if (pivot < temp3)			# bac
				b[2] = pivot
			    else {				# bca
				b[2] = temp3
				b[3] = pivot
			    }
			} else {				# cba
			    b[1] = temp3
			    b[3] = pivot
			}
		    } else if (temp3 < temp) {			# acb|cab
			b[3] = temp
			if (pivot < temp3)			# acb
			    b[2] = temp3
			else {					# cab
			    b[1] = temp3
			    b[2] = pivot
			}
		    } else
			next
		}
		goto copy_
	    }

	    # General case
	    do i = 1, npix
		b[i] = Memi[a[i]+l]

	    lv[1] = 1
	    uv[1] = npix
	    p = 1

	    while (p > 0) {
		if (lv[p] >= uv[p])		# only one elem in this subset
		    p = p - 1			# pop stack
		else {
		    # Dummy do loop to trigger the Fortran optimizer.
		    do p = p, ARB {
			i = lv[p] - 1
			j = uv[p]

			# Select as the pivot the element at the center of the
			# array, to avoid quadratic behavior on an already
			# sorted array.

			k = (lv[p] + uv[p]) / 2
			swap (b[j], b[k])
			pivot = b[j]		   # pivot line

			while (i < j) {
			    for (i=i+1;  b[i] < pivot;  i=i+1)
				;
			    for (j=j-1;  j > i;  j=j-1)
				if (b[j] <= pivot)
				    break
			    if (i < j)		   # out of order pair
				swap (b[i], b[j])  # interchange elements
			}

			j = uv[p]		   # move pivot to position i
			swap (b[i], b[j])	   # interchange elements

			if (i-lv[p] < uv[p] - i) { # stack so shorter done first
			    lv[p+1] = lv[p]
			    uv[p+1] = i - 1
			    lv[p] = i + 1
			} else {
			    lv[p+1] = i + 1
			    uv[p+1] = uv[p]
			    uv[p] = i - 1
			}

			break
		    }
		    p = p + 1			   # push onto stack
		}
	    }

copy_
	    do i = 1, npix
		Memi[a[i]+l] = b[i]
	}
end


# IC_2SORT -- Quicksort.  This is based on the VOPS asrt except that
# the input is an array of pointers to image lines and the sort is done
# across the image lines at each point along the lines.  The number of
# valid pixels at each point is allowed to vary.  The cases of 1, 2, and 3
# pixels per point are treated specially.  A second integer set of
# vectors is sorted.

procedure ic_2sorti (a, b, c, d, nvecs, npts)

pointer	a[ARB]			# pointer to input vectors
int	b[ARB]			# work array
pointer	c[ARB]			# pointer to associated integer vectors
int	d[ARB]			# work array
int	nvecs[npts]		# number of vectors
int	npts			# number of points in vectors

int	pivot, temp, temp3
int	i, j, k, l, p, npix, lv[LOGPTR], uv[LOGPTR], itemp
define	swap {temp=$1;$1=$2;$2=temp}
define	iswap {itemp=$1;$1=$2;$2=itemp}
define	copy_	10

begin
	do l = 0, npts-1 {
	    npix = nvecs[l+1]
	    if (npix <= 1)
		next

	    do i = 1, npix {
		b[i] = Memi[a[i]+l]
		d[i] = Memi[c[i]+l]
	    }

	    # Special cases
	    if (npix <= 3) {
		pivot = b[1]
		temp = b[2]
		if (npix == 2) {
		    if (temp < pivot) {
			b[1] = temp
			b[2] = pivot
			iswap (d[1], d[2])
		    } else
			next
		} else {
		    temp3 = b[3]
		    if (temp < pivot) {				# bac|bca|cba
			if (temp < temp3) {			# bac|bca
			    b[1] = temp
			    if (pivot < temp3) {		# bac
				b[2] = pivot
				iswap (d[1], d[2])
			    } else {				# bca
				b[2] = temp3
				b[3] = pivot
				itemp = d[2]
				d[2] = d[3]
				d[3] = d[1]
				d[1] = itemp
			    }
			} else {				# cba
			    b[1] = temp3
			    b[3] = pivot
			    iswap (d[1], d[3])
			}
		    } else if (temp3 < temp) {			# acb|cab
			b[3] = temp
			if (pivot < temp3) {			# acb
			    b[2] = temp3
			    iswap (d[2], d[3])
			} else {				# cab
			    b[1] = temp3
			    b[2] = pivot
			    itemp = d[2]
			    d[2] = d[1]
			    d[1] = d[3]
			    d[3] = itemp
			}
		    } else
			next
		}
		goto copy_
	    }

	    # General case
	    lv[1] = 1
	    uv[1] = npix
	    p = 1

	    while (p > 0) {
		if (lv[p] >= uv[p])		# only one elem in this subset
		    p = p - 1			# pop stack
		else {
		    # Dummy do loop to trigger the Fortran optimizer.
		    do p = p, ARB {
			i = lv[p] - 1
			j = uv[p]

			# Select as the pivot the element at the center of the
			# array, to avoid quadratic behavior on an already
			# sorted array.

			k = (lv[p] + uv[p]) / 2
			swap (b[j], b[k]); swap (d[j], d[k])
			pivot = b[j]		   # pivot line

			while (i < j) {
			    for (i=i+1;  b[i] < pivot;  i=i+1)
				;
			    for (j=j-1;  j > i;  j=j-1)
				if (b[j] <= pivot)
				    break
			    if (i < j) {	   # out of order pair
				swap (b[i], b[j])  # interchange elements
				swap (d[i], d[j])
			    }
			}

			j = uv[p]		   # move pivot to position i
			swap (b[i], b[j])	   # interchange elements
			swap (d[i], d[j])

			if (i-lv[p] < uv[p] - i) { # stack so shorter done first
			    lv[p+1] = lv[p]
			    uv[p+1] = i - 1
			    lv[p] = i + 1
			} else {
			    lv[p+1] = i + 1
			    uv[p+1] = uv[p]
			    uv[p] = i - 1
			}

			break
		    }
		    p = p + 1			   # push onto stack
		}
	    }

copy_	   
	    do i = 1, npix {
		Memi[a[i]+l] = b[i]
		Memi[c[i]+l] = d[i]
	    }
	}
end

# IC_SORT -- Quicksort.  This is based on the VOPS asrt except that
# the input is an array of pointers to image lines and the sort is done
# across the image lines at each point along the lines.  The number of
# valid pixels at each point is allowed to vary.  The cases of 1, 2, and 3
# pixels per point are treated specially.

procedure ic_sortr (a, b, nvecs, npts)

pointer	a[ARB]			# pointer to input vectors
real	b[ARB]			# work array
int	nvecs[npts]		# number of vectors
int	npts			# number of points in vectors

real	pivot, temp, temp3
int	i, j, k, l, p, npix, lv[LOGPTR], uv[LOGPTR]
define	swap {temp=$1;$1=$2;$2=temp}
define	copy_	10

begin
	do l = 0, npts-1 {
	    npix = nvecs[l+1]
	    if (npix <= 1)
		next

	    do i = 1, npix
		b[i] = Memr[a[i]+l]

	    # Special cases
	    if (npix <= 3) {
		pivot = b[1]
		temp = b[2]
		if (npix == 2) {
		    if (temp < pivot) {
			b[1] = temp
			b[2] = pivot
		    } else
			next
		} else {
		    temp3 = b[3]
		    if (temp < pivot) {				# bac|bca|cba
			if (temp < temp3) {			# bac|bca
			    b[1] = temp
			    if (pivot < temp3)			# bac
				b[2] = pivot
			    else {				# bca
				b[2] = temp3
				b[3] = pivot
			    }
			} else {				# cba
			    b[1] = temp3
			    b[3] = pivot
			}
		    } else if (temp3 < temp) {			# acb|cab
			b[3] = temp
			if (pivot < temp3)			# acb
			    b[2] = temp3
			else {					# cab
			    b[1] = temp3
			    b[2] = pivot
			}
		    } else
			next
		}
		goto copy_
	    }

	    # General case
	    do i = 1, npix
		b[i] = Memr[a[i]+l]

	    lv[1] = 1
	    uv[1] = npix
	    p = 1

	    while (p > 0) {
		if (lv[p] >= uv[p])		# only one elem in this subset
		    p = p - 1			# pop stack
		else {
		    # Dummy do loop to trigger the Fortran optimizer.
		    do p = p, ARB {
			i = lv[p] - 1
			j = uv[p]

			# Select as the pivot the element at the center of the
			# array, to avoid quadratic behavior on an already
			# sorted array.

			k = (lv[p] + uv[p]) / 2
			swap (b[j], b[k])
			pivot = b[j]		   # pivot line

			while (i < j) {
			    for (i=i+1;  b[i] < pivot;  i=i+1)
				;
			    for (j=j-1;  j > i;  j=j-1)
				if (b[j] <= pivot)
				    break
			    if (i < j)		   # out of order pair
				swap (b[i], b[j])  # interchange elements
			}

			j = uv[p]		   # move pivot to position i
			swap (b[i], b[j])	   # interchange elements

			if (i-lv[p] < uv[p] - i) { # stack so shorter done first
			    lv[p+1] = lv[p]
			    uv[p+1] = i - 1
			    lv[p] = i + 1
			} else {
			    lv[p+1] = i + 1
			    uv[p+1] = uv[p]
			    uv[p] = i - 1
			}

			break
		    }
		    p = p + 1			   # push onto stack
		}
	    }

copy_
	    do i = 1, npix
		Memr[a[i]+l] = b[i]
	}
end


# IC_2SORT -- Quicksort.  This is based on the VOPS asrt except that
# the input is an array of pointers to image lines and the sort is done
# across the image lines at each point along the lines.  The number of
# valid pixels at each point is allowed to vary.  The cases of 1, 2, and 3
# pixels per point are treated specially.  A second integer set of
# vectors is sorted.

procedure ic_2sortr (a, b, c, d, nvecs, npts)

pointer	a[ARB]			# pointer to input vectors
real	b[ARB]			# work array
pointer	c[ARB]			# pointer to associated integer vectors
int	d[ARB]			# work array
int	nvecs[npts]		# number of vectors
int	npts			# number of points in vectors

real	pivot, temp, temp3
int	i, j, k, l, p, npix, lv[LOGPTR], uv[LOGPTR], itemp
define	swap {temp=$1;$1=$2;$2=temp}
define	iswap {itemp=$1;$1=$2;$2=itemp}
define	copy_	10

begin
	do l = 0, npts-1 {
	    npix = nvecs[l+1]
	    if (npix <= 1)
		next

	    do i = 1, npix {
		b[i] = Memr[a[i]+l]
		d[i] = Memi[c[i]+l]
	    }

	    # Special cases
	    if (npix <= 3) {
		pivot = b[1]
		temp = b[2]
		if (npix == 2) {
		    if (temp < pivot) {
			b[1] = temp
			b[2] = pivot
			iswap (d[1], d[2])
		    } else
			next
		} else {
		    temp3 = b[3]
		    if (temp < pivot) {				# bac|bca|cba
			if (temp < temp3) {			# bac|bca
			    b[1] = temp
			    if (pivot < temp3) {		# bac
				b[2] = pivot
				iswap (d[1], d[2])
			    } else {				# bca
				b[2] = temp3
				b[3] = pivot
				itemp = d[2]
				d[2] = d[3]
				d[3] = d[1]
				d[1] = itemp
			    }
			} else {				# cba
			    b[1] = temp3
			    b[3] = pivot
			    iswap (d[1], d[3])
			}
		    } else if (temp3 < temp) {			# acb|cab
			b[3] = temp
			if (pivot < temp3) {			# acb
			    b[2] = temp3
			    iswap (d[2], d[3])
			} else {				# cab
			    b[1] = temp3
			    b[2] = pivot
			    itemp = d[2]
			    d[2] = d[1]
			    d[1] = d[3]
			    d[3] = itemp
			}
		    } else
			next
		}
		goto copy_
	    }

	    # General case
	    lv[1] = 1
	    uv[1] = npix
	    p = 1

	    while (p > 0) {
		if (lv[p] >= uv[p])		# only one elem in this subset
		    p = p - 1			# pop stack
		else {
		    # Dummy do loop to trigger the Fortran optimizer.
		    do p = p, ARB {
			i = lv[p] - 1
			j = uv[p]

			# Select as the pivot the element at the center of the
			# array, to avoid quadratic behavior on an already
			# sorted array.

			k = (lv[p] + uv[p]) / 2
			swap (b[j], b[k]); swap (d[j], d[k])
			pivot = b[j]		   # pivot line

			while (i < j) {
			    for (i=i+1;  b[i] < pivot;  i=i+1)
				;
			    for (j=j-1;  j > i;  j=j-1)
				if (b[j] <= pivot)
				    break
			    if (i < j) {	   # out of order pair
				swap (b[i], b[j])  # interchange elements
				swap (d[i], d[j])
			    }
			}

			j = uv[p]		   # move pivot to position i
			swap (b[i], b[j])	   # interchange elements
			swap (d[i], d[j])

			if (i-lv[p] < uv[p] - i) { # stack so shorter done first
			    lv[p+1] = lv[p]
			    uv[p+1] = i - 1
			    lv[p] = i + 1
			} else {
			    lv[p+1] = i + 1
			    uv[p+1] = uv[p]
			    uv[p] = i - 1
			}

			break
		    }
		    p = p + 1			   # push onto stack
		}
	    }

copy_	   
	    do i = 1, npix {
		Memr[a[i]+l] = b[i]
		Memi[c[i]+l] = d[i]
	    }
	}
end

# IC_SORT -- Quicksort.  This is based on the VOPS asrt except that
# the input is an array of pointers to image lines and the sort is done
# across the image lines at each point along the lines.  The number of
# valid pixels at each point is allowed to vary.  The cases of 1, 2, and 3
# pixels per point are treated specially.

procedure ic_sortd (a, b, nvecs, npts)

pointer	a[ARB]			# pointer to input vectors
double	b[ARB]			# work array
int	nvecs[npts]		# number of vectors
int	npts			# number of points in vectors

double	pivot, temp, temp3
int	i, j, k, l, p, npix, lv[LOGPTR], uv[LOGPTR]
define	swap {temp=$1;$1=$2;$2=temp}
define	copy_	10

begin
	do l = 0, npts-1 {
	    npix = nvecs[l+1]
	    if (npix <= 1)
		next

	    do i = 1, npix
		b[i] = Memd[a[i]+l]

	    # Special cases
	    if (npix <= 3) {
		pivot = b[1]
		temp = b[2]
		if (npix == 2) {
		    if (temp < pivot) {
			b[1] = temp
			b[2] = pivot
		    } else
			next
		} else {
		    temp3 = b[3]
		    if (temp < pivot) {				# bac|bca|cba
			if (temp < temp3) {			# bac|bca
			    b[1] = temp
			    if (pivot < temp3)			# bac
				b[2] = pivot
			    else {				# bca
				b[2] = temp3
				b[3] = pivot
			    }
			} else {				# cba
			    b[1] = temp3
			    b[3] = pivot
			}
		    } else if (temp3 < temp) {			# acb|cab
			b[3] = temp
			if (pivot < temp3)			# acb
			    b[2] = temp3
			else {					# cab
			    b[1] = temp3
			    b[2] = pivot
			}
		    } else
			next
		}
		goto copy_
	    }

	    # General case
	    do i = 1, npix
		b[i] = Memd[a[i]+l]

	    lv[1] = 1
	    uv[1] = npix
	    p = 1

	    while (p > 0) {
		if (lv[p] >= uv[p])		# only one elem in this subset
		    p = p - 1			# pop stack
		else {
		    # Dummy do loop to trigger the Fortran optimizer.
		    do p = p, ARB {
			i = lv[p] - 1
			j = uv[p]

			# Select as the pivot the element at the center of the
			# array, to avoid quadratic behavior on an already
			# sorted array.

			k = (lv[p] + uv[p]) / 2
			swap (b[j], b[k])
			pivot = b[j]		   # pivot line

			while (i < j) {
			    for (i=i+1;  b[i] < pivot;  i=i+1)
				;
			    for (j=j-1;  j > i;  j=j-1)
				if (b[j] <= pivot)
				    break
			    if (i < j)		   # out of order pair
				swap (b[i], b[j])  # interchange elements
			}

			j = uv[p]		   # move pivot to position i
			swap (b[i], b[j])	   # interchange elements

			if (i-lv[p] < uv[p] - i) { # stack so shorter done first
			    lv[p+1] = lv[p]
			    uv[p+1] = i - 1
			    lv[p] = i + 1
			} else {
			    lv[p+1] = i + 1
			    uv[p+1] = uv[p]
			    uv[p] = i - 1
			}

			break
		    }
		    p = p + 1			   # push onto stack
		}
	    }

copy_
	    do i = 1, npix
		Memd[a[i]+l] = b[i]
	}
end


# IC_2SORT -- Quicksort.  This is based on the VOPS asrt except that
# the input is an array of pointers to image lines and the sort is done
# across the image lines at each point along the lines.  The number of
# valid pixels at each point is allowed to vary.  The cases of 1, 2, and 3
# pixels per point are treated specially.  A second integer set of
# vectors is sorted.

procedure ic_2sortd (a, b, c, d, nvecs, npts)

pointer	a[ARB]			# pointer to input vectors
double	b[ARB]			# work array
pointer	c[ARB]			# pointer to associated integer vectors
int	d[ARB]			# work array
int	nvecs[npts]		# number of vectors
int	npts			# number of points in vectors

double	pivot, temp, temp3
int	i, j, k, l, p, npix, lv[LOGPTR], uv[LOGPTR], itemp
define	swap {temp=$1;$1=$2;$2=temp}
define	iswap {itemp=$1;$1=$2;$2=itemp}
define	copy_	10

begin
	do l = 0, npts-1 {
	    npix = nvecs[l+1]
	    if (npix <= 1)
		next

	    do i = 1, npix {
		b[i] = Memd[a[i]+l]
		d[i] = Memi[c[i]+l]
	    }

	    # Special cases
	    if (npix <= 3) {
		pivot = b[1]
		temp = b[2]
		if (npix == 2) {
		    if (temp < pivot) {
			b[1] = temp
			b[2] = pivot
			iswap (d[1], d[2])
		    } else
			next
		} else {
		    temp3 = b[3]
		    if (temp < pivot) {				# bac|bca|cba
			if (temp < temp3) {			# bac|bca
			    b[1] = temp
			    if (pivot < temp3) {		# bac
				b[2] = pivot
				iswap (d[1], d[2])
			    } else {				# bca
				b[2] = temp3
				b[3] = pivot
				itemp = d[2]
				d[2] = d[3]
				d[3] = d[1]
				d[1] = itemp
			    }
			} else {				# cba
			    b[1] = temp3
			    b[3] = pivot
			    iswap (d[1], d[3])
			}
		    } else if (temp3 < temp) {			# acb|cab
			b[3] = temp
			if (pivot < temp3) {			# acb
			    b[2] = temp3
			    iswap (d[2], d[3])
			} else {				# cab
			    b[1] = temp3
			    b[2] = pivot
			    itemp = d[2]
			    d[2] = d[1]
			    d[1] = d[3]
			    d[3] = itemp
			}
		    } else
			next
		}
		goto copy_
	    }

	    # General case
	    lv[1] = 1
	    uv[1] = npix
	    p = 1

	    while (p > 0) {
		if (lv[p] >= uv[p])		# only one elem in this subset
		    p = p - 1			# pop stack
		else {
		    # Dummy do loop to trigger the Fortran optimizer.
		    do p = p, ARB {
			i = lv[p] - 1
			j = uv[p]

			# Select as the pivot the element at the center of the
			# array, to avoid quadratic behavior on an already
			# sorted array.

			k = (lv[p] + uv[p]) / 2
			swap (b[j], b[k]); swap (d[j], d[k])
			pivot = b[j]		   # pivot line

			while (i < j) {
			    for (i=i+1;  b[i] < pivot;  i=i+1)
				;
			    for (j=j-1;  j > i;  j=j-1)
				if (b[j] <= pivot)
				    break
			    if (i < j) {	   # out of order pair
				swap (b[i], b[j])  # interchange elements
				swap (d[i], d[j])
			    }
			}

			j = uv[p]		   # move pivot to position i
			swap (b[i], b[j])	   # interchange elements
			swap (d[i], d[j])

			if (i-lv[p] < uv[p] - i) { # stack so shorter done first
			    lv[p+1] = lv[p]
			    uv[p+1] = i - 1
			    lv[p] = i + 1
			} else {
			    lv[p+1] = i + 1
			    uv[p+1] = uv[p]
			    uv[p] = i - 1
			}

			break
		    }
		    p = p + 1			   # push onto stack
		}
	    }

copy_	   
	    do i = 1, npix {
		Memd[a[i]+l] = b[i]
		Memi[c[i]+l] = d[i]
	    }
	}
end

