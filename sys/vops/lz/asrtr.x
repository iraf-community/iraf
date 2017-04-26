# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

define	LOGPTR	32			# log2(maxpts) (4e9)

# ASRT -- Vector Quicksort.  The output vector may be the same as the
# input vector.

procedure asrtr (a, b, npix)

real	a[ARB], b[ARB]		# input, output arrays
int	npix			# number of pixels

real	pivot, temp
int	i, j, k, p, lv[LOGPTR], uv[LOGPTR]
define	swap {temp=$1;$1=$2;$2=temp}

begin
	call amovr (a, b, npix)		# in place sort

	lv[1] = 1
	uv[1] = npix
	p = 1

	while (p > 0) {
	    if (lv[p] >= uv[p])			# only one elem in this subset
		p = p - 1			# pop stack
	    else {
		# Dummy do loop to trigger the Fortran optimizer.
		do p = p, ARB {
		    i = lv[p] - 1
		    j = uv[p]

		    # Select as the pivot the element at the center of the
		    # array, to avoid quadratic behavior on an already sorted
		    # array.

		    k = (lv[p] + uv[p]) / 2
		    swap (b[j], b[k])
		    pivot = b[j]			# pivot line

		    while (i < j) {
			for (i=i+1;  b[i] < pivot;  i=i+1)
			    ;
			for (j=j-1;  j > i;  j=j-1)
			    if (b[j] <= pivot)
				break
			if (i < j)		# out of order pair
			    swap (b[i], b[j])	# interchange elements
		    }

		    j = uv[p]			# move pivot to position i
		    swap (b[i], b[j])		# interchange elements

		    if (i-lv[p] < uv[p] - i) {	# stack so shorter done first
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
		p = p + 1			# push onto stack
	    }
	}
end
