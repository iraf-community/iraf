# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

define	LOGPTR 	32			# log2(maxpts)  (4e9)

# QSORT -- General quicksort for arbitrary objects.  X is an integer array
# indexing the array to be sorted.  The user supplied COMPARE function is used
# to compare objects indexed by X:
# 
# 	-1,0,1 = compare (x1, x2)
# 
# where the value returned by COMPARE has the following significance:
# 
# 	-1	obj[x1]  < obj[x2]
# 	 0	obj[x1] == obj[x2]
# 	 1	obj[x1]  > obj[x2]
# 
# QSORT reorders the elements of the X array, which must be of type integer.
# **NOTE** - See also gqsort.x, a more recent version of this routine.

procedure qsort (x, nelem, compare)

int	x[ARB]			# array to be sorted
int	nelem			# number of elements in array
extern	compare()		# function to be called to compare elements

int	i, j, k, lv[LOGPTR], p, pivot, uv[LOGPTR], temp
define	swap {temp=$1;$1=$2;$2=temp}
int	compare()

begin
	lv[1] = 1
	uv[1] = nelem
	p = 1

	while (p > 0) {
	    if (lv[p] >= uv[p])			# only one elem in this subset
		p = p - 1			# pop stack
	    else {
		# Dummy loop to trigger the optimizer.
		do p = p, ARB {
		    i = lv[p] - 1
		    j = uv[p]

		    # Select as the pivot the element at the center of the
		    # subfile, to avoid quadratic behavior on an already
		    # sorted list.

		    k = (lv[p] + uv[p]) / 2
		    swap (x[j], x[k])
		    pivot = x[j]			# pivot line

		    while (i < j) {
			for (i=i+1;  compare (x[i], pivot) < 0;  i=i+1)
			    ;
			for (j=j-1;  j > i;  j=j-1)
			    if (compare (x[j], pivot) <= 0)
				break
			if (i < j)			# out of order pair
			    swap (x[i], x[j])	# interchange elements
		    }

		    j = uv[p]			# move pivot to position i
		    swap (x[i], x[j])		# interchange elements

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
