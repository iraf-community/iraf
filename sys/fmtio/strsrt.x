# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

define	LOGPTR	20		# log2(maxpts)  (1e6)

# STRSRT -- Sort a list of strings, given an array of indices pointing into a
# string buffer (e.g., sbuf=1 is Memc).  The sort is performed by permutation
# of the index array.

procedure strsrt (x, sb, nstr)

int	x[ARB]			# array of string pointers or indices
char	sb[ARB]			# string buffer
int	nstr			# number of strings

int	i, j, k, p, temp
int	lv[LOGPTR], uv[LOGPTR], pivot
define	swap {temp=$1;$1=$2;$2=temp}
int	strcmp()

begin
	lv[1] = 1
	uv[1] = nstr
	p = 1

	while (p > 0) {
	    if (lv[p] >= uv[p])			# only one elem in this subset
		p = p - 1			# pop stack
	    else {
		# Dummy do-loop to trigger optimizer.
		do p = p, ARB {
		    i = lv[p] - 1
		    j = uv[p]

		    # Select as the pivot the element at the middle of the
		    # subfile; move it to the end of the range so that the
		    # for loops below do not have to skip over it.  Selecting
		    # a pivot near the center of the subfile helps prevent
		    # quadratic behavior when sorting an already sorted array.

		    k = (lv[p] + uv[p]) / 2
		    swap (x[j], x[k])
		    pivot = x[j]

		    while (i < j) {
			for (i=i+1;  strcmp (sb[x[i]], sb[pivot]) < 0;  i=i+1)
			    ;
			for (j=j-1;  j > i;  j=j-1)
			    if (strcmp (sb[x[j]], sb[pivot]) <= 0)
				break
			if (i < j)		# out of order pair
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
