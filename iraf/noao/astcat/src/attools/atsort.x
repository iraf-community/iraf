define  LOGPTR          32
define  swap            {temp=$1;$1=$2;$2=temp}

# AT_SSQUICK -- Quicksort for text data.  NOTE -- This algorithm is quadratic in
# the worst case, i.e., when the data is already sorted.  A random method of
# selecting the pivot should be used to improve the behaviour on sorted arrays.

procedure at_ssquick (linbuf, linptr, index, nlines)

char    linbuf[ARB]             #I the input string buffer
int     linptr[ARB]             #U the indices of strings in buffer
int	index[ARB]		#O the output sort index
int     nlines                  #I the number of strings

int     i, j, k, temp, lv[LOGPTR], p, pivlin, uv[LOGPTR]
int     strncmp()

begin
        lv[1] = 1
        uv[1] = nlines
        p = 1

	do i = 1, nlines
	    index[i] = i

        while (p > 0) {
            if (lv[p] >= uv[p])         # only one elem in this subset
                p = p - 1               # pop stack
            else {
                # Dummy loop to trigger optimizer.
                do p = p, ARB {
                    i = lv[p] - 1
                    j = uv[p]

                    # Select pivot element at midpoint of interval to avoid
                    # quadratic behavior on a sorted list.

                    k = (lv[p] + uv[p]) / 2
                    swap (linptr[j], linptr[k])
                    swap (index[j], index[k])
                    pivlin = linptr[j]
                    while (i < j) {
                        for (i=i+1; strncmp (linbuf, linptr[i], pivlin) < 0;
                            i=i+1)
                            ;
                        for (j=j-1;  j > i;  j=j-1)
                            if (strncmp (linbuf, linptr[j], pivlin) <= 0)
                                break
                        if (i < j) {              # out of order pair
                            swap (linptr[i], linptr[j])
			    swap (index[i], index[j])
			}

                    }

                    j = uv[p]                   # move pivot to position i
                    swap (linptr[i], linptr[j])
                    swap (index[i], index[j])

                    if (i-lv[p] < uv[p] - i) {  # stack so shorter done first
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

                p = p + 1                       # push onto stack
            }
        }
end
