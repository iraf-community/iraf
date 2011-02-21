# MOVENULLS -- Move all null elements to the end of the index array
#
# This procedure rearranges an array of row indices so that all rows with
# nulls in a particular column are moved to the end of the index array.
# The position of the nulls in the column is indicated by an array of null
# flags, whose length might be greater than the length of the array of
# indices, i.e., only a subset of the rows in a table might be in the index
# array.
#
# B.Simon	15-Dec-87	First Code

int procedure movenulls (nindex, nulflg, index) 

int	nindex		#  i: Number of indices
bool	nulflg[ARB]	#  i: Array of null flags
int	index[ARB]	# io: Array of row indices
#--
int	nelem, idx, jdx

begin
	nelem = nindex

	do idx = nindex, 1, -1 {
	    jdx = index[idx]
	    if (nulflg[jdx]) {
		if (nelem != idx) {
		    index[idx] = index[nelem]
		    index[nelem] = jdx
		}
		nelem = nelem - 1
	    }
	}

	return (nelem)
end
