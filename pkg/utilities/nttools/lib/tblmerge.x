include	"reloperr.h"

# TBL_MERGE -- Merge two tables on the basis of a common column
#
# This procedure creates an array of row indices from two tables where the
# row indices point to a pair of rows where the values stored in the two
# columns are equal within an input tolerance. The column values are stored in
# the two arrays pointed to by aryptr. The two columns must already be sorted
# in ascending order with the row indices of the two columns stored in the
# arrays pointed to by idxptr. This procedure keeps on going even when an
# output array overflow condition is detected so that the caller knows how
# large the output array must be.
#
# B.Simon	 1-Nov-87	First code
# B.Simon	16-Dec-87	Changed to handle table subsets

procedure tbl_merge (tol, dtype, nary, aryptr, nidx, idxptr, nmax,
		     nmerge, index1, index2)

double	tol		# i: Tolerance used in test for equality
int	dtype[2]	# i: Data types of columns
int	nary[2]		# i: Size of arrays containing columns
pointer	aryptr[2]	# i: Pointers to column arrays
int	nidx[2]		# i: Size of arrays containing row indices
pointer	idxptr[2]	# i: Pointers to index arrays
int	nmax		# i: Max size of arrays containing merged row indices
int	nmerge		# o: Number of merged row indices
int	index1[ARB]	# o: Array of merged row indices for first table
int	index2[ARB]	# o: Array of merged row indices for second table
#--
double	dbl_tol
int	itab, int_tol, idx, jdx, kdx, order, lendata[2], spptype[2]
pointer	ptr1, ptr2
real	real_tol

bool	strlt(), strgt()

string	badtype "Data types of the two columns to be merged must be equal"
string	badtol	"Tolerance for boolean or character columns must be zero"

begin
	# Get data type and length from dtype

	do itab = 1, 2 {
	    if (dtype[itab] < 0) {
		lendata[itab] = 1 - dtype[itab]
		spptype[itab] = TY_CHAR
	    } else {
		lendata[itab] = 1
		spptype[itab] = dtype[itab]
	    }
	}

	if (spptype[1] != spptype[2])
	    call error (SYNTAX, badtype)

	# Convert tolerance to the same type as the data

	switch (spptype[1]) {
	case TY_BOOL, TY_CHAR:
	    if (tol > 0.0)
		call error (SYNTAX, badtol)
	case TY_SHORT, TY_INT, TY_LONG:
	    int_tol = tol
	case TY_REAL:
	    real_tol = tol
	case TY_DOUBLE:
	    dbl_tol = tol
	}

	idx = 1
	jdx = 1
	kdx = 1

	nmerge = 0
	while (idx <= nidx[1] && jdx <= nidx[2]) {

	    # Calculate addresses of array elements

	    ptr1 = aryptr[1] + lendata[1] * (Memi[idxptr[1]+idx-1] - 1)
	    ptr2 = aryptr[2] + lendata[2] * (Memi[idxptr[2]+jdx-1] - 1)

	    # Determine relative order of the two elements
	    # If mem[ptr1] < mem[ptr2],  order = -1
	    # If mem[ptr1] == mem[ptr2], order = 0
	    # If mem[ptr1] > mem[ptr2],  order = 1

	    switch (spptype[1]) {
	    case TY_BOOL:
		# false < true

		if (! Memb[ptr1] && Memb[ptr2])
		    order = -1
		else if (Memb[ptr1] && ! Memb[ptr2])
		    order = 1
		else
		    order = 0
	    case TY_CHAR:
		if (strlt (Memc[ptr1], Memc[ptr2]))
		    order = -1
		else if (strgt (Memc[ptr1], Memc[ptr2]))
		    order = 1
		else
		    order = 0
	    case TY_SHORT,TY_INT, TY_LONG:
		if (Memi[ptr1] + int_tol < Memi[ptr2])
		    order = -1
		else if (Memi[ptr1] > Memi[ptr2] + int_tol)
		    order = 1
		else
		    order = 0
	    case TY_REAL:
		if (Memr[ptr1] + real_tol < Memr[ptr2])
		    order = -1
		else if (Memr[ptr1] > Memr[ptr2] + real_tol)
		    order = 1
		else
		    order = 0
	    case TY_DOUBLE:
		if (Memd[ptr1] + dbl_tol < Memd[ptr2])
		    order = -1
		else if (Memd[ptr1] > Memd[ptr2] + dbl_tol)
		    order = 1
		else
		    order = 0
	    }

	    # Increment the indices to the two arrays and if a match is
	    # found, add it to the index array.

	    # The third index, kdx, tells where to fall back to when the
	    # value in the first array exceeds the value in the second array.
	    # Because the arrays are sorted in ascending order, the array
	    # element pointed to by idx exceeds all those previous to the
	    # element pointed to by kdx, so there is no use checking them.

	    switch (order) {
	    case -1:
		idx = idx + 1
		jdx = kdx
	    case 0:              
		nmerge = nmerge + 1
		if (nmerge <= nmax) {
		    index1[nmerge] = Memi[idxptr[1]+idx-1]
		    index2[nmerge] = Memi[idxptr[2]+jdx-1]
		}

		# Keep fron reading past the end of the array

		if (jdx < nidx[2]) {
		    jdx = jdx + 1
		} else {
		    idx = idx + 1
		    jdx = kdx
		}
	    case 1:
		jdx = jdx + 1
		kdx = jdx
	    }
	}

end
