include	"reloperr.h"

.help tbl_sortm
.nf____________________________________________________________________________

This file contains two routines that sort a table on multiple columns. Both
routines put an existing array of row indices into sorted order. The first
routine, tbl_sortm has a simpler interface and is the routine to be used in
a majority of cases. The second routine, tbl_msort, requires that the calling
routine read the table column into an array and handle null elements by
itself. This routine should be used if the table column requires some
special preprocessing before it can be sorted. One example of required
preprocessing is conversion of dates from character strings to julian dates.
Both routines use merge sort to sort the data. Merge sort is fast, though not
as fast as quick sort, and stable, so it can be used to sort on multiple
columns. Its disadvantage is that it requires additional work space to run.

.endhelp_______________________________________________________________________

# TBL_SORTM -- Sort a table on multiple columns
#
# This procedure rearranges an array of row indices into sorted order. The
# order is from smallest to largest value if ascend is true, if ascend is
# false, the order is from largest to smallest. In either case undefined
# elements will be last in the array. For purposes of this routine boolean
# false is considered to be less than true. If character strings are being
# sorted, case can be ignored by setting casesens to false. The array of row
# indices must be created before calling this procedure.
#
# B.Simon	28-Sept-87	First Code
# B.Simon	15-Dec-87	Changed to handle table subsets

procedure tbl_sortm (ascend, casesens, tp, numptr, colptr, nindex, index)

bool	ascend		#  i: Sort in ascending order
bool	casesens	#  i: Sort is case sensitive
pointer	tp		#  i: Table descriptor
int	numptr		#  i: Number of columns to sort on
pointer	colptr[ARB]	#  i: Array of column descriptors
int	nindex		# io: Number of rows
int	index[ARB]	# io: Array of row indices in sorted order
#--
int	dtype, spptype, lendata
int	iptr, nary, iary, nelem, nidx
pointer	cp, idxptr, nulptr, aryptr, curptr

int	movenulls()

begin
	# Allocate storage for index array

	nidx = 2 * nindex
	call malloc (idxptr, nidx, TY_INT)

	# Initialize the array of row indices

	call amovi (index, Memi[idxptr], nindex)

	# Loop over all columns to be sorted

	do iptr = numptr, 1, -1 {

	    cp = colptr(iptr)

	    # Read in the column of table values. Setting dtype to zero
	    # gets the actual column type.

	    dtype = 0
	    call gettabcol (tp, cp, dtype, nary, aryptr, nulptr)

	    if (dtype < 0) {
		lendata = - dtype
	    	spptype = TY_CHAR

		if (! casesens) {
		    curptr = aryptr
		    do iary = 1, nary {
			call strupr (Memc[curptr])
			curptr = curptr + lendata + 1
		    }
		}
	    } else {
		lendata = 1
		spptype = dtype
	    }

	    # Move all null elements to the end of the array

	    nelem = movenulls (nindex, Memb[nulptr], Memi[idxptr])

	    # Perform an indirect sort on the row indices using merge sort

	    call tbl_msort (ascend, dtype, aryptr, nelem, nidx, idxptr)

	    # Free memory used to hold table column and null flags

	    call mfree (aryptr, spptype)
	    call mfree (nulptr, TY_BOOL)
	}

	# Move the row indices into the output array

	call amovi (Memi[idxptr], index, nindex)
	call mfree (idxptr, TY_INT)

end

# TBL_MSORT -- Indirect merge sort of a table column using an index array

procedure tbl_msort (ascend, dtype, aryptr, nelem, nidx, idxptr)

bool	ascend		# i: Sort array in ascending order
int	dtype		# i: Data type of array to be sorted
pointer	aryptr		# i: Pointer to array to be sorted
int	nelem		# i: Number of array elements to be sorted
int	nidx		# i: Size of index array
pointer	idxptr		# o: Pointer to array of indices

include	"compare.com"

int	spptype

extern	compascb, compascd, compasci, compascr, compasct
extern	compdscb, compdscd, compdsci, compdscr, compdsct

begin
	dataptr = aryptr

	if (dtype < 0) {
	    lendata = - dtype
	    spptype = TY_CHAR
	} else {
	    lendata = 1
	    spptype = dtype
	}

	# Convert the type to the SPP format

	# Call the merge sort procedure with the proper comparison routine

	switch (spptype) { 
	case TY_BOOL:
	    if (ascend)
		call msort (Memi[idxptr], nidx, nelem, compascb)
	    else
		call msort (Memi[idxptr], nidx, nelem, compdscb)
	case TY_CHAR:
	    if (ascend)
		call msort (Memi[idxptr], nidx, nelem, compasct)
	    else
		call msort (Memi[idxptr], nidx, nelem, compdsct)
	case TY_SHORT,TY_INT,TY_LONG:
	    if (ascend)
		call msort (Memi[idxptr], nidx, nelem, compasci)
	    else
		call msort (Memi[idxptr], nidx, nelem, compdsci)
	case TY_REAL:
	    if (ascend)
		call msort (Memi[idxptr], nidx, nelem, compascr)
	    else
		call msort (Memi[idxptr], nidx, nelem, compdscr)
	case TY_DOUBLE: 
	    if (ascend)
		call msort (Memi[idxptr], nidx, nelem, compascd)
	    else
		call msort (Memi[idxptr], nidx, nelem, compdscd)
	}
end
