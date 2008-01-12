include	"reloperr.h"

.help tbl_sort1
.nf____________________________________________________________________________

This file contains two routines that sort a table on a single column. Both
routines put an existing array of row indices into sorted order. The first
routine, tbl_sort1 has a simpler interface and is the routine to be used in
a majority of cases. The second routine, tbl_qsort, requires that the calling
routine read the table column into an array and handle null elements by
itself. This routine should be used if the table column requires some
special preprocessing before it can be sorted. One example of required
preprocessing is conversion of dates from character strings to julian dates.
Both routines use quick sort to sort the data. Quick is one of the fastest
sorting routines, but it cannot be used to sort several table columns because
it is not stable. This means that one sort destroys the ordering of a previous
sort on a different column.

.endhelp_______________________________________________________________________

# TBL_SORT1 -- Sort a table on a single column
#
# This procedure rearranges an array of row indices into sorted order. The
# order is from smallest to largest value if ascend is true, if ascend is
# false, the order is from largest to smallest. In either case undefined
# elements will be last in the array. For purposes of this routine boolean
# false is considered to be less than true. If character strings are being
# sorted, case can be ignored by setting casesens to false. The array of row
# indices must be created before calling this procedure.
#
# B.Simon	16-Sept-87	First Code
# B.Simon	15-Dec-87	Changed to handle table subsets

procedure tbl_sort1 (ascend, casesens, tp, cp, nindex, index)

bool	ascend		#  i: Sort in ascending order
bool	casesens	#  i: Sort is case sensitive
pointer	tp		#  i: Table descriptor
pointer	cp		#  i: Column descriptor
int    	nindex		# io: Number of rows
int	index[ARB]	# io: Array of row pointers in sorted order
#--
int	dtype, spptype, lendata
int	nary, iary, nelem
pointer	idxptr, nulptr, aryptr, curptr

int	movenulls()

begin
	# Allocate storage for index array

	call malloc (idxptr, nindex, TY_INT)

	# Initialize the array of row indices

	call amovi (index, Memi[idxptr], nindex)

	# Read in the column of table values. Setting dtype to
	# zero gets the actual data type of the column

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

	# Perform an indirect sort on the row indices using quicksort

	call tbl_qsort (ascend, dtype, aryptr, nelem, idxptr)

	# Move the row indices into the output array

	call amovi (Memi[idxptr], index, nindex)

	call mfree (idxptr, TY_INT)
	call mfree (nulptr, TY_BOOL)
	call mfree (aryptr, spptype)

end

# TBL_QSORT -- Indirect quick sort of a table column using an index array

procedure tbl_qsort (ascend, dtype, aryptr, nelem, idxptr)

bool	ascend		# i: Sort array in ascending order
int	dtype		# i: Data type of array to be sorted
pointer	aryptr		# i: Pointer to array to be sorted
int	nelem		# i: Number of elements to be sorted
pointer	idxptr		# o: Pointer to array of indices

include	"compare.com"

int	spptype

extern	compascb, compascd, compasci, compascr, compasct
extern	compdscb, compdscd, compdsci, compdscr, compdsct

begin
	dataptr = aryptr

	# Convert the type to the SPP format

	if (dtype < 0) {
	    lendata = - dtype
	    spptype = TY_CHAR
	} else {
	    lendata = 1
	    spptype = dtype
	}

	# Call the quick sort procedure with the proper comparison routine

	switch (spptype) { 
	case TY_BOOL:
	    if (ascend)
		call qsort (Memi[idxptr], nelem, compascb)
	    else
		call qsort (Memi[idxptr], nelem, compdscb)
	case TY_CHAR:
	    if (ascend)
		call qsort (Memi[idxptr], nelem, compasct)
	    else
		call qsort (Memi[idxptr], nelem, compdsct)
	case TY_SHORT,TY_INT,TY_LONG:
	    if (ascend)
		call qsort (Memi[idxptr], nelem, compasci)
	    else
		call qsort (Memi[idxptr], nelem, compdsci)
	case TY_REAL:
	    if (ascend)
		call qsort (Memi[idxptr], nelem, compascr)
	    else
		call qsort (Memi[idxptr], nelem, compdscr)
	case TY_DOUBLE: 
	    if (ascend)
		call qsort (Memi[idxptr], nelem, compascd)
	    else
		call qsort (Memi[idxptr], nelem, compdscd)
	}
end
