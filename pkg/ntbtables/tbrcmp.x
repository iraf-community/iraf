include "tbtables.h"	# for TBL_IS_INDEFD
include	<tbset.h>

# TBRCMP -- Comparison function used to sort table rows
#
# This procedure returns an integer indicating the order of two rows.
# The value of the integer is set according to the following scheme: 
#
#		if row1  < row2, order = -1
#		if row1 == row2, order =  0
#		if row1  > row2, order =  1
#
# The comparison is done on the columns whose pointers are passed in the
# colptr array. The first column in the array is the most significant. 
# Subsequent columns are used to break ties in the order of the previous
# columns. Nulls are considered to be larger than any other value. Case is
# ignored in the sort order if fold is set to true.
#
# A column that contains arrays is treated much like an array of columns.
# The arrays of values for the two rows are gotten, and they are compared
# element by element.  The first element that does not match determines
# the order.  One aspect of this that may not be intuitive is that if one
# array is padded with nulls to make it effectively shorter than the other,
# and the non-null elements all match, the shorter array will be considered
# to be larger, since nulls are larger than any other value.
#
# B.Simon	22-Jan-1990	First Code
# Phil Hodge	22-Jan-1993	Use IS_INDEF instead of == INDEF.
# Phil Hodge	 1-Apr-1993	Include short datatype.
# Phil Hodge	 2-Jun-1997	Replace IS_INDEFD with TBL_IS_INDEFD.
# Phil Hodge	18-Jan-1999	Add a section for comparing arrays;
#				get boolean as short and check for indef.

int procedure tbrcmp (tp, numcols, colptr, fold, row1, row2)

pointer	tp		# i: Table descriptor
int	numcols		# i: Number of columns to sort on
pointer	colptr[ARB]	# i: Array of column descriptors
bool	fold		# i: Fold upper and lower case when sorting
int	row1		# i: Index to first row to compare
int	row2		# i: Index to second row to compare
#--
int	datatype	# data type of column
int	nelem		# number of elements in array (or one if scalar)
double	dval1, dval2
int	ival1, ival2
short	sval1, sval2
pointer	str1, str2
real	rval1, rval2

pointer sp2		# stack pointer for ptr1 & ptr2
pointer ptr1, ptr2	# pointers to arrays of values
int	i		# loop index

int	icol, order
pointer	sp, cp

int	tbcigi(), strcmp()

begin
	# Allocate dynamic memory for strings

	call smark (sp)
	call salloc (str1, SZ_LINE, TY_CHAR)
	call salloc (str2, SZ_LINE, TY_CHAR)

	# Loop over each column until the two rows don't match

	order = 0
	for (icol = 1; icol <= numcols && order == 0; icol = icol + 1) {
	    cp = colptr[icol]

	    datatype = tbcigi (cp, TBL_COL_DATATYPE)
	    nelem = tbcigi (cp, TBL_COL_LENDATA)

	    if (nelem == 1) {

		switch (datatype) {

		case TY_INT,TY_LONG:
		    call tbegti (tp, cp, row1, ival1)
		    call tbegti (tp, cp, row2, ival2)

		    if (ival1 == ival2)
			order = 0
		    else if (IS_INDEFI (ival1))
			order = 1
		    else if (IS_INDEFI (ival2))
			order = -1
		    else if (ival1 > ival2)
			order = 1
		    else if (ival1 < ival2)
			order = -1

		case TY_SHORT,TY_BOOL:
		    # note:  boolean true --> YES (1), false --> NO (0)
		    call tbegts (tp, cp, row1, sval1)
		    call tbegts (tp, cp, row2, sval2)

		    if (sval1 == sval2)
			order = 0
		    else if (IS_INDEFS (sval1))
			order = 1
		    else if (IS_INDEFS (sval2))
			order = -1
		    else if (sval1 > sval2)
			order = 1
		    else if (sval1 < sval2)
			order = -1

		case TY_REAL:
		    call tbegtr (tp, cp, row1, rval1)
		    call tbegtr (tp, cp, row2, rval2)

		    if (rval1 == rval2)
			order = 0
		    else if (IS_INDEFR (rval1))
			order = 1
		    else if (IS_INDEFR (rval2))
			order = -1
		    else if (rval1 > rval2)
			order = 1
		    else if (rval1 < rval2)
			order = -1

		case TY_DOUBLE:
		    call tbegtd (tp, cp, row1, dval1)
		    call tbegtd (tp, cp, row2, dval2)

		    if (dval1 == dval2)
			order = 0
		    else if (TBL_IS_INDEFD (dval1))
			order = 1
		    else if (TBL_IS_INDEFD (dval2))
			order = -1
		    else if (dval1 > dval2)
			order = 1
		    else if (dval1 < dval2)
			order = -1

		default:
		    call tbegtt (tp, cp, row1, Memc[str1], SZ_LINE)
		    call tbegtt (tp, cp, row2, Memc[str2], SZ_LINE)

		    if (fold) {
			call strlwr (Memc[str1])
			call strlwr (Memc[str2])
		    }

		    if (Memc[str1] == Memc[str2]) {
		       order = strcmp (Memc[str1], Memc[str2])
		       if (order != 0)
			    order = sign (1, order)
		    } else if (Memc[str1] == EOS) {
		       order = 1
		    } else if (Memc[str2] == EOS) {
		       order = -1
		    } else {
		       order = sign (1, int (Memc[str1] - Memc[str2]))
		    }
		}

	    } else {		# the current column contains arrays

		call smark (sp2)	# just for ptr1 & ptr2

		switch (datatype) {

		case TY_INT,TY_LONG:
		    call salloc (ptr1, nelem, TY_INT)
		    call salloc (ptr2, nelem, TY_INT)

		    call tbagti (tp, cp, row1, Memi[ptr1], 1, nelem)
		    call tbagti (tp, cp, row2, Memi[ptr2], 1, nelem)

		    do i = 0, nelem-1 {
			if (Memi[ptr1+i] == Memi[ptr2+i])
			    order = 0
			else if (IS_INDEFI (Memi[ptr1+i]))
			    order = 1
			else if (IS_INDEFI (Memi[ptr2+i]))
			    order = -1
			else if (Memi[ptr1+i] > Memi[ptr2+i])
			    order = 1
			else if (Memi[ptr1+i] < Memi[ptr2+i])
			    order = -1
			if (order != 0)
			    break
		    }

		case TY_SHORT,TY_BOOL:
		    call salloc (ptr1, nelem, TY_SHORT)
		    call salloc (ptr2, nelem, TY_SHORT)

		    call tbagts (tp, cp, row1, Mems[ptr1], 1, nelem)
		    call tbagts (tp, cp, row2, Mems[ptr2], 1, nelem)

		    do i = 0, nelem-1 {
			if (Mems[ptr1+i] == Mems[ptr2+i])
			    order = 0
			else if (IS_INDEFS (Mems[ptr1+i]))
			    order = 1
			else if (IS_INDEFS (Mems[ptr2+i]))
			    order = -1
			else if (Mems[ptr1+i] > Mems[ptr2+i])
			    order = 1
			else if (Mems[ptr1+i] < Mems[ptr2+i])
			    order = -1
			if (order != 0)
			    break
		    }

		case TY_REAL:
		    call salloc (ptr1, nelem, TY_REAL)
		    call salloc (ptr2, nelem, TY_REAL)

		    call tbagtr (tp, cp, row1, Memr[ptr1], 1, nelem)
		    call tbagtr (tp, cp, row2, Memr[ptr2], 1, nelem)

		    do i = 0, nelem-1 {
			if (Memr[ptr1+i] == Memr[ptr2+i])
			    order = 0
			else if (IS_INDEFR (Memr[ptr1+i]))
			    order = 1
			else if (IS_INDEFR (Memr[ptr2+i]))
			    order = -1
			else if (Memr[ptr1+i] > Memr[ptr2+i])
			    order = 1
			else if (Memr[ptr1+i] < Memr[ptr2+i])
			    order = -1
			if (order != 0)
			    break
		    }

		case TY_DOUBLE:
		    call salloc (ptr1, nelem, TY_DOUBLE)
		    call salloc (ptr2, nelem, TY_DOUBLE)

		    call tbagtd (tp, cp, row1, Memd[ptr1], 1, nelem)
		    call tbagtd (tp, cp, row2, Memd[ptr2], 1, nelem)

		    do i = 0, nelem-1 {
			if (Memd[ptr1+i] == Memd[ptr2+i])
			    order = 0
			else if (TBL_IS_INDEFD (Memd[ptr1+i]))
			    order = 1
			else if (TBL_IS_INDEFD (Memd[ptr2+i]))
			    order = -1
			else if (Memd[ptr1+i] > Memd[ptr2+i])
			    order = 1
			else if (Memd[ptr1+i] < Memd[ptr2+i])
			    order = -1
			if (order != 0)
			    break
		    }

		default:
		    do i = 1, nelem {
			call tbagtt (tp, cp, row1, Memc[str1], SZ_LINE, i, i)
			call tbagtt (tp, cp, row2, Memc[str2], SZ_LINE, i, i)

			if (fold) {
			    call strlwr (Memc[str1])
			    call strlwr (Memc[str2])
			}

			if (Memc[str1] == Memc[str2]) {
			   order = strcmp (Memc[str1], Memc[str2])
			   if (order != 0)
				order = sign (1, order)
			} else if (Memc[str1] == EOS) {
			   order = 1
			} else if (Memc[str2] == EOS) {
			   order = -1
			} else {
			   order = sign (1, int (Memc[str1] - Memc[str2]))
			}
			if (order != 0)
			    break
		    }
		}
		call sfree (sp2)
	    }
	}

	call sfree (sp)
	return (order)
end
