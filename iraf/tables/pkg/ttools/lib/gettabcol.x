include	<tbset.h>

# GETTABCOL -- Read in a table column of any data type
#
# This procedure produces an array of table column values and an array of
# null flags given an input table descriptor, column descriptor, and data
# type. If the data type is set to zero, the column data type is queried
# and returned to the calling program. The arrays are put in dynamic memory
# and pointers to these arrays are returned to the calling program, which must
# free the arrays when it is done with them.
#
# B.Simon	15-Dec-87	First Code

procedure gettabcol (tp, cp, dtype, nary, aryptr, nulptr)

pointer	tp		#  i: Table descriptor
pointer	cp		#  i: Column descriptor
int	dtype		# io: Data type of column (strings are -length)
int	nary		#  o: Length of output arrays
pointer	aryptr		#  o: Pointer to array of values
pointer	nulptr		#  o: Pointer to array of null flags
#--
int	lendata, spptype
int	tbpsta(), tbcigi()

errchk	malloc, tbpsta

begin
	# Allocate storage for null flags

	nary = tbpsta (tp, TBL_NROWS)
	call malloc (nulptr, nary, TY_BOOL)
	if (dtype == 0)
	    dtype = tbcigi (cp, TBL_COL_DATATYPE)

	# Break down data type into spp type and length

	if (dtype < 0) {
	    lendata = - dtype
	    spptype = TY_CHAR
	} else {
	    lendata = 1
	    spptype = dtype
	}

	# Read in the column of table values

	switch (spptype) {
	case TY_BOOL:
	    call malloc (aryptr, nary, TY_BOOL)
	    call tbcgtb (tp, cp, Memb[aryptr], Memb[nulptr], 1, nary)
	case TY_CHAR:
	    call malloc (aryptr, nary*(lendata+1), TY_CHAR)
	    call tbcgtt (tp, cp, Memc[aryptr], Memb[nulptr], lendata,
			 1, nary)
	case TY_SHORT,TY_INT,TY_LONG:
	    call malloc (aryptr, nary, TY_INT)
	    call tbcgti (tp, cp, Memi[aryptr], Memb[nulptr], 1, nary)
	case TY_REAL:
	    call malloc (aryptr, nary, TY_REAL)
	    call tbcgtr (tp, cp, Memr[aryptr], Memb[nulptr], 1, nary)
	case TY_DOUBLE: 
	    call malloc (aryptr, nary, TY_DOUBLE)
	    call tbcgtd (tp, cp, Memd[aryptr], Memb[nulptr], 1, nary)
	}

end
