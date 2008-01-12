include <tbset.h>

# TBL_DIFF -- Find rows in the first table which are not in the second
#
# B.Simon	15-Jun-88	First Code
# B.Simon	05-Feb-90	Revised to use tbtsrt and tbrcmp

procedure tbl_diff (tp1, tp2, otp, nptr, col1, col2)

pointer	tp1		#  i: Table descriptor of first input table
pointer	tp2		#  i: Table descriptor of second input table
pointer	otp		#  i: Output table descriptor
int	nptr		#  i: Number of column pointers
pointer	col1[ARB]	#  i: Array of column pointers for first table
pointer	col2[ARB]	#  i: Array of column pointers for second table
#--
bool	fold
int	irow1, irow2, nrow1, nrow2, order, orow, iptr
pointer	sp, colname, row1, row2, ocol

int	tbrcmp()

begin
	# Allocate dynamic memory for column name

	call smark (sp)
	call salloc (colname, SZ_COLNAME, TY_CHAR)

	# Sort the array on the selected columns.

	fold = false
	call allrows (tp1, nrow1, row1)
	call allrows (tp2, nrow2, row2)

	call tbtsrt (tp1, nptr, col1, fold, nrow1, Memi[row1])
	call tbtsrt (tp2, nptr, col2, fold, nrow2, Memi[row2])

	# Get the column pointers to compare on in the output tables

	call malloc (ocol, nptr, TY_INT)
	do iptr = 1, nptr {
	    call tbcigt (col1[iptr], TBL_COL_NAME, Memc[colname], SZ_COLNAME)
	    call tbcfnd (otp, Memc[colname], Memi[ocol+iptr-1], 1)
	}

	# Search for rows in first table which are not in the second

	orow = 1
	irow1 = 0
	irow2 = 0

	while (irow1 < nrow1 && irow2 < nrow2) {

	    # Copy rows from both tables into output table

	    call tbrcpy (tp1, otp, Memi[row1+irow1], orow)
	    call tbrcsc (tp2, otp, col2, Memi[ocol], Memi[row2+irow2], 
			 orow+1, nptr)

	    # Update row indices and add row1 to output table
	    # if it is not in the second table

	    order = tbrcmp (otp, nptr, Memi[ocol], fold, orow, orow+1)

	    switch (order) {
	    case -1:
		irow1 = irow1 + 1
		orow = orow + 1
	    case 0:
		irow1 = irow1 + 1
	    case 1:
		irow2 = irow2 + 1
	    }
	}

	# Delete extra rows from output table

	if (order < 0)
	    call tbrdel (otp, orow, orow)
	else
	    call tbrdel (otp, orow, orow+1)

	# Add remaining rows of first table to output table

	while (irow1 < nrow1) {
	    call tbrcpy (tp1, otp, Memi[row1+irow1], orow)
	    irow1 = irow1 + 1
	    orow = orow + 1
	}


	# Free dynamic memory

	call mfree (row1, TY_INT)
	call mfree (row2, TY_INT)
	call mfree (ocol, TY_INT)
	call sfree (sp)

end
