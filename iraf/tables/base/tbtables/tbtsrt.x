# TBTSRT -- Sort a table on multiple columns
#
# This procedure rearranges an array of row indices into sorted order.
# The array of row indices must be created before calling this procedure.
# Null elements will be last in the sort order. Boolean false is less 
# than true in the sort order. If character strings are being sorted, 
# case can be ignored by setting fold to true. 
# 
# B.Simon	22-Jan-90	First Code

procedure tbtsrt (tp, numcols, colptr, fold, nindex, index)

pointer	tp		#  i: Table descriptor
int	numcols		#  i: Number of columns to sort on
pointer	colptr[ARB]	#  i: Array of column descriptors
bool	fold		#  i: Fold upper and lower case when sorting
long	nindex		#  i: Number of rows
long	index[ARB]	# io: Array of row indices in sorted order
#--
common	/savcmp/	sv_tp, sv_colptr, sv_numcols, sv_fold
bool	sv_fold
int	sv_numcols
pointer	sv_tp, sv_colptr
size_t	sz_val

int	icol

int	tbqcmp()
extern	tbqcmp

begin
	# Fill common block used to pass info to comparison routine

	sz_val = numcols
	call malloc (sv_colptr, sz_val, TY_POINTER)
	
	sv_tp = tp
	sv_fold = fold
	sv_numcols = numcols
	do icol = 1, numcols
	    Memp[sv_colptr+icol-1] = colptr[icol]

	# Call quicksort routine

	call qsortl (index, nindex, tbqcmp)

	# Free memory

	call mfree (sv_colptr, TY_POINTER)

end

# TBQCMP -- Interface to comparison routine

int procedure tbqcmp (row1, row2)

long	row1		# i: Index to first row to compare
long	row2		# i: Index to second row to compare
#--
common	/savcmp/	sv_tp, sv_colptr, sv_numcols, sv_fold
bool	sv_fold
int	sv_numcols
pointer	sv_tp, sv_colptr

int	order
int	tbrcmp()

begin
	order = tbrcmp (sv_tp, sv_numcols, Memp[sv_colptr], 
			sv_fold, row1, row2)
	return (order)
end
