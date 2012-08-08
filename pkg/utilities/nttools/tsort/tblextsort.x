include	<tbset.h>

define	MERGEORDER	15

# TBL_EXTSORT -- External table sort based on Software Tools
#
# B.Simon	21-Feb-91	First Code
# B.Simon	15-Mar-00	Modified to speed up code
# Phil Hodge	10-Sep-04	Add new procedure tut_replace, and call it
#				instead of tbtdel/tbtren; use tbrcsc instead
#				of tbrcpy in tut_cpytab.
# Phil Hodge	15-Sep-04	Make 'heap' an array of one, because it is
#				declared to be an array in tut_deltab.
#				Copy the original table one set of rows at a
#				time to an "srt_cpy" temp table, and generate
#				the sorted index array from that; this is to
#				work around a file I/O buffer problem when
#				sorting an STSDAS-format table.
# Phil Hodge	 6-Oct-05	Find column pointers in heap table, instead
#				of using colptr.
#				Remove tp from calling sequence of tut_opntab.

procedure tbl_extsort (tp, numptr, colptr, maxrow, casesens, ascend)

pointer	tp		# u: Table descriptor
int	numptr		# i: Number of columns to sort on (size of colptr)
pointer	colptr[ARB]	# i: Array of column descriptors
int	maxrow		# i: Maximum number of rows to sort at one time
bool	casesens	# i: Sort is case sensitive
bool	ascend		# i: Sort in ascending order
#--
bool	fold
int	irow, mrow, nrow, ntab
int	low, high, limit
pointer	sp, index, tmproot, tmpname, tabname
pointer	otp, heap[1], itp[MERGEORDER]
# These are for a copy of a portion of the original table.
pointer cpyname, tp_cpy[1]
pointer cp_cpy		# column descriptors in srt_cpy temporary table
pointer cp_heap		# column descriptors in heap temporary table

pointer	tbtopn(), tut_maktab()
int	tbpsta()

begin
	# Allocate dynamic memory for temporary strings

	call smark (sp)
	call salloc (tmproot, SZ_FNAME, TY_CHAR)
	call salloc (tmpname, SZ_FNAME, TY_CHAR)
	call salloc (tabname, SZ_FNAME, TY_CHAR)
	call salloc (cpyname, SZ_FNAME, TY_CHAR)	# the srt_cpy table

	call salloc (cp_cpy, numptr, TY_POINTER)
	call salloc (cp_heap, numptr, TY_POINTER)

	# Initialize variables for external sort

	high = 0
	fold = ! casesens
	call malloc (index, maxrow, TY_INT)

	# This root name is used only for the srt_cpy temporary table.
	# Create the srt_cpy temporary table, and find the pointers to
	# column descriptors corresponding to colptr.
	call mktemp ("tmp$srt_cpy", Memc[tmproot], SZ_FNAME)
	tp_cpy[1] = tut_maktab (Memc[tmproot], 0, tp)
	call tut_fndcol (colptr, tp_cpy, Memi[cp_cpy], numptr)

	# This rootname is used for all the other temporary tables.
	call mktemp ("tmp$srt", Memc[tmproot], SZ_FNAME)

	# Extract one buffer's worth of rows from original table to the
	# srt_cpy temporary table, generate a sorted index array, and
	# copy/sort to another temporary table.

	nrow = tbpsta (tp, TBL_NROWS)
	for (irow = 1; irow <= nrow; irow = irow + maxrow) {

	    mrow = min (maxrow, nrow-irow+1)

	    # Extract rows to the srt_cpy table, without changing the order.
	    call tut_setidx (irow, mrow, Memi[index])
	    call tut_cpytab (tp, tp_cpy[1], true, mrow, Memi[index])
	    call tbtflu (tp_cpy[1])	# flush so we can read the table

	    # Generate the index array for the srt_cpy table.
	    call tut_setidx (1, mrow, Memi[index])
	    call tbtsrt (tp_cpy[1], numptr, Memi[cp_cpy],
			fold, mrow, Memi[index])

	    # Copy (in sorted order) the srt_cpy table to a temporary table.
	    high = high + 1
	    otp = tut_maktab (Memc[tmproot], high, tp)
	    call tut_cpytab (tp_cpy[1], otp, ascend, mrow, Memi[index])
	    call tbtclo (otp)
	}
	call tut_deltab (tp_cpy, 1)	# done with the srt_cpy table

	# Merge the temporary tables into a single sorted table

	heap[1] = tut_maktab (Memc[tmproot], 0, tp)
	call tut_fndcol (colptr, heap[1], Memi[cp_heap], numptr)

	for (low = 1; low < high; low = low + MERGEORDER) {
	    limit = min (low+MERGEORDER-1, high)
	    ntab = limit - low + 1

	    call tut_opntab (Memc[tmproot], low, limit, itp)
	    high = high + 1

	    otp = tut_maktab (Memc[tmproot], high, tp)
	    call tut_mrgtab (itp, ntab, numptr,
			fold, ascend, heap[1], Memi[cp_heap], otp)

	    call tut_deltab (itp, ntab)
	    # We really only need the name of the last table created.
	    call tbtnam (otp, Memc[tmpname], SZ_FNAME)
	    call tbtclo (otp)
	}
	call tut_deltab (heap, 1)

	call tbtnam (tp, Memc[tabname], SZ_FNAME)

	# Copy sorted temporary table to original table, and delete
	# temporary table
	otp = tbtopn (Memc[tmpname], READ_ONLY, NULL)
	call tut_replace (otp, tp)
	call tbtclo (tp)
	call tbtclo (otp)
	call tbtdel (Memc[tmpname])

	# Free dynamic memory

	call mfree (index, TY_INT)
	call sfree (sp)
end


#-----------------------------------------------------------------------#
#									#
#	These procedures are utility routines used by tbl_extsort()	#
#	They should not be called by other routines			#
#									#
#-----------------------------------------------------------------------#

# TUT_FNDCOL -- Get pointers to column descriptors

# Get the pointers to column descriptors for the srt_cpy temporary table.
# Use the column name rather than number for the original table to allow
# for the possibility that the column numbers are not the same in both
# tables, e.g. if a column selector was used with the original table name.

procedure tut_fndcol (colptr, tp_cpy, cp_cpy, numptr)

pointer	colptr[ARB]	# i: Array of column descriptors in original table
pointer	tp_cpy		# i: Descriptor for the srt_cpy temporary table
pointer	cp_cpy[ARB]	# o: Array of column descriptors in tp_cpy table
int	numptr		# i: Number of columns in colptr and cp_cpy arrays
#--
char	colname[SZ_COLNAME]
int	colnum

begin
	do colnum = 1, numptr {
	    call tbcigt (colptr[colnum], TBL_COL_NAME, colname, SZ_COLNAME)
	    call tbcfnd1 (tp_cpy, colname, cp_cpy[colnum])
	}
end

# TUT_CPYTAB -- Copy sorted rows to temporary table

procedure tut_cpytab (tp, otp, ascend, nrow, index)

pointer	tp		# i: Input table descriptor
pointer	otp		# i: Output table descriptor
bool	ascend		# i: Write output in ascending sort order?
int	nrow		# i: Number of rows to write
int	index[ARB]	# i: Array of row indices
#--
char	colname[SZ_COLNAME]
pointer cp, ocp
int	ncols, colnum
int	irow
pointer tbcnum()
int	tbpsta()

begin
	# Number of columns in temporary table
	ncols = tbpsta (otp, TBL_NCOLS)
	call malloc (cp, ncols, TY_POINTER)
	call malloc (ocp, ncols, TY_POINTER)

	# Get pointers to column descriptors
	do colnum = 1, ncols {
	    Memi[ocp+colnum-1] = tbcnum (otp, colnum)
	    call tbcigt (Memi[ocp+colnum-1], TBL_COL_NAME, colname, SZ_COLNAME)
	    call tbcfnd1 (tp, colname, Memi[cp+colnum-1])
	}

	if (ascend) {
	    do irow = 1, nrow {
		call tbrcsc (tp, otp, Memi[cp], Memi[ocp],
				index[irow], irow, ncols)
	    }
	} else {
	    do irow = nrow, 1, -1 {
		call tbrcsc (tp, otp, Memi[cp], Memi[ocp],
				index[irow], nrow-irow+1, ncols)
	    }
	}

	call mfree (cp, TY_POINTER)
	call mfree (ocp, TY_POINTER)
end

# TUT_REPLACE -- Copy sorted rows to temporary table
# This copies each row to the original table, without first deleting
# the original table.

procedure tut_replace (otp, tp_orig)

pointer	otp		# i: Descriptor for sorted temporary table
pointer	tp_orig		# i: Descriptor for original table
#--
pointer sp
pointer ocp, cp_orig
char	colname[SZ_COLNAME]
int	nrows, ncols
int	row, colnum
pointer tbcnum()
int	tbpsta()

begin
	call smark (sp)

	ncols = tbpsta (otp, TBL_NCOLS)
	call salloc (ocp, ncols, TY_POINTER)
	call salloc (cp_orig, ncols, TY_POINTER)

	# Get the pointers to column descriptors for the sorted and original
	# tables.
	do colnum = 1, ncols {
	    Memi[ocp+colnum-1] = tbcnum (otp, colnum)
	    call tbcigt (Memi[ocp+colnum-1], TBL_COL_NAME, colname, SZ_COLNAME)
	    call tbcfnd1 (tp_orig, colname, Memi[cp_orig+colnum-1])
	}

	nrows = tbpsta (otp, TBL_NROWS)
	do row = 1, nrows {
	    call tbrcsc (otp, tp_orig, Memi[ocp], Memi[cp_orig],
			row, row, ncols)
	}

	call sfree (sp)
end

# TUT_DELTAB -- Delete a group of temporary tables

procedure tut_deltab (itp, ntab)

pointer	itp[ARB]	# i: Array of table descriptors
int	ntab		# i: Number of tables to delete
#--
int	itab
pointer	sp, tmpname

begin
	call smark (sp)
	call salloc (tmpname, SZ_FNAME, TY_CHAR)

	do itab = 1, ntab {
	    call tbtnam (itp[itab], Memc[tmpname], SZ_FNAME)
	    call tbtclo (itp[itab])
	    call tbtdel (Memc[tmpname])
	}

	call sfree (sp)
end

# TUT_MAKNAM -- Construct a table name by appending a suffix to the root

procedure tut_maknam (tmproot, index, tmpname, maxch)

char	tmproot[ARB]	# i: Table name root
int	index		# i: Number to use as suffix
char	tmpname[ARB]	# o: Full table name
int	maxch		# i: Declared length of table name
#--
int	ic

int	gstrcpy(), itoc()

begin
	ic = gstrcpy (tmproot, tmpname, maxch) + 1
	ic = itoc (index, tmpname[ic], maxch-ic) + ic
	ic = gstrcpy (".tab", tmpname[ic], maxch-ic) +ic
	tmpname[ic] = EOS
end

# TUT_MAKTAB -- Create a table for temporary storage

pointer	procedure tut_maktab (tmproot, index, tp)

char	tmproot[ARB]	# i: Table name root
int	index		# i: Number to use as suffix
pointer	tp		# i: Table to use as template
#--
pointer	sp, otp, tmpname

pointer	tbtopn()

begin
	call smark (sp)
	call salloc (tmpname, SZ_FNAME, TY_CHAR)

	call tut_maknam (tmproot, index, Memc[tmpname], SZ_FNAME)

	iferr {
	    otp = tbtopn (Memc[tmpname], NEW_COPY, tp)
	    call tbpset (otp, TBL_WHTYPE, TBL_TYPE_S_ROW)
	    call tbtcre (otp)
	    call tbpset (otp, TBL_ADVICE, SEQUENTIAL)
	} then {
	    call error (1, "Can't create temporary table used for sorting")
	}

	call sfree (sp)
	return (otp)
end

# TUT_MRGTAB -- Merge a set of tables into a single table

procedure tut_mrgtab (itp, ntab, numptr, fold, ascend, heap, cp, otp)

pointer	itp[ARB]	# i: Array of input table descriptors
int	ntab		# i: Number of input tables
int	numptr		# i: Number of columns to merge on
bool	fold		# i: Fold case when merging?
bool	ascend		# i: Merge in ascending order?
pointer	heap		# i: Heap array descriptor
pointer	cp[ARB]		# i: Array of column descriptors in heap table
pointer	otp		# i: Output table descriptor
#--
int	nindex, itab, idx, jdx, temp
int	irow[MERGEORDER], nrow[MERGEORDER], index[MERGEORDER]

int	tbpsta()

begin
	# Create heap by copying one row from each table

	nindex = 0
	do itab = 1, ntab {
	    irow[itab] = 1
	    nrow[itab] = tbpsta (itp[itab], TBL_NROWS)

	    if (nrow[itab] > 0) {
		call tbrcpy (itp[itab], heap, 1, itab)
		nindex = nindex + 1
		index[nindex] = itab
	    }
	}

	# Put the heap in sort order

	call tbtsrt (heap, numptr, cp, fold, nindex, index)
	if (!ascend) {
	    idx = 1
	    jdx = nindex
	    while (idx < jdx) {
		temp = index[idx]
		index[idx] = index[jdx]
		index[jdx] = temp
		idx = idx + 1
		jdx = jdx - 1
	    }
	}

	# Copy a row from the heap, replacing it by a new row

	for (idx = 1; nindex > 0; idx = idx + 1) {
	    itab = index[1]
	    call tbrcpy (heap, otp, itab, idx)

	    if (irow[itab] < nrow[itab]) {
		irow[itab] = irow[itab] + 1
		call tbrcpy (itp[itab], heap, irow[itab], itab)
	    } else {
		index[1] = index[nindex]
		nindex = nindex - 1
	    }

	    call tut_reheap (heap, numptr, cp, fold, ascend,
			     nindex, index)
	}
end

# TUT_OPNTAB -- Open a group of temporary tables

procedure tut_opntab (tmproot, low, limit, itp)

char	tmproot[ARB]	# i: Temporary file name root
int	low		# i: First table to open
int	limit		# i: Last table to open
pointer	itp[ARB]	# o: Array of table descriptors
#--
int	itab
pointer	sp, tmpname

int	tbtopn()

begin
	call smark (sp)
	call salloc (tmpname, SZ_FNAME, TY_CHAR)

	do itab = 1, limit-low+1 {
	    call tut_maknam (tmproot, low+itab-1, Memc[tmpname], SZ_FNAME)
	    iferr (itp[itab] = tbtopn (Memc[tmpname], READ_ONLY, NULL))
		call error (1, "Can't open temporary table used for sorting")
	}

	call sfree (sp)
end

# TUT_REHEAP -- Restore heap to sort order

procedure tut_reheap (heap, numptr, cp, fold, ascend, nindex, index)

pointer	heap		#  i: Heap table descriptor
int	numptr		#  i: Number of columns to sort on
pointer	cp[ARB]		#  i: Array of column descriptors in heap table
bool	fold		#  i: Fold case together?
bool	ascend		#  i: Put head in ascending order?
int	nindex		#  i: Number of elements in index array
int	index[ARB]	# io: Array of head table indices in sort order
#--
int	order, irow, jrow, temp

int	tbrcmp()

begin
	# Set the sort order

	if (ascend)
	    order = 1
	else
	    order = -1

	# A heap is organized as a binary tree. The heap is partially
	# sorted so that a parent is in sort order with respect to its
	# children. However, siblings are not necessarily in sort order.
	# The children of index[irow] are at index[2*irow] and
	# index[2*irow+1].

	for (irow = 1; 2*irow <= nindex; irow = jrow) {

	    # Find the smaller (larger) of the two children

	    jrow = 2 * irow
	    if (jrow < nindex) {
		if (tbrcmp (heap, numptr, cp, fold,
			    index[jrow], index[jrow+1]) == order)
		    jrow = jrow + 1
	    }

	    # If child is smaller (larger) than parent,
	    # exhange their indices

	    if (tbrcmp (heap, numptr, cp, fold,
			index[irow], index[jrow]) != order) {
		break

	    } else {
		temp = index[irow]
		index[irow] = index[jrow]
		index[jrow] = temp
	    }
	}

end

# TUT_SETIDX -- Set up the index array

procedure tut_setidx (irow, nrow, index)

int	irow		# i: starting value
int	nrow		# i: number of rows in index array
int	index[ARB]	# o: index array
#--
int	jrow

begin
	do jrow = 1, nrow
	    index[jrow] = irow + jrow - 1
end
