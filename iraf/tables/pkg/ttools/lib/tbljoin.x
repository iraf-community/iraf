include	<tbset.h>
define	MAXPRI		7

# TBL_JOIN -- Relational join of two tables
#
# This procedure peforms a relational join by sorting the two tables on
# the column to be joined and then merging the tables on the basis of the
# common column. An input tolerance is used to control the test for equality
# in the merge. The variables which describe the two tables are the table
# descriptors (tp1 & tp2), column descriptors (cp1 & cp2), row index arrays
# (index1 & index2), and index array lengths (nindex1 & nindex2). The merged
# output table is described by two index arrays which contain the row indices
# from the respective input tables (index3 & index4) and the index array
# lengths (nindex3 & nindex4). On input these lengths are the declared length
# of the output index arrays, on output, they are the number of rows in
# the merged output table. The total number of merged rows is output as
# njoin. The output index arrays may not be large enough to hold the merged
# table indices. In this case, the output index arrays will be filled as much
# as possible. So if njoin is greater than nindex3 or nindex4, an error has
# occured, but this error can be recovered from by reallocating the output
# index arrays so that the can hold njoin elements and calling this procedure
# again.
#
# B.Simon	03-Nov-87	First Code
# B.Simon	16-Dec-87	Changed to handle table subsets
# B.Simon	06-Feb-90	Changed to use tbtsrt

procedure tbl_join (tol, casesens, tp1, tp2, cp1, cp2,  nindex1, nindex2,
		    index1, index2, nindex3, nindex4, index3, index4, njoin)

double	tol		#  i: Tolerance used in testing for equality
bool	casesens	#  i: Join is case sensitive
pointer	tp1		#  i: Table descriptor of first table
pointer	tp2		#  i: Table descriptor of second table
pointer	cp1		#  i: Column descriptor of merged column in first table
pointer	cp2		#  i: Column descriptor of merged column in second table
int	nindex1		#  i: Number of indices in first input array
int	nindex2		#  i: Number of indices in second input array
int	index1		#  i: Array of row indices for first input table
int	index2		#  i: Array of row indices for second input table
int	nindex3		# io: Number of indices in first output array
int	nindex4		# io: Number of indices in second output array
int	index3		#  o: Array of row indices for first output table
int	index4		#  o: Array of row indices for second output table
int	njoin		#  o: Number of joined rows
#--
bool	fold
int	dtype[2], spptype[2], lendata[2], colpri[2], nary[2], nidx[2]
int	itab, iary, nmax

pointer	nulptr, temptr, curptr
pointer	tp[2], cp[2], idxptr[2], aryptr[2]

int	priority[MAXPRI]
data	priority	/ TY_DOUBLE, TY_REAL, TY_LONG, TY_INT, TY_SHORT,
			  TY_CHAR,   TY_BOOL  /
double	mjd()
int	tbcigi()

begin
	# Move input variables into arrays

	fold = ! casesens

	tp[1] = tp1
	tp[2] = tp2

	cp[1] = cp1
	cp[2] = cp2

	nmax = min (nindex3, nindex4)

	nidx[1] = nindex1
	nidx[2] = nindex2

	call malloc (idxptr[1], nindex1, TY_INT)
	call amovi (index1, Memi[idxptr[1]], nindex1)

	call malloc (idxptr[2], nindex2, TY_INT)
	call amovi (index2, Memi[idxptr[2]], nindex2)

	# Determine the data type of the merged column

	do itab = 1, 2 {

	    dtype[itab] = tbcigi (cp[itab], TBL_COL_DATATYPE)

	    if (dtype[itab] < 0) {
		lendata[itab] = - dtype[itab]
		spptype[itab] = TY_CHAR
	    } else {
		lendata[itab] = 1
		spptype[itab] = dtype[itab]
	    }

	    for (colpri[itab] = 1;
		 spptype[itab] != priority[colpri[itab]];
		 colpri[itab] = colpri[itab] + 1
		) ;

	}

	if (colpri[1] < colpri[2]) {
	    spptype[2] = spptype[1]
	    lendata[2] = lendata[1]
	} else if (colpri[2] < colpri[1]) {
	    spptype[1] = spptype[2]
	    lendata[1] = lendata[2]
	}

	# Read common columns into arrays and sort

	do itab = 1, 2 {

	    # Sort the index array on the common column

	    call tbtsrt (tp[itab], 1, cp[itab], fold, 
			 nidx[itab], Memi[idxptr[itab]])

	    # Read in the common column

	    if (spptype[itab] == TY_CHAR)
		dtype[itab] = - lendata[itab]
	    else
		dtype[itab] = spptype[itab]

	    call gettabcol (tp[itab], cp[itab], dtype[itab],
			    nary[itab], aryptr[itab], nulptr)

	    # If the tolerance of a string column is non-zero, 
	    # interpret the column as a date

	    if (dtype[itab] < 0 && tol > 0.0) {

		call malloc (temptr, nary[itab], TY_DOUBLE)
		curptr = aryptr[itab]
		do iary = 1, nary[itab] {
		    if (Memb[nulptr+iary-1])
			Memd[temptr+iary-1] = INDEFD
		    else
			Memd[temptr+iary-1] = mjd (Memc[curptr])
		    curptr = curptr + lendata[itab] + 1
		}
		call mfree (aryptr[itab], TY_CHAR)
		dtype[itab] = TY_DOUBLE
		spptype[itab] = TY_DOUBLE
		lendata[itab] = 1
		aryptr[itab] = temptr
	    }
	}

	# Merge the two tables

	call tbl_merge (tol, dtype, nary, aryptr, nidx, idxptr,
			nmax, njoin, index3, index4)

	nindex3 = min (nmax, njoin)
	nindex4 = min (nmax, njoin)

	# Free dynamic memory

	call mfree (nulptr, TY_BOOL)
	do itab = 1, 2 {
	    call mfree (idxptr[itab], TY_INT)
	    call mfree (aryptr[itab], spptype[itab])
	}

end
