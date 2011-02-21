include <tbset.h>

#  TXTCPY --  Copy data from single row in 3D table to columns
#             in the output 2D table.
#
#
#  This code is adapted from B.Simon's 04-Nov-94 version of tcopy.
#
#
#
#  Revision history:
#  ----------------
#
#  22-Nov-96  -  Task created (I.Busko)


procedure txtcpy (itp, otp, irow, icp, ocp, ncols, compact)

pointer itp		# i: pointer to descriptor of input table
pointer otp		# i: pointer to descriptor of output table
int	irow		# i: row in input table
pointer icp[ncols]	# i: array of pointers for input columns
pointer ocp[ncols]	# i: array of pointers for output columns
int	ncols		# i: number of columns in input table
bool	compact		# i: write scalars as header keywords ?
#--
int	icol, dlen, dtype, maxlen, maxch, nbuf
pointer	sp, buf, errmsg, colname

string	badtype  "Unsupported column data type (%s)"

int	tcs_intinfo(), tcs_totsize()

begin
	# Number of rows in output table must match the
	# largest array size in input table.
	maxlen = 0
	do icol = 1, ncols {
	    dlen = tcs_totsize (icp[icol])
	    if (dlen > maxlen)
	        maxlen = dlen
	}

	# Main loop: process each column.
	do icol = 1, ncols {

	    # Determine datatype of table column
	    # and allocate a buffer to match.
	    dtype = tcs_intinfo (icp[icol], TBL_COL_DATATYPE)
	    maxch = 1
	    if (dtype < 0) {
		maxch = - dtype
		dtype = TY_CHAR
	    }
	    call smark (sp)
	    call salloc (buf, maxlen*(maxch + 1), dtype)

	    # Read data from input table and
	    # write it to output table.
	    switch (dtype) {
	    case TY_BOOL:
		call tcs_rdaryb (itp, icp[icol], irow, maxlen, nbuf, Memb[buf])
		call txtcptb (otp, ocp[icol], Memb[buf], 1, nbuf, icol, compact)
	    case TY_CHAR:
		call tcs_rdaryt (itp, icp[icol], irow, maxch, maxlen, 
				 nbuf, Memc[buf])
		call txtcptt (otp, ocp[icol], Memc[buf], maxch, 1, nbuf, 
                              icol, compact)
	    case TY_SHORT:
		call tcs_rdarys (itp, icp[icol], irow, maxlen, nbuf, Mems[buf])
		call txtcpts (otp, ocp[icol], Mems[buf], 1, nbuf, icol, compact)
	    case TY_INT, TY_LONG:
		call tcs_rdaryi (itp, icp[icol], irow, maxlen, nbuf, Memi[buf])
		call txtcpti (otp, ocp[icol], Memi[buf], 1, nbuf, icol, compact)
	    case TY_REAL:
		call tcs_rdaryr (itp, icp[icol], irow, maxlen, nbuf, Memr[buf])
		call txtcptr (otp, ocp[icol], Memr[buf], 1, nbuf, icol, compact)
	    case TY_DOUBLE:
		call tcs_rdaryd (itp, icp[icol], irow, maxlen, nbuf, Memd[buf])
		call txtcptd (otp, ocp[icol], Memd[buf], 1, nbuf, icol, compact)
	    default:
		# Unsupported type, write error message
		call salloc (colname, SZ_COLNAME, TY_CHAR)
		call salloc (errmsg, SZ_LINE, TY_CHAR)
		call tcs_txtinfo (icp[icol], TBL_COL_NAME, 
				  Memc[colname], SZ_COLNAME)
		call sprintf (Memc[errmsg], SZ_LINE, badtype)
		call pargstr (Memc[colname])
		call error (1, Memc[errmsg])
	    }

	    call sfree (sp)
	}
end
