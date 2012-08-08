include <tbset.h>

# TCPYROW -- Copy a single row from the input to output table
 
procedure tcpyrow (itp, otp, icp, ocp, irow, orow, ncols)

pointer itp		# i: pointer to descriptor of input table
pointer otp		# i: pointer to descriptor of output table
pointer icp[ncols]	# i: array of pointers for input columns
pointer ocp[ncols]	# i: array of pointers for output columns
int	irow		# i: row number in input table
int	orow		# i: row number in output table
int	ncols		# i: number of columns to be copied
#--
int	icol, dlen, dtype, maxch, nbuf
pointer	sp, buf, errmsg, colname

string	badtype  "Unsupported column data type (%s)"

int	tcs_intinfo(), tcs_totsize()

begin
	do icol = 1, ncols {
	    # Determine the length and datatype of the table column
	    # and allocate a buffer to match

	    dlen = tcs_totsize (icp[icol])
	    dtype = tcs_intinfo (icp[icol], TBL_COL_DATATYPE)

	    maxch = 1
	    if (dtype < 0) {
		maxch = - dtype
		dtype = TY_CHAR
	    }

	    call smark (sp)
	    call salloc (buf, dlen*(maxch + 1), dtype)
	    
	    # Read the data from the input table and write it 
	    # to the output table

	    switch (dtype) {
	    case TY_BOOL:
		call tcs_rdaryb (itp, icp[icol], irow, dlen, nbuf, Memb[buf])
		call tbaptb (otp, ocp[icol], orow, Memb[buf], 1, nbuf)
	    case TY_CHAR:
		call tcs_rdaryt (itp, icp[icol], irow, maxch, dlen, 
				 nbuf, Memc[buf])
		call tbaptt (otp, ocp[icol], orow, Memc[buf], maxch, 1, nbuf)
	    case TY_SHORT:
		call tcs_rdarys (itp, icp[icol], irow, dlen, nbuf, Mems[buf])
		call tbapts (otp, ocp[icol], orow, Mems[buf], 1, nbuf)
	    case TY_INT, TY_LONG:
		call tcs_rdaryi (itp, icp[icol], irow, dlen, nbuf, Memi[buf])
		call tbapti (otp, ocp[icol], orow, Memi[buf], 1, nbuf)
	    case TY_REAL:
		call tcs_rdaryr (itp, icp[icol], irow, dlen, nbuf, Memr[buf])
		call tbaptr (otp, ocp[icol], orow, Memr[buf], 1, nbuf)
	    case TY_DOUBLE:
		call tcs_rdaryd (itp, icp[icol], irow, dlen, nbuf, Memd[buf])
		call tbaptd (otp, ocp[icol], orow, Memd[buf], 1, nbuf)
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
