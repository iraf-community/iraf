include <tbset.h>

#  TXICPY --  Copy data from single row and column in 3D table to 
#             1-D image.
#
#
#
#
#  Revision history:
#  ----------------
#
#  26-Nov-96  -  Task created (I.Busko)

procedure txicpy (itp, im, irow, icp, datatype, size)

pointer itp		# i: pointer to descriptor of input table
pointer im		# i: pointer to output image
int	irow		# i: row in input table
pointer icp		# i: array of pointers for input columns
int	datatype	# i: data type
int	size		# i: array size
#--
int	nbuf
pointer	sp, bufin, bufout, errmsg, colname

string	badtype  "Unsupported column data type (%s)"

pointer	impl1s(), impl1i(), impl1r(), impl1d()
begin
	call smark (sp)
	call salloc (bufin, size, datatype)

	switch (datatype) {
	case TY_SHORT:
	    call tcs_rdarys (itp, icp, irow, size, nbuf, Mems[bufin])
	    bufout = impl1s (im)
	    call amovs (Mems[bufin], Mems[bufout], size)
	case TY_INT,TY_LONG:
	    call tcs_rdaryi (itp, icp, irow, size, nbuf, Memi[bufin])
	    bufout = impl1i (im)
	    call amovi (Memi[bufin], Memi[bufout], size)
	case TY_REAL:
	    call tcs_rdaryr (itp, icp, irow, size, nbuf, Memr[bufin])
	    bufout = impl1r (im)
	    call amovr (Memr[bufin], Memr[bufout], size)
	case TY_DOUBLE:
	    call tcs_rdaryd (itp, icp, irow, size, nbuf, Memd[bufin])
	    bufout = impl1d (im)
	    call amovd (Memd[bufin], Memd[bufout], size)
	default:
	    # Unsupported type, write error message
	    call salloc (colname, SZ_COLNAME, TY_CHAR)
	    call salloc (errmsg, SZ_LINE, TY_CHAR)
	    call tcs_txtinfo (icp, TBL_COL_NAME, Memc[colname], SZ_COLNAME)
	    call sprintf (Memc[errmsg], SZ_LINE, badtype)
	    call pargstr (Memc[colname])
	    call error (1, Memc[errmsg])
	}

	call sfree (sp)
end
