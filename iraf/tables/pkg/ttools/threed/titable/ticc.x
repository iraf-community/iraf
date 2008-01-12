
#  TICC  --  Copy data from column in input to cell array in output.
#
#
#
#
#  Revision history:
#  ----------------
#  20-Jan-97  -  Task created (I.Busko)


procedure ticc (itp, icp, otp, ocp, dtype, maxlen, rowsel, row)

pointer	itp		# i: input table descriptor
pointer	icp		# i: input column descriptor
pointer	otp		# i: output table descriptor
pointer	ocp		# i: output column descriptor
int	dtype		# i: data type of both input and output columns
int	maxlen		# i: array length
char	rowsel[ARB]	# i: work string for row selector
int	row		# i: row where to insert
#--
pointer	sp, buf
int	maxch

begin
	# Alloc buffer of apropriate length and type. 
	maxch = 1
	if (dtype < 0) {
	    maxch = - dtype
	    dtype = TY_CHAR
	}
	call smark (sp)
	call salloc (buf, maxlen*(maxch + 1), dtype)

	# Copy.
	switch (dtype) {
	case TY_CHAR:
	    call tirowst (itp, icp, otp, ocp, rowsel, row, maxch, maxlen, 
                          Memc[buf])
	case TY_BOOL:
	    call tirowsb (itp, icp, otp, ocp, rowsel, row, maxlen, Memb[buf])
	case TY_SHORT:
	    call tirowss (itp, icp, otp, ocp, rowsel, row, maxlen, Mems[buf])
	case TY_INT, TY_LONG:
	    call tirowsi (itp, icp, otp, ocp, rowsel, row, maxlen, Memi[buf])
	case TY_REAL:
	    call tirowsr (itp, icp, otp, ocp, rowsel, row, maxlen, Memr[buf])
	case TY_DOUBLE:
	    call tirowsd (itp, icp, otp, ocp, rowsel, row, maxlen, Memd[buf])
	default:
	    call error (1, "Non-supported data type.")
	}

	call sfree (sp)
end
