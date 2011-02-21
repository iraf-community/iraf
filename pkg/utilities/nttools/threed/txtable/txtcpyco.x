
#  TXTCPYCO  --  Copy column information
#
#
#
#
#
#  Revision history:
#  ----------------
#
#  03-Jan-97  -  Implemented after code review (IB)


procedure txtcpyco (otp, colptr, newcol, numptr, colname, colunits, colfmt,
                    compact)

pointer otp, colptr, newcol, colname, colunits, colfmt
int	numptr
bool	compact
#--
pointer	ocp
int     iptr, colnum, datatype, lendata, lenfmt

pointer	tcs_column()

begin
	do iptr = 1, numptr {
	    ocp = tcs_column (Memi[colptr+iptr-1])
	    call tbcinf (ocp, colnum, Memc[colname], Memc[colunits], 
                         Memc[colfmt], datatype, lendata, lenfmt)

	    # All columns in output are scalar-type !
	    # Column info for input scalars depends on compact mode.
            # If compact=no, just leave output column as scalar.
            # If compact=yes, signal input scalar by setting column
            # pointer to NULL.
	    if (compact && (lendata == 1)) {
	        Memi[newcol+iptr-1] = NULL
	     } else {
	        call tbcdef (otp, ocp, Memc[colname], Memc[colunits], 
                             Memc[colfmt], datatype, 1, 1)
	        Memi[newcol+iptr-1] = ocp    
	    }
	}
end
