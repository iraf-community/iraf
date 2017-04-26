
#  TXTCPYSC  --  Copy scalar columns in compact mode
#
#
#
#
#
#  Revision history:
#  ----------------
#
#  03-Jan-97  -  Implemented after code review (IB)


procedure txtcpysc (otp, colptr, newcol, numptr, colname, colunits, colfmt)

pointer otp, colptr, newcol, colname, colunits, colfmt
int	numptr

pointer	icp
int     iptr, colnum, datatype, lendata, lenfmt

pointer	tcs_column

begin
	do iptr = 1, numptr {
	    if (Memi[newcol+iptr-1] == NULL) {
	        icp = tcs_column (Memi[colptr+iptr-1])
	        call tbcinf (icp, colnum, Memc[colname], Memc[colunits], 
                             Memc[colfmt], datatype, lendata, lenfmt)
	        call txthc (otp, colnum, Memc[colname], Memc[colunits], 
                            Memc[colfmt], datatype, lenfmt)
	    }
	}
end
