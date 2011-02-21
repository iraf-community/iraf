include	<tbset.h>

#
# TIROWS  --  Expand row selector into array and copy it into output 
#             table cell.
#
#
#
#
#  Revision history:
#  ----------------
#  20-Jan-1997  -  Task created (I.Busko)
#   7-Feb-2000  -  For datatype = 'c', make buf an array of strings (P.Hodge)


procedure tirowsi (itp, icp, otp, ocp, rowsel, orow, len, buf)

pointer	itp		# i: input table descriptor
pointer	icp		# i: input column descriptor
pointer	otp		# i: output table descriptor
pointer	ocp		# i: output column descriptor
char	rowsel[ARB]	# i: row selector
int	orow		# i: row in output table where to write into
int	len		# i: buffer length
int	buf[ARB]
#--
double	undefd
real	undefr
pointer	pcode
int	undefi, i, nelem, irow, numrow, alength
short	undefs

pointer	trsopen()
int	tbpsta(), tbalen()
bool	trseval()

begin
	# Loop over selected rows on input table.
	pcode  = trsopen (itp, rowsel)
	numrow = tbpsta (itp, TBL_NROWS)
	nelem = 0
	do irow = 1, numrow {
	    if (trseval (itp, irow, pcode)) {
	        nelem = nelem + 1
	        if (nelem > len) {
	            nelem = len
	            break
	        }
	        # Get element and store in buffer.
	        call tbegti (itp, icp, irow, buf[nelem])
	    }
	}
	call trsclose (pcode)

	# Write buffer into array cell element.
	call tbapti (otp, ocp, orow, buf, 1, nelem)

	# If number of selected rows in current input table
	# is smaller than output table array length, fill
	# remaining array elements with INDEF.
	alength = tbalen (ocp)
	if (alength > nelem) {
	    undefd = INDEFD
	    undefr = INDEFR
	    undefi = INDEFI
	    undefs = INDEFS
	    do i = nelem+1, alength {
	        call tbapti (otp, ocp, orow, undefi, i, 1)
	    }
	}
end
