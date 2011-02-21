#
# TXTCPT  --  Copy data to output table. If array, copy into column.
#             If scalar, either write as column or write into header.
#
#
#
#
#  Revision history:
#  ----------------
#
#  22-Nov-1996  -  Task created (I.Busko)
#   7-Feb-2000  -  For datatype = 'c', make buf an array of strings (P.Hodge)

procedure txtcptt (otp, ocp, buf, maxch, start, nbuf, icol, compact)

pointer	otp		# i: table descriptor
pointer	ocp		# i: column descriptor
char	buf[maxch,ARB]	# i: array of values
int	maxch		# i: max length of string
int	start		# i: starting row in output table
int	nbuf		# i: number of elements to write into output
int	icol		# i: column number in input table
bool	compact		# i: write scalars as header keywords ?
#--

begin
	if (ocp != NULL) {

	    call tbcptt (otp, ocp, buf, maxch, start, nbuf)

	} else if (compact) {

	    call txthvt  (otp, icol, buf)
	}
end
