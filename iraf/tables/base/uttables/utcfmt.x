include <tbset.h>

# utcfmt -- change print format for column
# This procedure replaces the print format for a column.
#
# P.E. Hodge, 15-Sep-87  Replace call to utbftp by tbbftp.
# P.E. Hodge, 16-Sep-87  Delete section to write error message.

procedure utcfmt (tp, colptr, f77fmt, istat)

pointer tp			# i: pointer to table descriptor
pointer colptr			# i: pointer to a column descriptor
				# i: new print format for column
%      character*(*) f77fmt
int	istat			# o: return status
#--
char	colfmt[SZ_COLFMT]	# print format for column
int	errcode()

begin
	istat = OK

	call f77upk (f77fmt, colfmt, SZ_COLFMT)

	# Convert the format from Fortran to SPP.
	call tbbftp (colfmt, colfmt)

	iferr (call tbcfmt (tp, colptr, colfmt)) {
	    istat = errcode()
	}
end
