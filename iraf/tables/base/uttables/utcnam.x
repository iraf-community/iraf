include <tbset.h>

# utcnam -- change column name
# This procedure replaces the column name.
#
# P.E. Hodge, 16-Sep-87  Delete section to write error message.

procedure utcnam (tp, colptr, f77nam, istat)

pointer tp			# i: pointer to table descriptor
pointer colptr			# i: pointer to a column descriptor
				# i: new column name
%      character*(*) f77nam
int	istat			# o: return status
#--
char	colname[SZ_COLNAME]	# Column name
int	errcode()

begin
	istat = OK

	call f77upk (f77nam, colname, SZ_COLNAME)

	iferr (call tbcnam (tp, colptr, colname)) {
	    istat = errcode()
	}
end
