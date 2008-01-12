include <tbset.h>

# utcnit -- change column units
# This procedure replaces the units for a column.
#
# P.E. Hodge, 16-Sep-87  Delete section to write error message.

procedure utcnit (tp, colptr, f77nit, istat)

pointer tp			# i: pointer to table descriptor
pointer colptr			# i: pointer to a column descriptor
				# i: new value of units for column
%      character*(*) f77nit
int	istat			# o: return status
#--
char	colunits[SZ_COLUNITS]	# Column units
int	errcode()

begin
	istat = OK

	call f77upk (f77nit, colunits, SZ_COLUNITS)

	iferr (call tbcnit (tp, colptr, colunits)) {
	    istat = errcode()
	}
end
