include <tbset.h>

# tbcfnd1 -- find a column
# Find a column in a table.  This just calls tbcfnd.
# If the column is not found in the table, colptr will be set to NULL.
#
# Phil Hodge, 21-Aug-1995  Subroutine created.

procedure tbcfnd1 (tp, colname, colptr)

pointer tp			# i: pointer to table descriptor
char	colname[SZ_COLNAME]	# i: name of column
pointer colptr			# o: pointer to column, or NULL
#--
pointer cp[1]			# pointer to column, or NULL
char	cname[SZ_COLNAME,1]	# name of column
errchk	tbcfnd

begin
	call strcpy (colname[1], cname[1,1], SZ_COLNAME)

	call tbcfnd (tp, cname, cp, 1)

	colptr = cp[1]
end
