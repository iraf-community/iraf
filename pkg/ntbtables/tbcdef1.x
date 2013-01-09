include <tbset.h>

# tbcdef1 -- define a column
# Define (create) one table column.  This just calls tbcdef.
#
# Phil Hodge, 21-Aug-1995  Subroutine created.

procedure tbcdef1 (tp, colptr, colname, colunits, colfmt, datatype, nelem)

pointer tp			# i: pointer to table descriptor
pointer colptr			# o: pointer to new column
char	colname[SZ_COLNAME]	# i: name of column
char	colunits[SZ_COLUNITS]	# i: units for column
char	colfmt[SZ_COLFMT]	# i: print format for column
int	datatype		# i: data types of column
int	nelem			# i: number of elements for column
#--
pointer cp[1]			# pointer to new column
char	cname[SZ_COLNAME,1]	# name of column
char	cunits[SZ_COLUNITS,1]	# units for column
char	cfmt[SZ_COLFMT,1]	# print format for column
int	dtype[1]		# datatype
int	larray[1]		# nelem
errchk	tbcdef

begin
	call strcpy (colname[1], cname[1,1], SZ_COLNAME)
	call strcpy (colunits[1], cunits[1,1], SZ_COLUNITS)
	call strcpy (colfmt[1], cfmt[1,1], SZ_COLFMT)
	dtype[1] = datatype
	larray[1] = nelem

	call tbcdef (tp, cp, cname, cunits, cfmt, dtype, larray, 1)

	colptr = cp[1]
end
