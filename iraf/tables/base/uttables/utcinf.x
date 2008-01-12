include <tbset.h>

# utcinf -- get info about a column
# This routine obtains information about an existing column in a table:
# the name of the column, the units, the format for display, and the
# data type.  The data type will be returned as the SPP symbolic integer
# unless the column contains character strings, in which case the value
# will be -N, where N is the number of characters the column may contain.
#
# P.E. Hodge, 7-Aug-87  Datatype changed from character string to integer;
#			include istat in calling sequence.

procedure utcinf (tp, colptr,
		f77nam, f77nit, f77fmt, datatype, istat)

pointer tp			# i: Pointer to table descriptor
pointer colptr			# i: Pointer to column
				# o: Name of column
%      character*(*) f77nam
				# o: Units for column
%      character*(*) f77nit
				# o: Print format for column
%      character*(*) f77fmt
int	datatype		# o: Data type of column
int	istat			# o: Return status, =1 if colptr=0
#--
char	colname[SZ_COLNAME]	# Column name
char	colunits[SZ_COLUNITS]	# Units for column
char	colfmt[SZ_COLFMT]	# Print format for display of column
int	colnum			# Column number
int	lendata			# Length (unit=SZ_CHAR) of data in table
int	lenfmt			# Bytes for print format

begin
	istat = OK

	if (colptr == NULL) {
	    istat = 1
	    return
	}
	call tbcinf (colptr,
		colnum, colname, colunits, colfmt, datatype, lendata, lenfmt)

	call f77pak (colname, f77nam, SZ_COLNAME)
	call f77pak (colunits, f77nit, SZ_COLUNITS)
	# Convert the format from SPP to a legal Fortran format.
	call utbptf (colfmt, colfmt)
	call f77pak (colfmt, f77fmt, SZ_COLFMT)
end
