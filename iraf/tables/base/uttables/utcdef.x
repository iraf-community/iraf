include <tbset.h>

# utcdef -- define columns
# Use this routine to define new columns.  If the table is new, it will
# generally be faster to define all the columns before opening the table,
# but it will still be possible to call UTCDEF after opening the table.
# If all columns are defined before opening a new table (using UTTOPN)
# it will not be necessary to explicitly specify (using UTPSET) the record
# length that is required for those columns.
# The names in the array colname will be checked against the columns that
# have already been defined in the table, but the calling routine should
# make sure there are not any duplicate names within colname itself.  The
# order of the newly created columns in the table will be the same as
# the order of the names in the array colname.
#
# P.E. Hodge,  7-Aug-87  Datatype changed from character string to integer.
# P.E. Hodge, 16-Sep-87  Delete section to write error message.

procedure utcdef (tp, f77nam, f77nit, f77fmt, datatype, numcols,
		colptr, istat)

pointer tp			# i: Pointer to table descriptor
				# i: Array of column names
%      character*(*) f77nam(1)
				# i: Array of column units
%      character*(*) f77nit(1)
				# i: Array of print formats for columns
%      character*(*) f77fmt(1)
int	datatype[numcols]	# i: Array of data types of columns
pointer colptr[numcols]		# o: Array of pointers to new columns
int	numcols			# i: Number of columns to be defined
int	istat			# o: Return status
#--
pointer sp			# Stack pointer
char	pfmt[SZ_COLFMT]		# Print format for display of a column
int	cnam			# Pointer for scratch for column names
int	cunit			# Scratch for units for array of columns
int	cfmt			# For formats for display of each column
int	clen			# For data length of each column
int	k			# Loop index
int	jnam, junit, jfmt	# Array indexes for name, units, format
int	errcode()

begin
	istat = OK

	call smark (sp)
	# Add one extra character for EOS because these are arrays.
	call salloc (cnam,  (SZ_COLNAME+1)*numcols,  TY_CHAR)
	call salloc (cunit, (SZ_COLUNITS+1)*numcols, TY_CHAR)
	call salloc (cfmt,  (SZ_COLFMT+1)*numcols,   TY_CHAR)
	call salloc (clen, numcols, TY_INT)

	jnam = cnam			# incremented for each column
	junit = cunit
	jfmt = cfmt
	do k = 1, numcols {
	    call f77upk (f77nam[k], Memc[jnam], SZ_COLNAME)
	    call f77upk (f77nit[k], Memc[junit], SZ_COLUNITS)
	    call f77upk (f77fmt[k], pfmt, SZ_COLFMT)
	    call strlwr (pfmt)

	    # The data type contains the length info.
	    Memi[clen+k-1] = 1
	    # Convert the format from Fortran to SPP.
	    call tbbftp (pfmt, Memc[jfmt])
	    jnam = jnam + SZ_COLNAME+1
	    junit = junit + SZ_COLUNITS+1
	    jfmt = jfmt + SZ_COLFMT+1
	}

	iferr (call tbcdef (tp, colptr,
			Memc[cnam], Memc[cunit], Memc[cfmt],
			datatype, Memi[clen], numcols)) {
	    istat = errcode()
	}
	call sfree (sp)
end
