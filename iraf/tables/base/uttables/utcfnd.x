include <tbset.h>

# utcfnd -- find columns
# This routine searches for columns by name in a table.  For each column
# name that was found the corresponding column identifier will be a positive
# integer that may subsequently be used for I/O to that column until the
# table is closed.  For each column that was not found the corresponding
# column identifier will be zero.
# This routine may be called more than once for a given column name.
#
# P.E. Hodge, 7-Aug-87  Datatype changed from character string to integer;
#			istat included in calling sequence.

procedure utcfnd (tp, f77nam, numcols, colptr, istat)

pointer tp			# i: Pointer to table descriptor.
				# i: Array of column names.
%      character*(*) f77nam(1)
int	numcols			# i: Number of columns in array f77nam.
pointer colptr[numcols]		# o: Pointers to columns or zero.
int	istat			# o: Return status; -1 if any colptr is zero.
#--
pointer sp			# Stack pointer
int	cnam			# Pointer to scratch for column names
int	jnam			# Index for column name in scratch array
int	k			# Loop index

begin
	istat = OK

	call smark (sp)
	# Add one extra character for EOS.
	call salloc (cnam, (SZ_COLNAME+1)*numcols, TY_CHAR)

	jnam = cnam
	do k = 1, numcols {
	    call f77upk (f77nam[k], Memc[jnam], SZ_COLNAME)
	    jnam = jnam + SZ_COLNAME+1
	}
	call tbcfnd (tp, Memc[cnam], colptr, numcols)

	do k = 1, numcols {
	    if (colptr[k] == NULL) {
		istat = -1
		break
	    }
	}
	call sfree (sp)
end
