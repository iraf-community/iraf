include <tbset.h>
include "tbtables.h"

define	DEFNUMROWS  100

# tbynew -- Y create new table
# Allocate space for indef record buffer, write record for size information,
# and write each column descriptor (if any) to table.
# This version is for column-ordered SDAS tables.  The differences between
# this and tbxnew are that this does not allocate an indef record, a
# default number of allocated rows is assigned if none has been specified,
# and indef values are written to all columns of all allocated rows.
#
# Phil Hodge,  7-Mar-1989  Eliminate TB_MODSIZE.
# Phil Hodge, 16-Nov-1990  Use temporary variable instead of TB_FILE(tp) when
#	opening table file so TB_FILE(tp) will still be NULL in case of error.
# Phil Hodge, 15-Apr-1998  Change calling sequence of tbcwcd.

procedure tbynew (tp)

pointer tp		# i: pointer to table descriptor
#--
pointer colptr
pointer sp
pointer pstr		# buffer for dummy space for user parameters
pointer colinfo		# buffer for dummy space for extra column descr
int	k		# loop index
int	parnum		# parameter number (dummy values for "header")
int	colnum		# column number
int	fd		# fd for table file
int	open()
errchk	tbtwsi, tbhwpr, tbyncn, tbcwcd, open

begin
	call smark (sp)
	# Allocate space for dummy parameter & column descriptor records.
	call salloc (pstr, SZ_PARREC, TY_CHAR)
	call salloc (colinfo, LEN_COLSTRUCT, TY_INT)

	if (TB_ALLROWS(tp) <= 0)
	    TB_ALLROWS(tp) = DEFNUMROWS

	# Open the file.  This was split into two lines so that if the open
	# fails, TB_FILE(tp) will be unchanged (should be NULL).
	fd = open (TB_NAME(tp), TB_IOMODE(tp), BINARY_FILE)
	TB_FILE(tp) = fd

	# Write size information to table.
	call tbtwsi (tp)

	# Write blank records for user parameters to fill out allocated space
	do k = 1, SZ_PARREC
	    Memc[pstr+k-1] = ' '
	Memc[pstr+SZ_PARREC] = EOS
	do parnum = 1, TB_MAXPAR(tp)
	    call tbhwpr (tp, parnum, Memc[pstr])

	# We don't need an indef record for a column-ordered table.
	TB_INDEF(tp) = NULL

	# Write each column descriptor to table.
	do colnum = 1, TB_NCOLS(tp) {		# ncols may still be zero
	    colptr = TB_COLINFO(tp,colnum)
	    call tbcwcd (tp, colptr)
	}
	# Write dummy records for column descriptors to fill out allocated space
	call amovki (0, Memi[colinfo], LEN_COLSTRUCT)	# Zero buffer
	do colnum = TB_NCOLS(tp)+1, TB_MAXCOLS(tp) {
	    COL_NUMBER(colinfo) = colnum
	    call tbcwcd (tp, colinfo)		# write dummy descriptor
	}

	# Because the table is column-ordered, we must write indef values
	# to each row of each column.
	do colnum = 1, TB_NCOLS(tp) {		# ncols may still be zero
	    colptr = TB_COLINFO(tp,colnum)
	    call tbyncn (tp, colptr, 1)
	}

	call sfree (sp)
end
