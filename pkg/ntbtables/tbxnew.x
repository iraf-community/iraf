include <tbset.h>
include "tbtables.h"

# tbxnew -- X create new table
# Allocate space for indef record buffer, write record for size information,
# and write each column descriptor (if any) to table.
#
# Phil Hodge,  7-Mar-1989  Eliminate TB_MODSIZE.
# Phil Hodge, 16-Nov-1990  Use temporary variable instead of TB_FILE(tp) when
#	opening table file so TB_FILE(tp) will still be NULL in case of error.
# Phil Hodge, 30-Mar-1993  TB_INDEF is now TY_CHAR rather than TY_REAL.
# Phil Hodge, 15-Apr-1998  Change calling sequence of tbcwcd.

procedure tbxnew (tp)

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
errchk	tbtwsi, tbhwpr, tbcwcd, open, calloc

begin
	call smark (sp)
	# Allocate space for dummy parameter & column descriptor records.
	call salloc (pstr, SZ_PARREC, TY_CHAR)
	call salloc (colinfo, LEN_COLSTRUCT, TY_STRUCT)

	if (TB_ROWLEN(tp) < 0)
	    TB_ROWLEN(tp) = DEFMAXCOLS

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

	# Allocate space for indef record.
	call calloc (TB_INDEF(tp), TB_ROWLEN(tp), TY_CHAR)

	# Write each column descriptor to table.
	do colnum = 1, TB_NCOLS(tp) {		# ncols may still be zero
	    colptr = TB_COLINFO(tp,colnum)
	    call tbcwcd (tp, colptr)	# write column descr
	    call tbbnll (tp, colptr)	# assign indef value in indef record
	}

	# Write dummy records for column descriptors to fill out allocated space
	call amovki (0, Memi[colinfo], LEN_COLSTRUCT)	# Zero buffer
	do colnum = TB_NCOLS(tp)+1, TB_MAXCOLS(tp) {
	    COL_NUMBER(colinfo) = colnum
	    call tbcwcd (tp, colinfo)	# write dummy descriptor
	}

	call sfree (sp)
end
