include <error.h>
include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# tbuopn -- open existing table
# This routine opens an existing table file.
# For binary tables the default extension is appended.
# For each column, create a descriptor and read column info from
# the table.  Also create the indef record buffer.  For text tables
# the contents are read into memory.
# This version is for either row or column ordered SDAS tables
# or for text files.
# (Renamed from tbwopn.)
#
# Phil Hodge, 26-Feb-1988  Close table file if error in tbtrsi
# Phil Hodge,  7-Mar-1989  Eliminate TB_MODSIZE.
# Phil Hodge, 16-Nov-1990  Use local variable instead of TB_FILE(tp) when
#	opening table file so TB_FILE(tp) will still be NULL in case of error.
# Phil Hodge, 14-Jan-1992  Add option for text table type.
# Phil Hodge, 16-Nov-1992  Close TB_FILE in tbtopn instead of here if error.
# Phil Hodge, 30-Mar-1993  TB_INDEF is now TY_CHAR rather than TY_REAL.
# Phil Hodge, 20-Sep-1994  Don't allocate an INDEF buffer if readonly.
# Phil Hodge, 15-Dec-1994  Table name is now SZ_LINE instead of SZ_FNAME.
# Phil Hodge, 23-Dec-1994  Add option for CDF or FITS file.
# Phil Hodge, 14-Apr-1998  Change calling sequence of tbcrcd.
# Phil Hodge,  7-Jun-1999  Replace TB_F_TYPE by TB_TYPE;
#	when allocating TB_COLPTR, the type is TY_POINTER, not TY_INT.
# Phil Hodge,  3-Aug-1999  For FITS table, get all column info in one call.

procedure tbuopn (tp)

pointer tp		# i: pointer to table descriptor
#--
pointer colptr		# pointer to column descriptor
int	colnum		# column number (a loop index)
int	fd		# fd for table file
int	open()
errchk	open, calloc, malloc, tbtext, tbtrsi, tbcrcd, tbfopn, tbfrcd, tbzopn

begin
	# Open the file.
	if (TB_TYPE(tp) == TBL_TYPE_TEXT) {
	    fd = open (TB_NAME(tp), TB_IOMODE(tp), TEXT_FILE)
	    TB_FILE(tp) = fd

	} else if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    # Table in a FITS file.
	    call tbfopn (tp)

	} else if (TB_TYPE(tp) == TBL_TYPE_CDF) {
	    # Table in a CDF file.
	    ;   # call tbvopn (tp)

	} else {
	    # For a binary table we need to check that there's an
	    # extension, and if not, append the default extension.
	    call tbtext (TB_NAME(tp), TB_NAME(tp), SZ_LINE)
	    fd = open (TB_NAME(tp), TB_IOMODE(tp), BINARY_FILE)
	    TB_FILE(tp) = fd
	}

	TB_IS_OPEN(tp) = true

	# Read size information from table.
	call tbtrsi (tp)

	# Allocate space for the array of pointers to column descriptors.
	call malloc (TB_COLPTR(tp), TB_MAXCOLS(tp), TY_POINTER)

	# Create column descriptors.
	# (For a text table, TB_NCOLS will still be zero.)
	do colnum = 1, TB_NCOLS(tp) {
	    call malloc (colptr, LEN_COLSTRUCT, TY_STRUCT)
	    TB_COLINFO(tp,colnum) = colptr
	}

	# Read column descriptors from the table.
	if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    call tbfrcd (tp, TB_COLINFO(tp,1), TB_NCOLS(tp))
	} else {
	    do colnum = 1, TB_NCOLS(tp) {
		colptr = TB_COLINFO(tp,colnum)
		call tbcrcd (tp, colptr, colnum)
	    }
	}

	if (TB_TYPE(tp) == TBL_TYPE_S_ROW && !TB_READONLY(tp)) {

	    # Allocate space for indef record.
	    call calloc (TB_INDEF(tp), TB_ROWLEN(tp), TY_CHAR)
	    # Assign the appropriate indef value in the indef record buffer.
	    do colnum = 1, TB_NCOLS(tp) {
		colptr = TB_COLINFO(tp,colnum)
		call tbbnll (tp, colptr)
	    }

	} else if (TB_TYPE(tp) == TBL_TYPE_TEXT) {

	    # Read the contents of the file into memory.
	    call tbzopn (tp)
	}
end
