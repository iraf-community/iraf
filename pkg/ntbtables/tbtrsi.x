include <mach.h>
include <tbset.h>
include "tbtables.h"
include "tblerr.h"

define	DEF_ALLROWS	100	# initial number of "rows" for text file

# tbtrsi -- read size info
# This procedure reads the size information record from a table and
# saves the values in the table descriptor.
#
# Phil Hodge, 15-Oct-1987  Seek BOF instead of 1.
# Phil Hodge, 10-Nov-1987  Check table type to see if it is valid.
# Phil Hodge, 14-Jan-1992  Add option for text table type.
# Phil Hodge,  5-Apr-1993  Also read version number.
# Phil Hodge,  8-Jun-1995  Modify for FITS file.
# Phil Hodge,  7-Jun-1999  For text tables, don't set TB_ALLROWS,
#		TB_MAXCOLS, TB_MAXPAR, or TB_NPAR.
# Phil Hodge, 22-Oct-2004  Check for byte-swapped size information record,
#		in order to give a more informative error message.

procedure tbtrsi (tp)

pointer tp			# Pointer to table descriptor
#--
int	sizinfo[LEN_SIZINFO]	# Size information record
long	tbtbod()
int	read()
errchk	seek, read, tbfrsi

begin
	if (TB_TYPE(tp) == TBL_TYPE_TEXT) {
	    TB_BOD(tp) = 0
	    return
	}

	if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    # Get number of rows and column in FITS table.
	    call tbfrsi (tp)
	    return
	}

	call seek (TB_FILE(tp), BOF)
	if (read (TB_FILE(tp), sizinfo, SZ_SIZINFO) == EOF)
	    call error (ER_TBFILEMPTY, "table data file is empty")
        if (SZ_INT != SZ_INT32)
            call iupk32 (sizinfo, sizinfo, SZ_SIZINFO)

	TB_TYPE(tp) = S_TYPE(sizinfo)
	if ((TB_TYPE(tp) != TBL_TYPE_S_ROW) &&
	    (TB_TYPE(tp) != TBL_TYPE_S_COL)) {
	    # Check whether sizinfo is just byte swapped.
	    call bswap4 (sizinfo, 1, sizinfo, 1, SZ_SIZINFO*SZB_CHAR)
	    if ((S_TYPE(sizinfo) == TBL_TYPE_S_ROW) ||
		(S_TYPE(sizinfo) == TBL_TYPE_S_COL)) {
		call error (ER_BYTESWAPPED,
			"can't open table, it appears to be byte-swapped")
	    } else {
		call error (ER_TBCORRUPTED, "unknown table type")
	    }
	}

	TB_NPAR(tp)    = S_NPAR(sizinfo)
	TB_MAXPAR(tp)  = S_MAXPAR(sizinfo)
	TB_NROWS(tp)   = S_NROWS(sizinfo)
	TB_ALLROWS(tp) = S_ALLROWS(sizinfo)
	TB_NCOLS(tp)   = S_NCOLS(sizinfo)
	TB_MAXCOLS(tp) = S_MAXCOLS(sizinfo)
	TB_COLUSED(tp) = S_COLUSED(sizinfo)
	TB_ROWLEN(tp)  = S_ROWLEN(sizinfo)
	TB_VERSION(tp) = S_VERSION(sizinfo)

	TB_BOD(tp) = tbtbod (TB_MAXPAR(tp), TB_MAXCOLS(tp))
end
