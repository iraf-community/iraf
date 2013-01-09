include <tbset.h>
include "tbtables.h"
include "tblfits.h"	# defines FITS_INDEFI and FITS_INDEFS

# tbfdef -- write new column in FITS table
# This routine creates a new column in a FITS table, writing the header
# keywords and also the INDEF data values.
#
# Phil Hodge,  6-Jul-1995  Subroutine created
# Phil Hodge, 23-Apr-1997  Add option for FITS ASCII table.
# Phil Hodge, 12-Mar-1999  Change the sizes of ttype, tform and tunit
#	from SZ_FNAME to SZ_FTTYPE, SZ_FTFORM and SZ_FTUNIT respectively,
#	and change the size of tdisp to SZ_COLFMT.
# Phil Hodge,  7-Jun-1999  Use TB_SUBTYPE instead of TB_HDUTYPE.

procedure tbfdef (tp, cp)

pointer tp		# i: pointer to table descriptor
pointer cp		# i: pointer to descriptor for new column
#--
pointer sp
pointer keyword		# for keyword name (TDISPn)
pointer tdisp		# for print format
pointer ttype, tform, tunit	# for values of header keywords
int	dtype, nelem	# data type and array length
int	colnum		# column number
int	row		# loop index for row number
int	ival		# undefined value for int, short, bool
char	dtype_c		# data type:  'D', 'E', 'J', 'I', 'L, 'A'
int	lenfmt		# width needed for printing value
int	status		# zero is OK
errchk	tbferr, tbfptf, tbftfo

begin
	status = 0

	call smark (sp)
	call salloc (keyword, SZ_FNAME, TY_CHAR)
	call salloc (tdisp, SZ_COLFMT, TY_CHAR)
	call salloc (ttype, SZ_FTTYPE, TY_CHAR)
	call salloc (tform, SZ_FTFORM, TY_CHAR)
	call salloc (tunit, SZ_FTUNIT, TY_CHAR)

	# Get column information.
	call tbcinf (cp,
		colnum, Memc[ttype], Memc[tunit], Memc[tdisp],
		dtype, nelem, lenfmt)

	if (TB_SUBTYPE(tp) == TBL_SUBTYPE_ASCII) {		# ASCII table

	    # Create TFORM string to specify format and data type.
	    call tbftfo (dtype, lenfmt, Memc[tdisp], Memc[tform], SZ_FTFORM)

	} else if (TB_SUBTYPE(tp) == TBL_SUBTYPE_BINTABLE) {	# binary table

	    # Create TFORM string for BINTABLE.
	    switch (dtype) {
	    case TY_DOUBLE:
		dtype_c = 'D'
	    case TY_REAL:
		dtype_c = 'E'
	    case TY_INT:
		dtype_c = 'J'
	    case TY_SHORT:
		dtype_c = 'I'
	    case TY_BOOL:
		dtype_c = 'L'
	    default:
		dtype_c = 'A'
	    }
	    if (dtype > 0) {
		call sprintf (Memc[tform], SZ_FNAME, "%d%c")
		    call pargi (nelem)
		    call pargc (dtype_c)
	    } else if (nelem > 1) {		# array of char strings
		call sprintf (Memc[tform], SZ_FNAME, "%d%c%d")
		    call pargi (-dtype * nelem)	# FITSIO special convention
		    call pargc (dtype_c)
		    call pargi (-dtype)
	    } else {				# character string
		call sprintf (Memc[tform], SZ_FNAME, "%d%c")
		    call pargi (-dtype)
		    call pargc (dtype_c)
	    }

	} else {
	    call error (1, "tbfdef:  invalid HDU type")
	}

	# Create new column.
	call fsicol (TB_FILE(tp), colnum, Memc[ttype], Memc[tform], status)
	if (status != 0)
	    call tbferr (status)

	# Create TUNIT string, and add to header.
	call sprintf (Memc[keyword], SZ_FNAME, "TUNIT%d")
	    call pargi (colnum)
	call fspkys (TB_FILE(tp), Memc[keyword],
			Memc[tunit], "column units", status)
	if (status != 0)
	    call tbferr (status)

	if (TB_SUBTYPE(tp) == TBL_SUBTYPE_ASCII) {		# ASCII table

	    # Add TNULL (an *) to header.
	    call sprintf (Memc[keyword], SZ_FNAME, "TNULL%d")
		call pargi (colnum)
	    call fspkys (TB_FILE(tp), Memc[keyword],
		    "*", "undefined value for column", status)
	    if (status != 0)
		call tbferr (status)

	} else if (TB_SUBTYPE(tp) == TBL_SUBTYPE_BINTABLE) {	# binary table

	    # Create TDISP string, and add to header.
	    call sprintf (Memc[keyword], SZ_FNAME, "TDISP%d")
		call pargi (colnum)
	    call tbfptf (Memc[tdisp], Memc[tdisp], SZ_COLFMT)	# in-place
	    call fspkys (TB_FILE(tp), Memc[keyword],
			Memc[tdisp], "display format", status)
	    if (status != 0)
		call tbferr (status)

	    # Add TNULL to header.
	    if (dtype == TY_INT || dtype == TY_SHORT) {

		call sprintf (Memc[keyword], SZ_FNAME, "TNULL%d")
		    call pargi (colnum)
		if (dtype == TY_INT)
		    ival = FITS_INDEFI
		else if (dtype == TY_SHORT)
		    ival = FITS_INDEFS
		call fspkyj (TB_FILE(tp), Memc[keyword],
			ival, "undefined value for column", status)
		if (status != 0)
		    call tbferr (status)
	    }
	}

	call fsrdef (TB_FILE(tp), status)	# shouldn't be necessary

	# Fill the new column with INDEF.
	do row = 1, TB_NROWS(tp) {
	    call fspclu (TB_FILE(tp), colnum, row, 1, nelem, status)
	    if (status != 0)
		call tbferr (status)
	}

	call sfree (sp)
end

procedure tbftfo (dtype, lenfmt, tdisp, tform, maxch)

int	dtype		# i: data type of column
int	lenfmt		# i: width needed for printing value
char	tdisp[ARB]	# i: display format (could be SPP style)
char	tform[maxch]	# o: TFORM for ASCII table column
int	maxch		# i: size of tform string
#--
bool	badfmt		# bad print format?
errchk	tbfptf

begin
	badfmt = false

	switch (dtype) {
	case TY_DOUBLE:
	    call tbfptf (tdisp, tform, maxch)
	    if (tform[1] == 'E') {
		tform[1] = 'D'
	    } else if (tform[1] == 'G') {
		tform[1] = 'D'
	    } else if (tform[1] == 'F') {
		call error (1,
		"Use E format for double precision in FITS ASCII table")
	    }
	    badfmt = (tform[1] != 'D')
	case TY_REAL:
	    call tbfptf (tdisp, tform, maxch)
	    if (tform[1] == 'G')
		tform[1] = 'E'
	    if (tform[1] != 'E' && tform[1] != 'F')
		badfmt = true
	case TY_INT, TY_SHORT:
	    call sprintf (tform, maxch, "I%d")
		call pargi (lenfmt)
	case TY_BOOL:
	    call error (1, "Boolean column not supported in FITS ASCII table")
	default:
	    call sprintf (tform, maxch, "A%d")
		call pargi (lenfmt)
	}

	if (badfmt) {
	    call error (1,
	"Use simple Fortran format for new column in FITS ASCII table")
	}
end
