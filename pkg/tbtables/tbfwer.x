include <mach.h>		# for SZB_CHAR
include <tbset.h>
include "tbtables.h"
include "tblfits.h"		# defines FITS_TNULL_NOT_SET

# tbfwer -- write empty rows to end of FITS table
#
# Phil Hodge,  6-Jul-1995  Subroutine created
# Phil Hodge,  3-Jun-1996  Remove call to fsirow.
# Phil Hodge, 23-Apr-1997  Add TNULL to header for FITS ASCII table.
# Phil Hodge, 29-Jul-1997  Call fsirow to create new rows.
# Phil Hodge,  7-Jun-1999  Use TB_SUBTYPE instead of TB_HDUTYPE.
# Phil Hodge, 25-Aug-2000  Delete the call to fsirow.
# Phil Hodge, 12-Sep-2000  Use TB_INDEF for a row of undefined values.

procedure tbfwer (tp, nrows, new_nrows)

pointer tp		# i: pointer to table descriptor
int	nrows		# i: number of rows on entry to this routine
int	new_nrows	# i: number of rows after calling this routine
#--
pointer sp
pointer keyword		# for TNULL keyword
pointer cp		# pointer to one column descriptor
int	row, col	# row and column numbers
int	nelem		# number of elements, if column is array type
int	dtype		# data type of column (needed to set TNULL)
int	ival		# undefined value
int	status		# zero is OK
#
pointer comment		# for getting NAXIS1
int	nbytes		# value of NAXIS1 (length of a row, in bytes)
int	nchar		# number of char elements in nbytes
#
pointer tbcnum()
int	tbcigi()
errchk	tbferr

begin
	if (new_nrows <= nrows)
	    return			# nothing to do

	call smark (sp)
	call salloc (keyword, SZ_FNAME, TY_CHAR)

	status = 0

	if (TB_INDEF_IS_CURRENT(tp)) {

	    # Write the INDEF record to all the new rows.
	    do row = nrows+1, new_nrows {
		call fsptbb (TB_FILE(tp), row, 1, TB_ROWLEN(tp),
			Memc[TB_INDEF(tp)], status)
		if (status != 0)
		    call tbferr (status)
	    }

	} else {

	    # We don't have a valid INDEF record yet, so explictly write
	    # the undefined values for one row, then read that into TB_INDEF.

	    row = nrows + 1
	    do col = 1, TB_NCOLS(tp) {		# loop over columns

		cp = tbcnum (tp, col)
		nelem = tbcigi (cp, TBL_COL_LENDATA)

		call fspclu (TB_FILE(tp), col, row, 1, nelem, status)

		if (status == FITS_TNULL_NOT_SET) {

		    status = 0
		    call ftcmsg()

		    # Create TNULL string, and add to header.

		    call sprintf (Memc[keyword], SZ_FNAME, "TNULL%d")
			call pargi (col)

		    if (TB_SUBTYPE(tp) == TBL_SUBTYPE_ASCII) {

			# TNULL = "*"
			call fspkys (TB_FILE(tp), Memc[keyword],
				"*", "undefined value for column", status)

		    } else if (TB_SUBTYPE(tp) == TBL_SUBTYPE_BINTABLE) {

			dtype = tbcigi (cp, TBL_COL_DATATYPE)
			if (dtype == TY_INT || dtype == TY_SHORT) {
			    if (dtype == TY_INT)
				ival = FITS_INDEFI
			    else if (dtype == TY_SHORT)
				ival = FITS_INDEFS
			    call fspkyj (TB_FILE(tp), Memc[keyword],
				ival, "undefined value for column", status)
			}		# else don't do anything
		    }
		    # try again
		    call fsrdef (TB_FILE(tp), status)
		    call fspclu (TB_FILE(tp), col, row, 1, nelem, status)
		}
		if (status != 0)
		    call tbferr (status)
	    }

	    # Allocate memory for TB_INDEF, and read the record that we just
	    # wrote, reading into TB_INDEF.

	    call salloc (comment, SZ_FNAME, TY_CHAR)
	    call fsrdef (TB_FILE(tp), status)
	    call fsgkyj (TB_FILE(tp), "NAXIS1", nbytes, Memc[comment], status)
	    if (status != 0)
		call tbferr (status)

	    TB_ROWLEN(tp) = nbytes	# note:  this is the number of BYTES

	    # round up
	    nchar = (nbytes + SZB_CHAR-1) / (SZB_CHAR)
	    call realloc (TB_INDEF(tp), nchar, TY_CHAR)
	    call fsgtbb (TB_FILE(tp), row, 1, TB_ROWLEN(tp),
			Memc[TB_INDEF(tp)), status)
	    if (status != 0)
		call tbferr (status)

	    TB_INDEF_IS_CURRENT(tp) = true

	    # Now that we have the INDEF record in TB_INDEF, write it to
	    # all the other new rows.
	    do row = nrows+2, new_nrows {
		call fsptbb (TB_FILE(tp), row, 1, TB_ROWLEN(tp),
			Memc[TB_INDEF(tp)], status)
		if (status != 0)
		    call tbferr (status)
	    }
	}

	call sfree (sp)
end
