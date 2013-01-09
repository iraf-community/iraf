# This file contains tbpsta, tbtszd, and tbrlen.

include <fset.h>
include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# tbpsta -- get status of table
# Get integer-valued information about a table which has been opened
# or at least initialized.
#
# Phil Hodge, 30-Sep-1987  FIO options added.
# Phil Hodge, 15-Nov-1988  Remove option to get max buffer size.
# Phil Hodge, 10-Jul-1991  Modify IF statement for TBL_DATA_SIZE.
# Phil Hodge,  8-Apr-1993  Modify for short datatype by including
#		TBL_ROWLEN_CHAR and TBL_ROWLEN_CHAR_USED; round up
#		for TBL_ROWLEN and TBL_ROWLEN_USED; add TBL_VERSION.
# Phil Hodge, 30-Sep-1997  Return reduced values for nrows or ncols
#		if selectors were used.  Add tbrlen function, and
#		include tbtszd in this file.
# Phil Hodge,  7-Jun-1999  Add table subtype.
# Phil Hodge, 25-May-2000  For buffer size for a FITS table, call tbfsiz.

int procedure tbpsta (tp, param)

pointer tp			# i: pointer to table descriptor
int	param			# i: the parameter to be determined.
#--
int	value
int	fstati(), tbfsiz(), tbrlen(), tbtszd()
errchk	fstati, tbfsiz

begin
	switch (param) {

	case TBL_NROWS:			# How many rows have been written?
	    if (TB_ROW_SELECT(tp) == YES)
		value = TB_NSEL_ROWS(tp)
	    else
		value = TB_NROWS(tp)
	    return (value)

	case TBL_NCOLS:			# How many columns have been defined?
	    if (TB_COLUMN_SELECT(tp) == YES)
		value = TB_NSEL_COLS(tp)
	    else
		value = TB_NCOLS(tp)
	    return (value)

	case TBL_ROWLEN:		# row length, unit = SZ_REAL
	    value = tbrlen (tp, param)
	    return ((value + SZ_REAL - 1) / SZ_REAL)

	case TBL_ROWLEN_USED:		# Length of row used, unit=SZ_REAL
	    value = tbrlen (tp, param)
	    return ((value + SZ_REAL - 1) / SZ_REAL)

	case TBL_ROWLEN_CHAR:		# row length, unit = SZ_CHAR
	    value = tbrlen (tp, param)
	    return (value)

	case TBL_ROWLEN_CHAR_USED:	# Length of row used, unit=SZ_CHAR
	    value = tbrlen (tp, param)
	    return (value)

	case TBL_ALLROWS:		# Number of allocated rows
	    return (TB_ALLROWS(tp))

	case TBL_WHTYPE:		# What type (row- or column-ordered)?
	    return (TB_TYPE(tp))

	case TBL_SUBTYPE:		# What subtype of table?
	    return (TB_SUBTYPE(tp))

	case TBL_NPAR:			# number of user parameters
	    return (TB_NPAR(tp))

	case TBL_MAXPAR:		# space allocated for user parameters
	    return (TB_MAXPAR(tp))

	case TBL_MAXCOLS:		# space allocated for column descriptors
	    return (TB_MAXCOLS(tp))

	case TBL_VERSION:
	    # version number (zero indexed) of software that created the table
	    return (TB_VERSION(tp))

	case TBL_BUFSIZE:		# size of FIO buffer in chars
	    if ( ! TB_IS_OPEN(tp) )
		call error (ER_TBNOTOPEN,
			"table must be open to get buffer size")
	    if (TB_TYPE(tp) == TBL_TYPE_FITS)
		return (tbfsiz (tp))
	    else
		return (fstati (TB_FILE(tp), F_BUFSIZE))

	case TBL_DATA_SIZE:		# size of data portion of table in chars
	    if (TB_IS_OPEN(tp))
		return (tbtszd(tp))
	    return (0)

	default:
	    call error (ER_TBUNKPARAM, "unknown parameter for tbpsta")
	}
end

# tbtszd -- get size of data portion
# This function returns the size in chars of the data portion of a table.
# The complete table will be larger than this, as it also contains a record
# of size information, possible header parameters, and possible column
# definitions.
# The size returned is the space used, not allocated.  If row and/or column
# selectors are used, the size will be appropriately reduced.

int procedure tbtszd (tp)

pointer tp		# i: pointer to table descriptor
#--
int	nrows		# number of selected rows
int	rowlen		# row length (selected columns) in chars
int	tbrlen()

begin
	if (TB_ROW_SELECT(tp) == YES)
	    nrows = TB_NSEL_ROWS(tp)	# row selection is in effect
	else
	    nrows = TB_NROWS(tp)

	rowlen = tbrlen (tp, TBL_ROWLEN_USED)

	return (nrows * rowlen)
end

# tbrlen -- get row length
# This function returns the row length in units of SZ_CHAR.
# If a column selector was specified, the row length includes only the
# selected columns, and it is independent of the param argument.
# If no column selector is in effect, the row length returned will be
# either the allocated length or used length, depending on whether the
# param argument is TBL_ROWLEN or TBL_ROWLEN_USED respectively.
# (Values are returned in units of SZ_CHAR regardless of the _CHAR
# suffix in this routine.)

int procedure tbrlen (tp, param)

pointer tp		# i: pointer to table descriptor
int	param		# i: either TBL_ROWLEN or TBL_ROWLEN_USED
#--
pointer cp
int	value
int	colnum, tcs_column()

begin
	if (TB_COLUMN_SELECT(tp) == YES) {

	    # Column selection is in effect.
	    value = 0
	    do colnum = 1, TB_NSEL_COLS(tp) {
		cp = tcs_column (TB_SELCOL(tp,colnum))
		value = value + COL_LEN(cp)
	    }

	} else {

	    if (param == TBL_ROWLEN || param == TBL_ROWLEN_CHAR)
		value = TB_ROWLEN(tp)
	    else if (param == TBL_ROWLEN_USED || param == TBL_ROWLEN_CHAR_USED)
		value = TB_COLUSED(tp)
	    else
		value = 0

	}

	return (value)
end
