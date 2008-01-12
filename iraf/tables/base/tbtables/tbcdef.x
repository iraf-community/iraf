include <error.h>
include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# tbcdef -- define columns
# Define a set of columns for a table.  If the table is new then it does
# not need to be open. The names in the array colname will be checked
# against the columns that have already been defined in the table, but
# the calling routine should make sure there are not any duplicate names
# within colname itself.
#
# The values in the DATATYPE array may be TY_BOOL, TY_INT, TY_SHORT,
# TY_REAL, TY_DOUBLE; for a character-type column the value should be
# a negative integer, the absolute value of which is the maximum number
# of characters for that column.
#
# If this procedure increases the space for column-descriptor pointers, the
# new size will be made DEFMAXCOLS larger than required to hold all those that
# that be defined when this routine returns.
#
# This routine may reallocate the array of pointers to column descriptors,
# and the values of TB_FILE(tp), TB_ROWLEN(tp) and TB_MAXCOLS(tp) may also
# be changed.
#
# The LENARRAY argument is the array size, or one for a scalar column.
#
# Phil Hodge, 16-Jan-1989  Flush the buffer after defining the column.
# Phil Hodge,  7-Mar-1989  Eliminate TB_MODSIZE.
# Phil Hodge,  1-May-1989  Change calling sequences of tbxwnc, tbywnc.
# Phil Hodge, 14-Jan-1992  Add option for text table type;
#			call tbtchs here instead of in tbcadd.
# Phil Hodge, 29-Jul-1994  Change lendata to lenarray, and use it.
# Phil Hodge,  3-Apr-1995  Set TB_MODIFIED to true.
# Phil Hodge, 20-Jun-1995  Modify for FITS tables.
# Phil Hodge,  2-Mar-1998  Call tbscol to allow a column selector.
# Phil Hodge, 12-Sep-2000  Initialize TB_INDEF_IS_CURRENT to false.

procedure tbcdef (tp, colptr,
	colname, colunits, colfmt, datatype, lenarray, numcols)

pointer tp				# i: pointer to table descriptor
pointer colptr[numcols]			# o: pointers to new columns
char	colname[SZ_COLNAME,numcols]	# i: names of columns
char	colunits[SZ_COLUNITS,numcols]	# i: units for columns
char	colfmt[SZ_COLFMT,numcols]	# i: print formats for columns
int	datatype[numcols]		# i: data types of columns
int	lenarray[numcols]		# i: number of elements for each column
int	numcols				# i: number of columns to be defined
#--
int	ntotal			# number of columns including new ones
int	old_ncols   		# TB_NCOLS before adding new columns
int	old_colused		# TB_COLUSED before increasing size
int	new_maxcols		# new maximum number of columns
int	new_colused		# new value for row length used
int	new_rowlen		# new value for row length allocated
int	dtype			# SPP data type of column
int	dlen			# number of char used by a column in table
int	larray			# max (lenarray, 1)
int	k			# Loop index
errchk	tbbaln, tbcadd, tbfdef, tbtchs, tbtflu

begin
	if (TB_READONLY(tp))
	    call error (ER_TBREADONLY,
		"can't define new columns in readonly table")

	# Save current values (in case table is open).
	old_ncols   = TB_NCOLS(tp)
	old_colused = TB_COLUSED(tp)

	# Check whether columns already exist.
	call tbcfnd (tp, colname, colptr, numcols)
	do k = 1, numcols {
	    if (colptr[k] != NULL) {
		call eprintf ("Warning:  column `%s' already exists.\n")
		    call pargstr (colname[1,k])
	    }
	}

	# Make sure we're not trying to create array columns for
	# a table type that doesn't support it.
	if (TB_TYPE(tp) != TBL_TYPE_S_ROW && TB_TYPE(tp) != TBL_TYPE_FITS) {
	    do k = 1, numcols {
		if (lenarray[k] > 1)
		    call error (1, "This table type doesn't support arrays.")
	    }
	}

	# Get the new total number of columns.
	ntotal = TB_NCOLS(tp) + numcols
	if (ntotal > TB_MAXCOLS(tp))
	    new_maxcols = ntotal + DEFMAXCOLS
	else
	    new_maxcols = -1		# i.e. don't change current value

	# Get the new row length used (actually needed only if row ordered).
	new_colused = TB_COLUSED(tp)
	do k = 1, numcols {
	    call tbbaln (datatype[k], dtype, dlen)
	    larray = max (1, lenarray[k])
	    new_colused = new_colused + dlen * larray
	}
	new_rowlen = max (TB_ROWLEN(tp), new_colused)

	# Update the values in the table struct and, if the table is open,
	# update the table size.
	call tbtchs (tp, -1, new_maxcols, new_rowlen, -1)

	# Create descriptors for the new columns.
	# For a text table, allocate memory for the column values.
	call tbcadd (tp, colptr, colname, colunits, colfmt,
			datatype, lenarray, numcols)

	if (TB_IS_OPEN(tp)) {

	    # If a column selector is in effect, add all new columns.
	    if (TB_COLUMN_SELECT(tp) == YES) {
		do k = 1, numcols
		    call tbscol (tp, colptr[k])
	    }

	    # Save new descriptors in table.
	    if (TB_TYPE(tp) == TBL_TYPE_S_ROW) {
		iferr {
		    call tbxwnc (tp, colptr, numcols, old_colused)
		} then {
		    TB_NCOLS(tp) = old_ncols
		    TB_COLUSED(tp) = old_colused
		    call erract (EA_ERROR)
		}
	    } else if (TB_TYPE(tp) == TBL_TYPE_S_COL) {
		iferr {
		    call tbywnc (tp, colptr, numcols)
		} then {
		    TB_NCOLS(tp) = old_ncols
		    TB_COLUSED(tp) = old_colused
		    call erract (EA_ERROR)
		}
	    } else if (TB_TYPE(tp) == TBL_TYPE_TEXT) {
		;
	    } else if (TB_TYPE(tp) == TBL_TYPE_FITS) {
		iferr {
		    do k = 1, numcols
			call tbfdef (tp, colptr[k])
		} then {
		    TB_NCOLS(tp) = old_ncols
		    TB_COLUSED(tp) = old_colused
		    call erract (EA_ERROR)
		}
	    } else {
		TB_NCOLS(tp) = old_ncols
		TB_COLUSED(tp) = old_colused
		call error (ER_TBCORRUPTED, "tbcdef:  table type is messed up")
	    }

	    call tbtflu (tp)		# flush the buffer
	}

	TB_MODIFIED(tp) = true
	TB_INDEF_IS_CURRENT(tp) = false
end
