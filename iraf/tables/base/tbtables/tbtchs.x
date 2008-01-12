include <error.h>
include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# tbtchs -- change table size
# This procedure changes the size of a table in order to change the
# allocated space for header keywords or column descriptors, or to
# change the allocated row length or column length.  If the table is
# open (and not readonly) then it will be physically reorganized by
# copying it to a temporary file, deleting the original table, and then
# renaming the temporary file to the name of the original table.  The
# value of tp is not changed.
#
# If an input value is negative then no change will be made to the
# corresponding table parameter.  If an input value is non-negative but
# less than the minimum reasonable value then the new value will be set
# to the minimum.
#
# If the table is open the size information record will be rewritten,
# even if nothing needs to be done with regard to changing the table size.
# (This applies only to stsdas format tables, not opened readonly.)
#
# If maxcols is not the same as the current value, the array of pointers
# to column descriptors will be reallocated.
# The value of tb_file might be changed if the table is open.
#
# Phil Hodge, 28-Aug-1987  Use zero to truncate to minimum allowed value.
# Phil Hodge,  8-Oct-1987  TB_COLPTR is of type TY_POINTER.
# Phil Hodge, 26-Apr-1989  Save and restore (if error) size info from tp.
# Phil Hodge, 14-Jan-1992  Add option for text table type.
# Phil Hodge, 30-Mar-1993  Minimum row length is now SZ_CHAR, not SZ_REAL.
# Phil Hodge,  3-Apr-1995  Set TB_MODIFIED to true.
# Phil Hodge, 20-Jun-1995  Modify for FITS tables.
# Phil Hodge,  9-Jul-1996  For FITS tables, realloc TB_COLPTR if required.
# Phil Hodge,  7-Jun-1999  Add old_maxpar to calling sequence of tbzsiz;
#	OK to call this routine for a readonly table (to realloc buffers).

procedure tbtchs (tp, maxpar, maxcols, rowlen, allrows)

pointer tp			# i: pointer to table descriptor
int	maxpar			# i: new value for max number of header keywords
int	maxcols			# i: new value for maximum number of columns
int	rowlen			# i: new value for row length
int	allrows			# i: new value of allocated number of rows
#--
pointer tp_save			# for saving size info from tp
int	new_maxpar		# New value of max number of header keywords
int	new_maxcols		# New value of maximum number of columns
int	new_rowlen		# New value of row length
int	new_allrows		# New value of allocated number of rows
int	old_maxpar		# Previous value of max number of keywords
int	old_maxcols		# Previous value of maximum number of columns
int	old_rowlen		# Previous value of row length
int	old_allrows		# Previous value of allocated number of rows
int	old_ncols		# Previous value of number of columns defined
int	old_colused		# Previous value of used portion of row
long	tbtbod()

errchk	realloc, tbtwsi

begin
	# Update the size info record and flush the file.
	if (TB_IS_OPEN(tp) && !TB_READONLY(tp) &&
	    (TB_TYPE(tp) == TBL_TYPE_S_ROW ||
	     TB_TYPE(tp) == TBL_TYPE_S_COL)) {
	    call tbtwsi (tp)
	    call flush (TB_FILE(tp))
	}

	# Space allocated for header keywords.
	if (maxpar >= 0)
	    new_maxpar = maxpar
	else
	    new_maxpar = TB_MAXPAR(tp)
	new_maxpar = max (new_maxpar, TB_NPAR(tp))

	# Space allocated for column descriptors.
	if (maxcols > 0)
	    new_maxcols = maxcols
	else if (maxcols == 0)
	    new_maxcols = 1
	else
	    new_maxcols = TB_MAXCOLS(tp)
	new_maxcols = max (1, new_maxcols, TB_NCOLS(tp))

	# Row length.
	if (rowlen > 0)
	    new_rowlen = rowlen
	else if (rowlen == 0)
	    new_rowlen = SZ_CHAR
	else
	    new_rowlen = TB_ROWLEN(tp)
	new_rowlen = max (SZ_CHAR, new_rowlen, TB_COLUSED(tp))

	if (allrows > 0)
	    new_allrows = allrows
	else if (allrows == 0)
	    new_allrows = 1
	else
	    new_allrows = TB_ALLROWS(tp)
	new_allrows = max (1, new_allrows, TB_NROWS(tp))

	# Save current values.
	old_maxpar  = TB_MAXPAR(tp)
	old_maxcols = TB_MAXCOLS(tp)
	old_rowlen  = TB_ROWLEN(tp)
	old_allrows = TB_ALLROWS(tp)
	old_ncols   = TB_NCOLS(tp)
	old_colused = TB_COLUSED(tp)

	if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    # Maxcols is the only parameter we might need to change.
	    new_maxpar = old_maxpar
	    new_rowlen = old_rowlen
	    new_allrows = old_allrows
	}

	# Quit now if we really don't need to do anything.
	if ((old_maxpar == new_maxpar) && (old_maxcols == new_maxcols) &&
	    (old_rowlen == new_rowlen) && (old_allrows == new_allrows))
	    return

	# Save the size info from tp.  If there is an error rewriting the
	# table we must restore this info back into tp; otherwise, we just
	# free the memory pointed to by tp_save.
	call tbdsav (tp, tp_save)

	# Reallocate the array of pointers to column descriptors, and
	# assign new values in the table descriptor.
	if (new_maxcols != TB_MAXCOLS(tp)) {
	    call realloc (TB_COLPTR(tp), new_maxcols, TY_POINTER)
	    TB_MAXCOLS(tp) = new_maxcols
	}
	TB_MAXPAR(tp)  = new_maxpar
	TB_ROWLEN(tp)  = new_rowlen 
	TB_ALLROWS(tp) = new_allrows
	TB_BOD(tp) = tbtbod (new_maxpar, new_maxcols)

	if (TB_IS_OPEN(tp)) {

	    # This is OK even for a readonly file; it just reallocates memory.
	    if (TB_TYPE(tp) == TBL_TYPE_TEXT) {
		iferr {
		    call tbzsiz (tp, old_maxpar, old_allrows)
		} then {
		    call tbdres (tp, tp_save)
		    call erract (EA_ERROR)
		}
	    }

	    if (!TB_READONLY(tp)) {
		# Reorganize the data file.
		if (TB_TYPE(tp) == TBL_TYPE_S_ROW) {
		    iferr {
			call tbxsiz (tp, old_maxpar, old_maxcols,
			    old_ncols, old_rowlen, old_colused)
		    } then {
			call tbdres (tp, tp_save)	# restore size info
			call erract (EA_ERROR)
		    }
		    TB_MODIFIED(tp) = true
		} else if (TB_TYPE(tp) == TBL_TYPE_S_COL) {
		    iferr {
			call tbysiz (tp, old_maxpar, old_maxcols,
			    old_ncols, old_allrows)
		    } then {
			call tbdres (tp, tp_save)
			call erract (EA_ERROR)
		    }
		    TB_MODIFIED(tp) = true
		}
	    }
	}
	call tbdfre (tp_save)
end
