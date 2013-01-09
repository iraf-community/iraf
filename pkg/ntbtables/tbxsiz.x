include <fset.h>
include "tbtables.h"

# tbxsiz -- X increase size
# Increase either the record size or the space for column descriptors
# for a table.  If only the record size needs to be increased and no
# rows have yet been written to the table, this routine does nothing.
# Increasing the record size and/or space is done by copying the table
# data file to a temporary file, deleting the original table, and
# then renaming the temporary file to the name of the original table.
# The INDEF record buffer is copied to a local scratch buffer and then
# used for the I/O, so when this procedure returns, all existing rows should
# have the correct INDEF values set for the new columns.
# If the new row length is not the same as the previous value, the INDEF
# record buffer will be reallocated.
# The table must be open when this procedure is called.
#
# NOTE that the file channel number TB_FILE(tp) may be changed by this
# procedure.  The values of TB_ROWLEN, TB_MAXPAR, TB_MAXCOLS and TB_BOD
# are assumed to have been updated by a calling routine (e.g. tbcdef).
#
# Phil Hodge, 28-Aug-1987  Flush file buffer after calling tbtwsi.
# Phil Hodge,  6-Nov-1987  Put temporary file in tmp.
# Phil Hodge, 24-Mar-1988  Save and restore FIO buffer size.
# Phil Hodge,  7-Mar-1989  Eliminate TB_CURROW.
# Phil Hodge,  1-May-1989  Realloc the INDEF record buffer.
# Phil Hodge, 30-Mar-1993  TB_INDEF is now TY_CHAR rather than TY_REAL.

procedure tbxsiz (tp, old_maxpar, old_maxcols,
		old_ncols, old_rowlen, old_colused)

pointer tp			# i: pointer to table descriptor
int	old_maxpar		# i: previous max number of user parameters
int	old_maxcols		# i: previous value for max number of columns
int	old_ncols		# i: previous number of columns
int	old_rowlen		# i: row length (=record len) in original table
int	old_colused		# i: previous number of char used in row
#--
int	iomode			# I/O mode for reopening the table
int	oldfd, newfd		# channel numbers for old & new table files
int	bufsize			# save and restore FIO buffer size
char	temp_file[SZ_FNAME]	# name of temporary file for table data
int	open(), fstati()
errchk	realloc, tbtwsi, tbxscp, mktemp, open, close, delete, rename, flush

begin
	# Reallocate indef record if necessary.
	if (TB_ROWLEN(tp) > old_rowlen)
	    call realloc (TB_INDEF(tp), TB_ROWLEN(tp), TY_CHAR)

	# First check whether we really have to do anything.
	if ( (TB_NROWS(tp) <= 0) && (TB_MAXPAR(tp) == old_maxpar) &&
	     (TB_MAXCOLS(tp) == old_maxcols) )
	    return

	oldfd = TB_FILE(tp)
	bufsize = fstati (oldfd, F_BUFSIZE)

	call mktemp ("tmp$tbl", temp_file, SZ_FNAME)
	newfd = open (temp_file, NEW_FILE, BINARY_FILE)

	# Copy the contents of the table to the temporary file.
	call tbxscp (tp, oldfd, newfd, old_maxpar,
		old_maxcols, old_ncols, old_rowlen, old_colused)

	call close (oldfd)
	call close (newfd)

	# Delete the original table that was too small, and rename the
	# temporary table to the same name as the original.
	call delete (TB_NAME(tp))
	call rename (temp_file, TB_NAME(tp))

	# Reopen the new table.
	if (TB_IOMODE(tp) == NEW_FILE)
	    iomode = READ_WRITE
	else
	    iomode = TB_IOMODE(tp)
	TB_FILE(tp) = open (TB_NAME(tp), iomode, BINARY_FILE)

	# Restore whatever buffer size the old table had.
	call fseti (TB_FILE(tp), F_BUFSIZE, bufsize)

	# Update the size information record in the new table.
	call tbtwsi (tp)			# write size information
	call flush (TB_FILE(tp))
end
