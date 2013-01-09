include <fset.h>
include "tbtables.h"

# tbysiz -- Y increase size
# Change the space for column descriptors and/or the allocated number
# of rows.  This is done by copying the table data file to a temporary
# file, deleting the original table, and then renaming the temporary file
# to the name of the original table.
# The table must be open when this procedure is called.
#
# NOTE that the file channel number TB_FILE(tp) may be changed by this
# procedure.  The values of TB_MAXCOLS, TB_ALLROWS and TB_COLUSED are
# assumed to have been updated by a calling routine (e.g. tbcdef).
#
# Phil Hodge, 28-Aug-1987  Flush file buffer after calling tbtwsi.
# Phil Hodge,  6-Nov-1987  Put temporary file in tmp.
# Phil Hodge, 24-Mar-1988  Save and restore FIO buffer size.
# Phil Hodge,  7-Mar-1989  Eliminate TB_CURROW.

procedure tbysiz (tp, old_maxpar, old_maxcols,
		old_ncols, old_allrows)

pointer tp			# i: pointer to table descriptor
int	old_maxpar		# i: previous value for max number of parmeters
int	old_maxcols		# i: previous value for max number of columns
int	old_ncols		# i: previous number of columns
int	old_allrows		# i: previous value of allocated number of rows

pointer sp
pointer colptr			# scratch for array of column pointers
int	k
int	new_allrows		# = TB_ALLROWS(tp)
int	iomode			# I/O mode for reopening the table
int	oldfd, newfd		# Channel numbers for old & new table files
int	bufsize			# save and restore FIO buffer size
char	temp_file[SZ_FNAME]	# Name of temporary file for table data
pointer tbcnum()
int	open(), fstati()
errchk	tbtwsi, tbyscp, mktemp, open, close, delete, rename, tbyscn, flush

begin
	# First check whether we really have to do anything.
	if ((TB_MAXPAR(tp) == old_maxpar) && (TB_MAXCOLS(tp) == old_maxcols) &&
	   ((TB_ALLROWS(tp) == old_allrows) || (old_ncols == 0)))
	    return

	oldfd = TB_FILE(tp)
	if (oldfd == NULL)
	    return
	bufsize = fstati (oldfd, F_BUFSIZE)

	new_allrows = TB_ALLROWS(tp)

	call mktemp ("tmp$tbl", temp_file, SZ_FNAME)
	newfd = open (temp_file, NEW_FILE, BINARY_FILE)

	# Copy the contents of the table to the temporary file.
	call tbyscp (tp, oldfd, newfd, old_maxpar,
		old_maxcols, old_ncols, old_allrows)

	# For each existing column, set all new rows to indef.
	if ((new_allrows > old_allrows) && (old_ncols > 0)) {
	    call smark (sp)
	    call salloc (colptr, old_ncols, TY_INT)
	    do k = 1, old_ncols
		Memi[colptr+k-1] = tbcnum (tp, k)
	    call tbyscn (tp, newfd, Memi[colptr], old_ncols,
			old_allrows+1, new_allrows)
	    call sfree (sp)
	}

	call close (oldfd)
	call close (newfd)

	# Delete the original table, and rename the temporary table to
	# the same name as the original.
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
