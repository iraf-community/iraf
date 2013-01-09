include <mach.h>
include <tbset.h>
include "tbtables.h"
define	SZ_PACKED_REC	(SZ_PARREC/SZB_CHAR)	# size of packed par record

# tbtscd -- copy to change size; column descriptors
# This routine copies the column-descriptor portion of one table to another.
# Old_ncols is the number of columns defined in the input table.
# This is called by either tbxscp or tbyscp.
#
# Phil Hodge,  1-Apr-1993  Change cbuf from TY_STRUCT to TY_CHAR.
# Phil Hodge, 14-Apr-1998  Change SZ_COLSTRUCT to SZ_COLDEF.

procedure tbtscd (tp, oldfd, newfd, old_maxpar, old_ncols)

pointer tp			# i: pointer to table descriptor
int	oldfd, newfd		# i: channel numbers for input & output tables
int	old_maxpar		# i: previous maximum number of user parameters
int	old_ncols		# i: previous number of columns
#--
pointer sp
pointer cbuf			# column-descriptor buffer
long	oldoff, newoff		# offsets from start of old & new files
int	cbufsiz			# size of buffer pointed to by cbuf
int	k			# loop index
int	stat
char	zero
int	read()
errchk	seek, read, write

begin
	# Create buffer for I/O
	call smark (sp)
	cbufsiz = SZ_COLDEF				# unit = SZ_CHAR
	call salloc (cbuf, cbufsiz, TY_CHAR)

	# Copy each column descriptor to the temporary file.
	oldoff = SZ_SIZINFO + old_maxpar * SZ_PACKED_REC + 1
	newoff = SZ_SIZINFO + TB_MAXPAR(tp) * SZ_PACKED_REC + 1
	do k = 1, old_ncols {
	    call seek (oldfd, oldoff)
	    call seek (newfd, newoff)
	    stat = read (oldfd, Memc[cbuf], SZ_COLDEF)
	    call write (newfd, Memc[cbuf], SZ_COLDEF)
	    oldoff = oldoff + SZ_COLDEF
	    newoff = newoff + SZ_COLDEF
	}
	# Fill out the rest of the space for column descriptors.
	zero = 0
	call amovkc (zero, Memc[cbuf], SZ_COLDEF)
	do k = old_ncols+1, TB_MAXCOLS(tp) {
	    call seek (newfd, newoff)
	    call write (newfd, Memc[cbuf], SZ_COLDEF)
	    newoff = newoff + SZ_COLDEF
	}

	call sfree (sp)
end
