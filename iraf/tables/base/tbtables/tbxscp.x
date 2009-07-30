include "tbtables.h"

# tbxscp -- X copy to change size
# This routine copies the contents of one table to another for the purpose
# of changing the size of the user-parameter space, the space for column
# descriptors, and/or the row or column length of the table itself.
# Old_maxpar, etc describe the characteristics of the input file,
# while TB_MAXPAR(tp), etc describe the output file.
# This is called by tbxsiz.
#
# Phil Hodge, 19-Jan-1990  Replace TB_ROWLEN(tp) by TB_ROWLEN(tp)/SZ_REAL
#			in call to amovr.
# Phil Hodge, 30-Mar-1993  TB_INDEF is now TY_CHAR rather than TY_REAL.

procedure tbxscp (tp, oldfd, newfd, old_maxpar,
		old_maxcols, old_ncols, old_rowlen, old_colused)

pointer tp		# i: pointer to table descriptor
int	oldfd, newfd	# i: channel numbers for input & output tables
int	old_maxpar	# i: previous maximum number of user parameters
int	old_maxcols	# i: previous value for maximum number of columns
int	old_ncols	# i: previous number of columns
long	old_rowlen	# i: row length (=record length) in original table
int	old_colused	# i: previous number of char used in row
#--
pointer sp
pointer sbuf			# size info buffer
pointer rbuf			# buffer for copying table (row buffer)
char	zero			# for amovkc
long	oldoff, newoff		# offsets from start of old & new files
size_t	sbufsiz			# size of buffer pointed to by sbuf
size_t	rbufsiz			# size of buffer pointed to by rbuf
long	k			# loop index
long	stat
size_t	sz_val
long	tbtbod(), read()
errchk	seek, read, write

begin
	# Create buffers for I/O
	call smark (sp)
	sbufsiz = SZ_SIZINFO				# unit = SZ_CHAR
	rbufsiz = max (TB_ROWLEN(tp), old_rowlen)	# unit = SZ_CHAR
	call salloc (sbuf, sbufsiz, TY_CHAR)
	call salloc (rbuf, rbufsiz, TY_CHAR)

	# Copy the indef record to the row buffer.
	# bug fix 1/19/90 PEH; changed to ty_char 3/30/93 PEH
	call amovc (Memc[TB_INDEF(tp)], Memc[rbuf], TB_ROWLEN(tp))

	# Write dummy size info record.
	zero = 0
	sz_val = SZ_SIZINFO
	call amovkc (zero, Memc[sbuf], sz_val)
	newoff = 1
	call seek (newfd, newoff)
	sz_val = SZ_SIZINFO
	call write (newfd, Memc[sbuf], sz_val)

	# Copy each user parameter to the temporary file.
	call tbtscu (tp, oldfd, newfd, old_maxpar)

	# Copy each column descriptor to the temporary file.
	call tbtscd (tp, oldfd, newfd, old_maxpar, old_ncols)

	# Copy each row of the table to the temporary file.
	# Note that only old_colused char of input record are read.
	oldoff = tbtbod (old_maxpar, old_maxcols)
	newoff = tbtbod (TB_MAXPAR(tp), TB_MAXCOLS(tp))
	do k = 1, TB_NROWS(tp) {
	    call seek (oldfd, oldoff)
	    call seek (newfd, newoff)
	    sz_val = old_colused
	    stat = read (oldfd, Memc[rbuf], sz_val)
	    sz_val = TB_ROWLEN(tp)
	    call write (newfd, Memc[rbuf], sz_val)
	    oldoff = oldoff + old_rowlen
	    newoff = newoff + TB_ROWLEN(tp)
	}
	call sfree (sp)
end
