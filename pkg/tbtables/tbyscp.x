include "tbtables.h"

# tbyscp -- Y copy to change size
# This routine copies the contents of one table to another for the purpose
# of changing the size of the user-parameter space, the space for column
# descriptors, and/or the row or column length of the table itself.
# Old_maxpar, etc describe the characteristics of the input file,
# while TB_MAXPAR(tp), etc describe the output file.
# This is called by tbysiz.

procedure tbyscp (tp, oldfd, newfd, old_maxpar,
		old_maxcols, old_ncols, old_allrows)

pointer tp			# Pointer to table descriptor
int	oldfd, newfd		# Channel numbers for input & output tables
int	old_maxpar		# Previous maximum number of user parameters
int	old_maxcols		# Previous value for maximum number of columns
int	old_ncols		# Previous number of columns
int	old_allrows		# Previous number of allocated rows

pointer cp			# pointer to a column descriptor
pointer sp
pointer sbuf			# size info buffer
pointer dbuf			# buffer for copying table (data buffer)
pointer extrabuf		# buffer for filling out rest of column
long	oldoff, newoff		# offsets from start of old & new files
int	new_allrows		# = TB_ALLROWS(tp)
int	sbufsiz			# size of buffer pointed to by sbuf
int	dbufsiz			# size of buffer pointed to by dbuf
int	extrasiz		# size of buffer pointer to by extrabuf
int	j, k			# loop indexes
int	dlen			# number of char in an element
int	stat
pointer tbcnum()
long	tbtbod()
int	read()
errchk	seek, read, write

begin
	new_allrows = TB_ALLROWS(tp)

	# Create buffers for I/O
	call smark (sp)
	sbufsiz = LEN_SIZINFO				# unit = SZ_INT32
	dbufsiz = min (new_allrows, old_allrows)	# unit = SZ_CHAR
	extrasiz = new_allrows - old_allrows
	call salloc (sbuf, sbufsiz, TY_INT)
	call salloc (dbuf, dbufsiz, TY_CHAR)
	if (new_allrows > old_allrows)
	    call salloc (extrabuf, extrasiz, TY_CHAR)

	# Write dummy size info record.
	call amovki (0, Memi[sbuf], LEN_SIZINFO)
	newoff = 1
	call seek (newfd, newoff)
        if (SZ_INT != SZ_INT32)
             call ipak32 (Memi[sbuf], Memi[sbuf], SZ_SIZINFO)
	call write (newfd, Memi[sbuf], SZ_SIZINFO)

	# Copy each user parameter to the temporary file.
	call tbtscu (tp, oldfd, newfd, old_maxpar)

	# Copy each column descriptor to the temporary file.
	call tbtscd (tp, oldfd, newfd, old_maxpar, old_ncols)

	# Copy each column of the table to the temporary file.
	oldoff = tbtbod (old_maxpar, old_maxcols)
	newoff = tbtbod (TB_MAXPAR(tp), TB_MAXCOLS(tp))
	do k = 1, old_ncols {
	    cp = tbcnum (tp, k)
	    dlen = COL_LEN(cp)
	    do j = 1, dlen {
		call seek (oldfd, oldoff)
		call seek (newfd, newoff)
		stat = read (oldfd, Memc[dbuf], dbufsiz)
		call write (newfd, Memc[dbuf], dbufsiz)
		oldoff = oldoff + dbufsiz
		newoff = newoff + dbufsiz
	    }
	    # Fill out the rest of the current column with dummy values.
	    if (new_allrows > old_allrows) {
		do j = 1, dlen {
		    call seek (newfd, newoff)
		    call write (newfd, Memc[extrabuf], extrasiz)
		    newoff = newoff + extrasiz
		}
	    }
	}

	call sfree (sp)
end
