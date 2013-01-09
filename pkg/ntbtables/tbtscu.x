include <mach.h>
include <tbset.h>
include "tbtables.h"
define	SZ_PACKED_REC	(SZ_PARREC/SZB_CHAR)	# size of packed par record

# tbtscu -- copy to change size; user parameters
# This routine copies the user-parameter portion of one table to another.
# Old_maxpar and TB_MAXPAR(tp) specify the sizes of the user-parameter areas
# in the input and output tables respectively.  Either of these may be the
# larger.
# This is called by either tbxscp or tbyscp.

procedure tbtscu (tp, oldfd, newfd, old_maxpar)

pointer tp			# i: pointer to table descriptor
int	oldfd, newfd		# i: channel numbers for input & output tables
int	old_maxpar		# i: previous maximum number of user parameters
#--
pointer sp
pointer pbuf			# buffer for user parameters
pointer blank			# buffer for extra (blank) user param records
long	oldoff, newoff		# offsets from start of old & new files
int	pbufsiz			# size of buffer pointed to by pbuf
int	k			# loop index
int	n_copy			# number of user-parameter records to copy
int	stat
int	read()
errchk	seek, read, write

begin
	# Create buffer for I/O
	call smark (sp)
	pbufsiz = SZ_PACKED_REC				# unit = SZ_CHAR
	call salloc (pbuf, pbufsiz, TY_CHAR)

	n_copy = min (old_maxpar, TB_MAXPAR(tp))

	# Copy each user parameter to the temporary file.
	oldoff = SZ_SIZINFO + 1			# initial values
	newoff = SZ_SIZINFO + 1
	do k = 1, n_copy {
	    call seek (oldfd, oldoff)
	    call seek (newfd, newoff)
	    stat = read (oldfd, Memc[pbuf], SZ_PACKED_REC)
	    call write (newfd, Memc[pbuf], SZ_PACKED_REC)
	    oldoff = oldoff + SZ_PACKED_REC
	    newoff = newoff + SZ_PACKED_REC
	}
	# Fill out the rest of the space (if any) for user parameters.
	if (TB_MAXPAR(tp) > n_copy) {
	    call salloc (blank, SZ_PARREC, TY_CHAR)
	    do k = 1, SZ_PARREC
		Memc[blank+k-1] = ' '
	    call strpak (Memc[blank], Memc[pbuf], SZ_PARREC)
	    do k = n_copy+1, TB_MAXPAR(tp) {
		call seek (newfd, newoff)
		call write (newfd, Memc[pbuf], SZ_PACKED_REC)
		newoff = newoff + SZ_PACKED_REC
	    }
	}

	call sfree (sp)
end
