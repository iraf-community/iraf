# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<syserr.h>
include	<fset.h>
include	<fio.h>
include	<gki.h>

# GKI_GETCELLARRAY -- Input a cell array (pixel array).
#
# BOI GKI_GETCELLARRAY L LL UR NC NL
#    
#        L(i)            9
#        LL(p)           coordinates of lower left corner of input area
#        UR(p)           coordinates of upper right corner of input area
#        NC(i)           number of columns in array
#        NL(i)           number of lines in array

procedure gki_getcellarray (fd, m, nx, ny, x1,y1, x2,y2)

int	fd			# output file
int	nx, ny			# number of columns and lines in M
short	m[nx,ny]		# output array
int	x1, y1			# lower left corner of window to be read
int	x2, y2			# upper right corner of window to be read

int	epa, nchars, npts
short	ca[GKI_CELLARRAY_LEN]
short	gki[GKI_GETCELLARRAY_LEN]
int	read()
data	gki[1] /BOI/, gki[2] /GKI_GETCELLARRAY/, gki[3] /GKI_GETCELLARRAY_LEN/
errchk	write, seek, flush, read
include	"gki.com"

begin
	# If the kernel is inline it will return the cell array value in the
	# graphics stream FIO buffer just as if the kernel were resident
	# in another process.  We rewind the buffer after the kernel writes
	# into it in preparation for the read below.

	if (IS_INLINE(fd)) {
	    call fseti (fd, F_CANCEL, OK)
	    epa = gk_dd[GKI_GETCELLARRAY]
	    if (epa != 0)
		call zcall6 (epa, nx,ny, x1,y1, x2,y2)
	    call seek (fd, BOFL)

	} else {
	    # Write get cell array instruction to the kernel.

	    gki[GKI_GETCELLARRAY_LL]   = x1
	    gki[GKI_GETCELLARRAY_LL+1] = y1
	    gki[GKI_GETCELLARRAY_UR]   = x2
	    gki[GKI_GETCELLARRAY_UR+1] = y2
	    gki[GKI_GETCELLARRAY_NC]   = nx
	    gki[GKI_GETCELLARRAY_NL]   = ny

	    call write (gk_fd[fd], gki, GKI_GETCELLARRAY_LEN)

	    # If the kernel is a subprocess we must call PR_PSIO to allow the
	    # kernel to read the instruction and return the cell array value.

	    if (IS_SUBKERNEL(fd)) {
		call seek (fd, BOFL)
		call zcall3 (gk_prpsio, KERNEL_PID(fd), fd, FF_READ)
		call seek (fd, BOFL)
	    } else
		call flush (gk_fd[fd])
	}

	# Read and decode the cell array value.

	nchars = GKI_CELLARRAY_LEN * SZ_SHORT
	if (read (fd, ca, nchars) < nchars) {
	    call syserr (SYS_GGCELL)
	} else if (ca[1] != BOI || ca[2] != GKI_CELLARRAY ||
	    ca[GKI_CELLARRAY_NP] <= 0) {
	    call syserr (SYS_GGCELL)
	} else {
	    npts = ca[GKI_CELLARRAY_NP]
	    nchars = min (nx * ny, npts) * SZ_SHORT
	    if (read (fd, m, nchars) < nchars)
		call syserr (SYS_GGCELL)
	}

	call fseti (fd, F_CANCEL, OK)
end
