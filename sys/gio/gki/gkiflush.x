# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<fio.h>
include	<gki.h>

# GKI_FLUSH -- Flush any buffered output.
#
# BOI GKI_FLUSH 0
#    
#        L(i)            set to the constant 3 (no data fields)

procedure gki_flush (fd)

int	fd			# output file

int	epa
short	gki[GKI_FLUSH_LEN]
data	gki[1] /BOI/, gki[2] /GKI_FLUSH/, gki[3] /LEN_GKIHDR/
errchk	write, seek
include	"gki.com"

begin
	if (IS_INLINE(fd)) {
	    epa = gk_dd[GKI_FLUSH]
	    if (epa != 0)
		call zcall1 (epa, 0)
	} else {
	    call write (gk_fd[fd], gki, GKI_FLUSH_LEN * SZ_SHORT)

	    # If writing to a subkernel we must call PR_PSIO to give the
	    # kernel a chance to read the spooled metacode.

	    if (IS_SUBKERNEL(fd)) {
		call seek (fd, BOFL)
		call zcall3 (gk_prpsio, KERNEL_PID(fd), fd, FF_WRITE)
	    } else
		call flush (gk_fd[fd])
	}
end
