# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<gki.h>

# GKI_WRITE -- Write a GKI metacode instruction to a graphics kernel.  If the
# kernel is inline the kernel is directly called to execute the instruction,
# otherwise the instruction is written into the graphics stream for the
# kernel.  This procedure is functionally equivalent to GKI_EXECUTE, but works
# for both inline and external kernels.

procedure gki_write (fd, gki)

int	fd			# graphics stream
short	gki[ARB]		# encoded instruction
int	length
include	"gki.com"

begin
	if (IS_INLINE(fd))
	    call gki_execute (gki, gk_dd)
	else {
	    length = gki[GKI_HDR_LENGTH]
	    call write (gk_fd[fd], gki, length * SZ_SHORT)
	}
end
