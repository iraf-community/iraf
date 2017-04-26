# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<fio.h>
include	<gki.h>

# GKI_FFLUSH -- Flush a graphics stream.  This does not issue the GKI_FLUSH
# graphics instruction to the graphics kernel, it merely flushes any buffered
# data in the output stream, and is a no-op in the case of an inline kernel.

procedure gki_fflush (fd)

int	fd			# output file

errchk	seek
include	"gki.com"

begin
	if (IS_SUBKERNEL(fd)) {
	    call seek (fd, BOFL)
	    call zcall3 (gk_prpsio, KERNEL_PID(fd), fd, FF_WRITE)
	} else if (!IS_INLINE(fd))
	    call flush (gk_fd[fd])
end
