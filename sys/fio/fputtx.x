# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<fio.h>

# FPUTTX -- Put a line to a text file.  Flush output if flag is set.
# Called by FLSBUF, SEEK, FLUSH, etc. to flush an output line.

procedure fputtx (fd, buf, nchars, status)

int	fd, nchars, status
char	buf[ARB]
int	and()
include	<fio.com>

begin
	fp = fiodes[fd]
	call zcall4 (ZPUTTX(fp), FCHAN(fp), buf, nchars, status)

	if (status != ERR && and (FF_FLUSHNL, fflags[fd]) != 0)
	    call zcall2 (ZFLSTX(fp), FCHAN(fp), status)
end
