# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>
include	<config.h>
include	"mtio.h"

# ZARDMT -- MTIO asynchronous read primitive.  Initiate a read of up to
# maxbytes bytes into the user buffer.  Do nothing if we are already at
# EOF or EOT.

procedure zardmt (mtchan, buf, maxbytes, offset)

int	mtchan, maxbytes
char	buf[ARB]
long	offset
include	"mtio.com"

begin
	if (MT_ATEOF(mtchan) == NO && MT_ATEOT(mtchan) == NO)
	    call zzrdmt (MT_OSCHAN(mtchan), buf, maxbytes)
end
