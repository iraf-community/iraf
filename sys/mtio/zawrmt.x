# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>
include	<config.h>
include	"mtio.h"

# ZAWRMT -- MTIO asynchronous write primitive.  Initiate a write of nbytes
# bytes to the tape.

procedure zawrmt (mtchan, buf, nbytes, offset)

int	mtchan, nbytes
char	buf[ARB]
long	offset
include	"mtio.com"

begin
	call zzwrmt (MT_OSCHAN(mtchan), buf, nbytes)
end
