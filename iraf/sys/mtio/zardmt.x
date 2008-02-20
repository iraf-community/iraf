# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>
include	<config.h>
include	"mtio.h"

# ZARDMT -- MTIO asynchronous read primitive.  Initiate a read of up to
# maxbytes bytes into the user buffer.

procedure zardmt (mtchan, buf, maxbytes, offset)

int	mtchan			#I i/o channel
char	buf[ARB]		#O output data buffer
size_t	maxbytes		#I max bytes to read
long	offset			#I file offset

int	chan
include	"mtio.com"

begin
	if (MT_ATEOF(mtchan) == NO) {
	    chan = MT_OSCHAN(mtchan)
	    call zzrdmt (chan, buf, maxbytes, offset)
	}
end
