# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>
include	<config.h>
include	"mtio.h"

# ZAWTMT -- Wait for the last i/o transfer to complete, update tape position
# counters, return nbytes|status to caller.

procedure zawtmt (mtchan, status)

int	mtchan			#I i/o channel
int	status			#O status (nbytes transferred or ERR)

include	"mtio.com"

begin
	# The "sticky" EOF should not be necessary but is needed due to the
	# way FIO behaves when it hits EOF on a blocked file.  In some
	# circumstances (depends upon the file length) two reads are made and
	# if the second read does not return zero EOF will not be detected.

	if (MT_ATEOF(mtchan) == YES)
	    status = 0
	else {
	    call zzwtmt (MT_OSCHAN(mtchan), MT_DEVPOS(mtchan), status)
	    if (status == 0)
		MT_ATEOF(mtchan) = YES
	}
end
