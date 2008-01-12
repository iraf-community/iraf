# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"m75.h"

# ZZRDII -- Initiate an asynchronous read from the IIS.

procedure zzrdii (fcb, buf, nbytes, offset)

pointer	fcb			# pointer to channel descriptor
char	buf[ARB]		# output buffer
int	nbytes			# number of  bytes to read
long	offset			# not used for this device

begin
	call zardbf (FCB_CHAN(fcb), buf, nbytes, offset)
end
