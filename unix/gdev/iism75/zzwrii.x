# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"m75.h"

# ZZWRII -- Initiate an asynchronous write to the IIS.

procedure zzwrii (fcb, buf, nbytes, offset)

pointer	fcb			# pointer to channel descriptor
char	buf[ARB]		# input buffer
int	nbytes			# number of  bytes to write
long	offset			# not used for this device

begin
	call zawrbf (FCB_CHAN(fcb), buf, nbytes, offset)
end
