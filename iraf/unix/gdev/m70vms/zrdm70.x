# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<knet.h>
include	"m70.h"

# ZRDM70 -- Initiate an asynchronous read from the IIS.

procedure zrdm70 (chan, buf, nbytes, offset)

int	chan			# FCB pointer for device
char	buf[ARB]		# output buffer
int	nbytes			# number of  bytes to read
long	offset			# not used for this device

pointer	fcb
short	rwflag, opcd, nwords, ier
data	rwflag /IIS_READ/, opcd /EFN/

begin
	fcb = chan
	if (FCB_KCHAN(fcb) == NULL) {
	    nwords = nbytes / (SZ_SHORT * SZB_CHAR)
	    call m70io (Mems[fcb], buf, nwords, rwflag, opcd, FCB_IOSB(fcb,1),
		ier)

	    FCB_NBYTES(fcb) = nbytes
	    FCB_EFN(fcb)    = opcd

	    if (ier != 0)
		FCB_STATUS(fcb) = ERR
	    else
		FCB_STATUS(fcb) = IIS_READ
	} else
	    call zardbf (FCB_KCHAN(fcb), buf, nbytes, offset)
end
