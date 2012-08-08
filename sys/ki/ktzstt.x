# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	"ki.h"

# KT_ZSTT -- Get file status on a text device.  We are called only if the
# device does not reside on the local node.

procedure kt_zstt (device, chan, what, lvalue)

int	device			# device driver code
int	chan			# channel assigned device
int	what			# file parameter to be returned
long	lvalue			# receives the parameter value

int	server
int	ki_sendrcv()
long	ki_decode()
include	"kichan.com"
include	"kii.com"

begin
	server   = k_node[chan]
	p_arg[1] = k_oschan[chan]
	p_arg[2] = what

	if (ki_sendrcv (server, device, TX_STT) == ERR)
	    lvalue = ERR
	else
	    lvalue = ki_decode (p_sbuf, NCHARS_LONG)
end
