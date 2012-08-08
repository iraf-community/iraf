# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	"ki.h"

# KT_ZFLS -- Flush output to a text device.  We are called only if the device
# does not reside on the local node.

procedure kt_zfls (device, chan, status)

int	device			# device driver code
int	chan			# channel assigned device
int	status			# receives nchars written or ERR

int	server
int	ki_sendrcv()
include	"kichan.com"
include	"kii.com"

begin
	call ki_flushtx (device, chan, status)
	if (status == ERR)
	    return

	server   = k_node[chan]
	p_arg[1] = k_oschan[chan]

	if (ki_sendrcv (server, device, TX_FLS) == ERR)
	    status = ERR
	else
	    status = p_arg[1]
end
