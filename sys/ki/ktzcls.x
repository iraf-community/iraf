# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	"ki.h"

# KT_ZCLS -- Close a text device.  We are called only if the device does not
# reside on the local node.

procedure kt_zcls (device, chan, status)

int	device			# device driver code
int	chan			# channel assigned device
int	status			# receives ok|err

int	server
int	ki_sendrcv()
include	"kichan.com"
include	"kii.com"

begin
	server   = k_node[chan]
	p_arg[1] = k_oschan[chan]

	# If we receive error on the KS channel when trying to close a file,
	# it is most likely due to a previous i/o error on the channel.  Do
	# not return error here because we are probably being called during
	# error recovery to free the logical channel, and if we return error
	# the real error will be hidden.

	if (ki_sendrcv (server, device, TX_CLS) == ERR)
	    status = OK
	else
	    status = p_arg[1]

	call mfree (k_bufp[chan], TY_STRUCT)
	call ki_freechan (chan)
end
