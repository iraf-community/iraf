# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	"ki.h"

# KT_ZSEK -- Seek on a text device.  We are called only if the device does not
# reside on the local node.

procedure kt_zsek (device, chan, loffset, status)

int	device			# device driver code
int	chan			# channel assigned device
long	loffset			# znottx offset to seek to
int	status			# receives nchars written or ERR

pointer	bd
int	server
int	ki_sendrcv()
include	"kichan.com"
include	"kii.com"

begin
	call ki_flushtx (device, chan, status)
	if (status == ERR)
	    return

	# Discard any cached input text to force a buffer refill at the new
	# offset.

	bd = k_bufp[chan]
	B_RP(bd) = B_ITOP(bd)

	# Transmit the seek request to the server.

	server   = k_node[chan]
	p_arg[1] = k_oschan[chan]

	# The long integer seek offset is passed as an encoded char sequence
	# rather than as an integer p_arg value to avoid possible loss of
	# precision.

	call ki_encode (loffset, p_sbuf, NCHARS_LONG)
	p_sbuflen = NCHARS_LONG

	if (ki_sendrcv (server, device, TX_SEK) == ERR)
	    status = ERR
	else
	    status = p_arg[1]
end
