# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	"ki.h"

# KB_ZARD -- Asynchronous read from a binary device.  We are called only if
# the device does not reside on the local node.  In this implementation
# these reads are not actually asynchronous.  This is possible but more
# complex to implement than I wanted to get into at this time, particularly
# since FIO is not yet fully asynchronous.  The problem is that a node
# channel may be used to multiplex any number of requests to the remote
# node process.  If an asynchronous read is pending this must be detected
# and the read completed before processing any other requests.

procedure kb_zard (device, chan, obuf, max_bytes, loffset)

int	device			# device driver code
int	chan			# channel assigned device
char	obuf[max_bytes]		# receives data
int	max_bytes		# max bytes to read
long	loffset			# file offset

int	server, status
int	ki_send(), ki_receive()
include	"kichan.com"
include	"kii.com"

begin
	server = k_node[chan]

	if (max_bytes <= 0) {
	    k_status[chan] = 0
	    return
	}

	# Send the request to initiate the read.

	p_arg[1] = k_oschan[chan]
	p_arg[2] = max_bytes
	p_arg[3] = loffset

	if (ki_send (server, device, BF_ARD) == ERR) {
	    status = ERR
	} else {
	    # Wait for the ZAWT packet.
	    if (ki_receive (server, device, BF_AWT) == ERR)
		status = ERR
	    else
		status = p_arg[1]

	    # Read the data block (if any) directly into caller's buffer.
	    if (status > 0) {
		call ks_aread (server, obuf, status)
		call ks_await (server, status)
	    }
	}

	k_status[chan] = status
end
