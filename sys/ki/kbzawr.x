# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	"ki.h"

# KB_ZAWR -- Asynchronous write to a binary device.  We are called only if
# the device does not reside on the local node.

procedure kb_zawr (device, chan, ibuf, nbytes, loffset)

int	device			# device driver code
int	chan			# channel assigned device
char	ibuf[nbytes]		# receives data
int	nbytes			# number of bytes to write
long	loffset			# file offset

int	server
int	ki_send()
include	"kichan.com"
include	"kii.com"

begin
	server = k_node[chan]

	if (nbytes <= 0) {
	    k_status[chan] = 0
	    return
	}

	# Send the request followed by the data block.  We must wait for the
	# write into the network channel to complete since the channel is
	# multiplexed.  Note that this is not the same as waiting for the
	# data transfer to the physical device to complete; that transfer is
	# at least partially asynchronous.

	p_arg[1] = k_oschan[chan]
	p_arg[2] = nbytes
	p_arg[3] = loffset

	if (ki_send (server, device, BF_AWR) == ERR)
	    k_status[chan] = ERR
	else {
	    call ks_awrite (server, ibuf, nbytes)
	    call ks_await  (server, k_status[chan])
	}
end
