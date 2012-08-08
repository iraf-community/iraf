# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	<fio.h>
include	"ki.h"

# KB_ZSTT -- Get file status on a binary device.  We are called only if the
# device does not reside on the local node.

procedure kb_zstt (device, chan, what, lvalue)

int	device			# device driver code
int	chan			# channel assigned device
int	what			# file parameter to be returned
long	lvalue			# receives the parameter value

int	server, ks_maxbufsize
int	ki_sendrcv()
include	"kichan.com"
include	"kinode.com"
include	"kii.com"
data	ks_maxbufsize /-1/

begin
	server   = k_node[chan]
	p_arg[1] = k_oschan[chan]
	p_arg[2] = what

	if (ki_sendrcv (server, device, BF_STT) == ERR)
	    lvalue = ERR
	else {
	    lvalue = p_arg[1]

	    # The maximum buffer (transfer) size for a device is determined
	    # by the the network interface or by the device, whichever is
	    # smaller.

	    if (what == FSTT_MAXBUFSIZE || what == FSTT_OPTBUFSIZE) {
		if (ks_maxbufsize < 0)
		    call zsttks (n_kschan[server], FSTT_MAXBUFSIZE, ks_maxbufsize)
		if (lvalue == 0)
		    lvalue = ks_maxbufsize
		else if (ks_maxbufsize > 0)
		    lvalue = min (lvalue, ks_maxbufsize)
	    }
	}
end
