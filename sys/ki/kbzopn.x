# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"ki.h"

# KB_ZOPN -- Open a binary device.  We are called only if the device does not
# reside on the local node.

procedure kb_zopn (device, osfn, mode, chan)

int	device			# device driver code
char	osfn[ARB]		# packed os filename
int	mode			# access mode
int	chan			# receives assigned channel

int	server
int	ki_connect(), ki_sendrcv(), ki_getchan()
include	"kii.com"

begin
	server = ki_connect (osfn)
	p_arg[2] = mode

	if (ki_sendrcv (server, device, BF_OPN) == ERR)
	    chan = ERR
	else if (p_arg[1] == ERR)
	    chan = ERR
	else
	    chan = ki_getchan (server, p_arg[1])
end
