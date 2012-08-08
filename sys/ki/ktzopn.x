# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<mach.h>
include	"ki.h"

# KT_ZOPN -- Open a text device.  We are called only if the device does not
# reside on the local node.

procedure kt_zopn (device, osfn, mode, chan)

int	device			# device driver code
char	osfn[ARB]		# packed os filename
int	mode			# access mode
int	chan			# receives assigned channel

pointer	bd, bp
int	server
int	ki_connect(), ki_sendrcv(), ki_getchan(), kmalloc()
include	"kichan.com"
include	"kii.com"

begin
	server = ki_connect (osfn)
	p_arg[2] = mode

	if (ki_sendrcv (server, device, TX_OPN) == ERR)
	    chan = ERR
	else if (p_arg[1] == ERR)
	    chan = ERR
	else if (kmalloc (bd, LEN_TXBDES, TY_STRUCT) == ERR)
	    chan = ERR
	else {
	    chan = ki_getchan (server, p_arg[1])
	    k_bufp[chan] = NULL

	    # Init the text buffer and buffer descriptor.  Text i/o over the
	    # KI is buffered, greatly reducing the number of packets sent back
	    # and forth between nodes and likewise increasing the bandwidth.
	    # Buffering at the KI layer is necessary because FIO buffers only
	    # one line of text at a time, leaving blocking and deblocking of
	    # text lines to the kernel.

	    k_bufp[chan] = bd
	    bp = B_BUFPTR(bd)
	    B_RP(bd)     = bp
	    B_CI(bd)     = 0
	    B_ITOP(bd)   = bp
	    B_OTOP(bd)   = bp
	    B_BUFTOP(bd) = bp + SZ_TXBUF
	}
end
