# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	"ki.h"

# KT_ZNOT -- Note the file position of a text device.  We are called only if
# the device does not reside on the local node.

procedure kt_znot (device, chan, loffset)

int	device			# device driver code
int	chan			# channel assigned device
long	loffset			# receives the file offset

pointer	bd, rp
int	server, status
int	ki_sendrcv()
long	ki_decode()
include	"kichan.com"
include	"kii.com"
define	physnote_ 91

begin
	bd = k_bufp[chan]

	# The buffering of text file input and output complicates the seek and
	# note functions.  Our solution is [1] for an output file, flush the
	# output and then call the kernel server to note the file position, and
	# [2] for an input file, have the kernel server note the offset of each
	# input line of text in the header area of each input record.

	if (B_ITOP(bd) >= B_BUFPTR(bd)) {
	    # Input file.

	    rp = B_RP(bd)

	    # Check for end of input buffer.
	    if (rp >= B_ITOP(bd))
		goto physnote_

	    # If already part way into line, return offset of the next line.
	    if (B_CI(bd) > 0) {
		rp = rp + ki_decode (R_RECLEN(rp), NCHARS_INT)
		if (rp >= B_ITOP(bd))
		    goto physnote_
	    }

	    # Decode seek offset from record header of next line to be
	    # read.  The seek offset is encoded as a char sequence.

	    loffset = ki_decode (R_SEKOFF(rp), NCHARS_LONG)
	    return

	} else {
	    # Output file.

	    call ki_flushtx (device, chan, status)
	    if (status == ERR)
		return
	}

physnote_

	# Physically call the kernel server to note the file offset.

	server   = k_node[chan]
	p_arg[1] = k_oschan[chan]

	if (ki_sendrcv (server, device, TX_NOT) == ERR)
	    loffset = ERR
	else
	    loffset = ki_decode (p_sbuf, NCHARS_LONG)
end
