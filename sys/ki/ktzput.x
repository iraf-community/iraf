# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	"ki.h"

# KT_ZPUT -- Put a line of text to a text device.  We are called only if the
# device does not reside on the local node.  Output is buffered for greater
# bandwidth and efficiency.

procedure kt_zput (device, chan, ibuf, nchars, status)

int	device			# device driver code
int	chan			# channel assigned device
char	ibuf[nchars]		# receives text
int	nchars			# nchars to write
int	status			# receives nchars written or ERR

pointer	bd, ep
include	"kichan.com"

begin
	bd = k_bufp[chan]
	ep = B_RP(bd) + nchars - 1

	# If this is the first write into the buffer, OTOP will be set to
	# the beginning of the buffer and we must reset it to the end.

	if (ep >= B_OTOP(bd)) {
	    # Check whether there is room remaining in the buffer for the data.
	    # We do not break output lines across buffer boundaries.

	    if (ep >= B_BUFTOP(bd)) {
		call ki_flushtx (device, chan, status)
		if (status == ERR)
		    return

		# Check for buffer too small to hold data record.  This should
		# not be possible if SZ_TXBUF is chosen large enough, because
		# FIO will break records larger than the FIO buffer size.

		if (nchars > SZ_TXBUF) {
		    status = ERR
		    return
		}
	    }

	    B_OTOP(bd) = B_BUFTOP(bd)
	}

	# Append the text data to the output buffer.

	call amovc (ibuf, Memc[B_RP(bd)], nchars)
	B_RP(bd) = B_RP(bd) + nchars
	status = nchars
end


# KI_FLUSHTX -- Flush any buffered output of a text file.  Text output is
# transmitted as a stream without reading the status back after each write.
# If a write error occurs on the logical channel to the text file, the kernel
# server will shut down entirely, causing a write error on the physical
# channel to the kernel server.  It is harmless if we are called when the
# output buffer is empty or contains input data.

procedure ki_flushtx (device, chan, status)

int	device			# text file device code
int	chan			# channel assigned device
int	status			# receives nchars written or ERR

pointer	bd, bp
int	server, nchars
int	ki_send()
include	"kichan.com"
include	"kii.com"

begin
	bd = k_bufp[chan]
	bp = B_BUFPTR(bd)

	# OTOP will have been set to BUFTOP if the buffer was written into.
	# RP may be greater than BUFPTR for input buffers, too.  If there is
	# nothing to flush return without taking any action and without 
	# changing any file pointers.

	nchars = min (B_OTOP(bd), B_RP(bd)) - bp
	if (nchars <= 0) {
	    status = 0
	    return
	}

	server   = k_node[chan]
	p_arg[1] = k_oschan[chan]
	p_arg[2] = nchars

	# If the buffer is small enough it is sent in the string buffer,
	# otherwise it is sent as a second record.

	if (nchars <= SZ_SBUF) {
	    call amovc (Memc[bp], p_sbuf, nchars)
	    p_sbuflen = nchars
	} else
	    p_sbuflen = 0

	# Send packet.
	if (ki_send (server, device, TX_PUT) == ERR)
	    status = ERR
	else if (nchars > SZ_SBUF) {
	    # Send data record.

	    call chrpak (Memc[bp], 1, Memc[bp], 1, nchars)
	    call ks_awrite (server, Memc[bp], nchars)
	    call ks_await  (server, status)

	    if (status != nchars)
		status = ERR
	}

	# Mark the buffer empty.

	B_RP(bd)   = bp
	B_ITOP(bd) = bp
	B_OTOP(bd) = bp
end
