# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<ctype.h>
include	"ki.h"

# KI_RECEIVE -- Read a machine independent KII packet from the network
# interface and decode it into the internal, machine dependent form in the
# kii common.  An error status is returned if the opcode and subcode in
# the packet do not match those expected, or if an i/o error occurs on the
# channel.

int procedure ki_receive (server, opcode, subcode)

int	server			# node index of server process
int	opcode			# function opcode
int	subcode			# function subcode (for drivers)

int	stat, ip, op, ch
int	gstrcpy()
include	"kii.com"
include	"kinode.com"

begin
	# Read the packet.
	call ks_aread (server, p_packet, SZB_PACKET)
	call ks_await (server, stat)

	# Hard error on the channel to the kernel server.
	if (stat == ERR)
	    return (ERR)

	# The encoded packet consists of LEN_INTFIELDS 32 bit MII integers
	# followed by p_sbuflen chars, one char per byte.

	call miiupk32 (p_packet, FIRSTINTFIELD, LEN_INTFIELDS, TY_INT)
	call chrupk (p_packet, LEN_INTFIELDS * 4 + 1, p_sbuf, 1,
	    max(0, min(SZ_SBUF, p_sbuflen)) + 1)

	# Check for out of band data, i.e., the data read was not a packet
	# but some unsolicited message, e.g., error message, from the
	# called program.  If this happens, print the error message and
	# return an error status.

	if (stat != SZB_PACKET || p_opcode != opcode || p_subcode != subcode) {

	    # Is it a printable string?  If so, print the message in the
	    # format "node: message".

	    op = gstrcpy (n_alias[1,1,server], p_sbuf, SZ_ALIAS) + 1
	    p_sbuf[op] = ':'
	    op = op + 1
	    p_sbuf[op] = ' '
	    op = op + 1
	    call chrupk (p_packet, 1, p_sbuf, op, SZ_LINE)

	    do ip = op, SZ_LINE {
		ch = p_sbuf[ip]
		if (ch == EOS)
		    break
		else if (!IS_ASCII(ch))
		    call strcpy ("out of band data on ki channel\n", p_sbuf,
			SZ_LINE)
	    }

	    call xer_putline (STDERR, p_sbuf)
	    return (ERR)

	} else
	    return (OK)
end
