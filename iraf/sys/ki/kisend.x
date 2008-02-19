# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"ki.h"

# KI_SEND -- Encode the packet in the kii common in a machine independent form
# and send it over the network.

int procedure ki_send (server, opcode, subcode)

int	server			# node index of server process
int	opcode			# function opcode
int	subcode			# function subcode (for drivers)

size_t	sz_val
size_t	sz_val1
size_t	sz_val2
long	status
include	"kii.com"

begin
	p_opcode  = opcode
	p_subcode = subcode

	# Encode the packet in machine independent form, i.e., LEN_LONGFIELDS
	# 64 bit MII integers followed by p_sbuflen chars, one char per byte.

	sz_val = LEN_LONGFIELDS
	call miipak64 (FIRSTLONGFIELD, p_packet, sz_val, TY_LONG)
	sz_val = p_sbuflen + 1
	sz_val1 = 1
	sz_val2 = LEN_LONGFIELDS * 8 + 1
	call chrpak (p_sbuf, sz_val1, p_packet, sz_val2, sz_val)

	# Transmit the packet.
	sz_val = SZB_PACKET
	call ks_awrite (server, p_packet, sz_val)
	call ks_await  (server, status)

	return (status)
end
