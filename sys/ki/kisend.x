# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"ki.h"

# KI_SEND -- Encode the packet in the kii common in a machine independent form
# and send it over the network.

int procedure ki_send (server, opcode, subcode)

int	server			# node index of server process
int	opcode			# function opcode
int	subcode			# function subcode (for drivers)

int	status
include	"kii.com"

begin
	p_opcode  = opcode
	p_subcode = subcode

	# Encode the packet in machine independent form, i.e., LEN_INTFIELDS
	# 32 bit MII integers followed by p_sbuflen chars, one char per byte.

	call miipak32 (FIRSTINTFIELD, p_packet, LEN_INTFIELDS, TY_INT)
	call chrpak (p_sbuf, 1, p_packet, LEN_INTFIELDS * 4 + 1, p_sbuflen + 1)

	# Transmit the packet.
	call ks_awrite (server, p_packet, SZB_PACKET)
	call ks_await  (server, status)

	return (status)
end
