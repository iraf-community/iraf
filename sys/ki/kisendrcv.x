# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# KI_SENDRCV -- Encode and send a packet to a remote server process, then
# read and decode the response packet.  The opcode and subcode in the
# response packet must agree with those in the packet sent.

int procedure ki_sendrcv (server, opcode, subcode)

int	server			# os channel to server process
int	opcode			# function opcode
int	subcode			# function subcode (for drivers)

int	ki_send(), ki_receive()

begin
	if (ki_send (server, opcode, subcode) == ERR)
	    return (ERR)
	else
	    return (ki_receive (server, opcode, subcode))
end
