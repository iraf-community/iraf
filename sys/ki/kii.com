# KI common -- Contains both the packed and unpacked packet used to transmit
# and receive requests over the network.  Since the packed packet is first
# in the common any overflow due to failure of the assumptions about the
# size of the packed packet in host ints will only damage the area used by
# the unpacked packet, causing no harm.

int	p_packet[SZB_PACKET/SZB_CHAR/SZ_MII_INT]	# packed packet

int	p_opcode		# instruction opcode
int	p_subcode		# subcode, if device driver
int	p_arg[MAX_ARGS]		# procedure arguments
int	p_sbuflen		# nchars in use in string buffer
char	p_sbuf[SZ_SBUF]		# string buffer

common	/kiicom/ p_packet, p_opcode, p_subcode, p_arg, p_sbuflen, p_sbuf
