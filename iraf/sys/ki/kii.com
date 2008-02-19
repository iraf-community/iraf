# KI common -- Contains both the packed and unpacked packet used to transmit
# and receive requests over the network.  Since the packed packet is first
# in the common any overflow due to failure of the assumptions about the
# size of the packed packet in host ints will only damage the area used by
# the unpacked packet, causing no harm.

# Unpacked
long	p_opcode		# instruction opcode
long	p_subcode		# subcode, if device driver
long	p_arg[MAX_ARGS]		# procedure arguments
long	p_sbuflen		# nchars in use in string buffer
char	p_sbuf[SZ_SBUF]		# string buffer

# SZB_PACKET: 384 bytes (mii64-packed)
char	p_packet[SZB_PACKET/SZB_CHAR]	# packed packet

common	/kiicom/ p_opcode, p_subcode, p_arg, p_sbuflen, p_sbuf, p_packet
