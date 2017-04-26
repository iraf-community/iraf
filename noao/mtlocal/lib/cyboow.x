# CYBOOW, CYBOEW -- Order the bits in an odd or even indexed 60-bit Cyber word.
# The operation may not be performed in-place.  The offsets and sizes of the
# bit segments which must be moved are as follows:
#
#	 --> Odd Words <--	        --> Even Words <--
#       [from]   [to]  [nbits]
# 	 1	53	8		-3	57	4
# 	 9	45	8		5	49	8
# 	17	37	8		13	41	8
# 	25	29	8		21	33	8
# 	33	21	8		29	25	8
# 	41	13	8		37	17	8
# 	49	 5	8		45	 9	8
# 	61	 1	4		53	 1	8
#
# Input bit-offsets must be a multiple of the Cyber word size, i.e., 1, 61,
# 121, etc.  An output word may begin at any bit-offset.


# CYBOOW -- Order odd cyber word.  After swapping the first 8 bytes of IN the
# ordered 60-bit Cyber word is in bits 5-64 of the temporary storage area at W.

procedure cyboow (in, inbit, out, outbit)

int	in[ARB]
int	inbit
int	out[ARB]
int	outbit

begin
	call error (1, "Cyber readers have not been implemented")
end


# CYBOEW -- Order even cyber word.  After swapping the 8 bytes the ordered
# Cyber word will be found in bits 1-60 of the temporary storage area at W.

procedure cyboew (in, inbit, out, outbit)

int	in[ARB]
int	inbit
int	out[ARB]
int	outbit

begin
	call error (1, "Cyber readers have not been implemented")
end
