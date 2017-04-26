# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GBYTES -- Locally implemented bit unpacker for the NCAR extended metacode
# translator.   3 may 84 cliff stoll
# Required for the ncar/gks vdi metacode generator.
#
# Essentially this routine accepts an array which is a packed series of bits. 
# [array BUFIN], and unpacks them into an array [array BUFOUT].  Received
# integer INDEX is the beginning bit in BUFIN  where information is to be 
# placed.  INDEX is zero indexed.  Received integer argument SIZE is the 
# number of bits in each "information packet".  Received argument SKIP is the 
# number of bits to skip between bit packets.  For more info, see page 4 of 
# the NCAR "Implementaton details for the new metafile translator, version 1.0"

procedure gbytes (bufin, bufout, index, size, skip, count)

int	bufout[ARB], bufin[ARB], index, size, skip, count
int	pack
int	offset
int	bitupk()	# Iraf function to unpack bits

begin
	for (pack = 1; pack <= count ; pack = pack+1) {
	    # Offset is a bit offset into the input buffer bufin.
	    # (offset is 1- indexed; INDEX is zero indexed)

	    offset = (size + skip) * (pack - 1) + index + 1
	    bufout(pack) = bitupk(bufin, offset, size)
	}
end
