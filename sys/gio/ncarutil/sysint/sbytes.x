# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

# SBYTES -- Locally implemented bit packer for the NCAR extended metacode
# translator.   3 may 84 cliff stoll
# Required for the ncar/gks vdi metacode generator.
#
# Essentially this routine accepts an array of "information packets"
# [array BUFIN], and packs them into a packed array [array BUFOUT]
# received integer argument INDEX points to the beginning bit in BUFOUT
# where information is to be placed.  INDEX is zero indexed.
# received integer argument SIZE is the number of bits in each "information
# packet.  received argument SKIP is the number of bits to skip between
# bit packets.  For more info, see page 6 of the NCAR "Implementaton
# details for the new metafile translator, version 1.0"  
# bufin is stuffed into bufout

procedure sbytes (bufout, bufin, index, size, skip, count)

int	bufout[ARB], bufin[ARB], index, size, skip, count
int	metacode_word_length
int	pack
int	offset

data	metacode_word_length / 16 /

begin
	if (metacode_word_length != NBITS_SHORT) 
	    call error ( 0, " bad metacode word length in SBYTES")

	for (pack = 1; pack <= count; pack = pack + 1) {
	    # Offset is a bit offset into the output buffer bufout.
	    # (offset is 1- indexed; INDEX is zero indexed)
	    # see page 58 of IRAF system interface book

	    offset =  (size + skip) * (pack - 1) + index + 1
	    call bitpak (bufin[pack], bufout, offset, size)
	}
end
