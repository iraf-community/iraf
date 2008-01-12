include <imio.h>
include	<imhdr.h>
include <mach.h>
include "gi.h"

# GI_GEIS -- Return true if image is in STF format
#
# B.Simon	12-Jul-93	Extracted from gi_gfind
# B.Simon 	27-Jun-97	Modified to check other fields

bool procedure gi_geis (im)

pointer	im		# i: Image descriptor
#--
char	star
int	ic, nc, nbytes, bitpix
pointer	stf

data	star  / '*' /

int	stridx(), ctoi()

begin

	# Get stf descriptor. Compare several fields in the stf
	# descriptor to the image descriptor to make sure this is
	# a valid stf descriptor.

	stf = IM_KDES(im)
	if (stf == NULL)
	    return (false)

	if (IM_ACMODE(im) != STF_ACMODE(stf))
	    return (false)

	ic = stridx (STF_DATATYPE(stf), "RICU")
	if (ic == 0)
	    return (false)

	ic = stridx (star, STF_DATATYPE(stf))
	if (ic == 0)
	    return (false)

	ic = ic + 1
	nc = ctoi (STF_DATATYPE(stf), ic, nbytes)
	bitpix = nbytes * NBITS_BYTE

	if (STF_BITPIX(stf) != bitpix)
	    return (false)

	return (true)
end
