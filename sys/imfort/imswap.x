# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include "imfort.h"


# IMSWAP -- Swap bytes in pixel data if indicated for this host and image.

procedure imswap (im, buf, nchars)

pointer	im
char	buf[ARB]
int	nchars

int	nbytes

begin
	if (IM_SWAP(im) == NO)
	    return

	nbytes = nchars * SZB_CHAR
	switch (IM_SZPIXEL(im) * SZB_CHAR) {
	case 2:
	    call bswap2 (buf, 1, buf, 1, nbytes)
	case 4:
	    call bswap4 (buf, 1, buf, 1, nbytes)
	case 8:
	    call bswap8 (buf, 1, buf, 1, nbytes)
	}
end
