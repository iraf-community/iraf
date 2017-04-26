include <imhdr.h>
include "i2sun.h"


# TR_SETUP -- Set up spatial transformation parameters.

procedure tr_setup (im, tr)

pointer	im			# An input image descriptor
pointer	tr			# Transformation structure

int	ncols, nlines

begin
	ncols  = IM_LEN(im,COL)
	nlines = IM_LEN(im,LINE)

	# Determine output raster dimensions.  
	TR_XS(tr) = 1
	TR_XE(tr) = ncols
	if (!IS_INDEFI(TR_XSIZE(tr)))
	    TR_XE(tr) = max (1, TR_XSIZE(tr))
	else if (TR_XMAG(tr) != 1.0)
	    TR_XE(tr) = max (1, ncols * int(TR_XMAG(tr)))

	TR_YS(tr) = 1
	TR_YE(tr) = nlines
	if (!IS_INDEFI(TR_YSIZE(tr)))
	    TR_YE(tr) = max (1, TR_YSIZE(tr))
	else if (TR_YMAG(tr) != 1.0)
	    TR_YE(tr) = max (1, nlines * int(TR_YMAG(tr)))
end
