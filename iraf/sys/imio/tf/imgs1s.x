# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>

# IMGS1? -- Get a section from an apparently one dimensional image.

pointer procedure imgs1s (im, x1, x2)

pointer	im
int	x1, x2
pointer	imggss(), imgl1s()

begin
	if (x1 == 1 && x2 == IM_LEN(im,1))
	    return (imgl1s (im))
	else
	    return (imggss (im, long(x1), long(x2), 1))
end
