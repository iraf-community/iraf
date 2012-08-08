# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>

# IMGS1? -- Get a section from an apparently one dimensional image.

pointer procedure imgs1x (im, x1, x2)

pointer	im
int	x1, x2
pointer	imggsx(), imgl1x()

begin
	if (x1 == 1 && x2 == IM_LEN(im,1))
	    return (imgl1x (im))
	else
	    return (imggsx (im, long(x1), long(x2), 1))
end
