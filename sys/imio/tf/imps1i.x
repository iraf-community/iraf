# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>

# IMPS1? -- Put a section to an apparently one dimensional image.

pointer procedure imps1i (im, x1, x2)

pointer	im		# image header pointer
int	x1		# first column
int	x2		# last column

pointer	impgsi(), impl1i()

begin
	if (x1 == 1 && x2 == IM_LEN(im,1))
	    return (impl1i (im))
	else
	    return (impgsi (im, long(x1), long(x2), 1))
end
