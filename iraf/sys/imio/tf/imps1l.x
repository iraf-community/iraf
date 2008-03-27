# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>

# IMPS1? -- Put a section to an apparently one dimensional image.

pointer procedure imps1l (im, x1, x2)

pointer	im		# image header pointer
long	x1		# first column
long	x2		# last column

pointer	impgsl(), impl1l()

begin
	if (x1 == 1 && x2 == IM_LEN(im,1))
	    return (impl1l (im))
	else
	    return (impgsl (im, x1, x2, 1))
end
