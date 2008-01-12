# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>

# IMPS2? -- Put a section to an apparently two dimensional image.

pointer procedure imps2d (im, x1, x2, y1, y2)

pointer	im
int	x1, x2, y1, y2
long	vs[2], ve[2]
pointer	impgsd(), impl2d()

begin
	if (x1 == 1 && x2 == IM_LEN(im,1) && y1 == y2)
	    return (impl2d (im, y1))
	else {
	    vs[1] = x1
	    ve[1] = x2

	    vs[2] = y1
	    ve[2] = y2

	    return (impgsd (im, vs, ve, 2))
	}
end
