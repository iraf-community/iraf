# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>

# IMPS3? -- Put a section to an apparently three dimensional image.

pointer procedure imps3r (im, x1, x2, y1, y2, z1, z2)

pointer	im
int	x1, x2, y1, y2, z1, z2
long	vs[3], ve[3]
pointer	impgsr(), impl3r()

begin
	if (x1 == 1 && x2 == IM_LEN(im,1) && y1 == y2 && z1 == z2)
	    return (impl3r (im, y1, z1))
	else {
	    vs[1] = x1
	    ve[1] = x2

	    vs[2] = y1
	    ve[2] = y2

	    vs[3] = z1
	    ve[3] = z2

	    return (impgsr (im, vs, ve, 3))
	}
end
