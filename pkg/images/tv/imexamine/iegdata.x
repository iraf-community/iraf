# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
 
# IE_GDATA -- Get image data with boundary checking.
 
pointer procedure ie_gdata (im, x1, x2, y1, y2)
 
pointer	im			# IMIO pointer
int	x1, x2, y1, y2		# Subraster limits (input and output)
 
int	i, nc, nl
pointer	imgs2r()
errchk	imgs2r
 
begin
	nc = IM_LEN(im,1)
	nl = IM_LEN(im,2)

	if (IS_INDEFI (x1))
	    x1 = 1
	if (IS_INDEFI (x2))
	    x2 = nc
	if (IS_INDEFI (y1))
	    y1 = 1
	if (IS_INDEFI (y2))
	    y2 = nl
 
	i = max (x1, x2)
	x1 = min (x1, x2)
	x2 = i
	i = max (y1, y2)
	y1 = min (y1, y2)
	y2 = i

	if (x2 < 1 || x1 > nc || y2 < 1 || y1 > nl)
	    call error (1, "Pixels out of bounds")
	
	x1 = max (1, x1)
	x2 = min (nc, x2)
	y1 = max (1, y1)
	y2 = min (nl, y2)

	return (imgs2r (im, x1, x2, y1, y2))
end
