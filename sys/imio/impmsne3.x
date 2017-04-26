# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imio.h>

# IM_PMSNE3 -- Pixel mask section not empty.

bool procedure im_pmsne3 (im, x1,x2, y1,y2, z1,z2)

pointer	im			#I image descriptor
int	x1, x2			#I section to be tested
int	y1, y2			#I section to be tested
int	z1, z2			#I section to be tested

long	vs[3], ve[3]
bool	pm_sectnotempty()

begin
	vs[1] = x1;  vs[2] = y1;  vs[3] = z1
	ve[1] = x2;  ve[2] = y2;  ve[3] = z2

	return (pm_sectnotempty (IM_PL(im), vs, ve, 3))
end
