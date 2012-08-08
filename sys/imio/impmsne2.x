# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imio.h>

# IM_PMSNE2 -- Pixel mask section not empty.

bool procedure im_pmsne2 (im, x1, x2, y1, y2)

pointer	im			#I image descriptor
int	x1, x2			#I section to be tested
int	y1, y2			#I section to be tested

long	vs[2], ve[2]
bool	pm_sectnotempty()

begin
	vs[1] = x1;  vs[2] = y1
	ve[1] = x2;  ve[2] = y2

	return (pm_sectnotempty (IM_PL(im), vs, ve, 2))
end
