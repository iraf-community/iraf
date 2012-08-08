# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imio.h>

# IM_PMSNE1 -- Pixel mask section not empty.

bool procedure im_pmsne1 (im, x1, x2)

pointer	im			#I image descriptor
int	x1, x2			#I section to be tested

bool	pm_sectnotempty()

begin
	return (pm_sectnotempty (IM_PL(im), x1, x2, 1))
end
