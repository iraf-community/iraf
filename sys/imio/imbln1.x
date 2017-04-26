# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imio.h>

# IMBLN1 -- Get the length of the axes of a one dimensional subraster.
# Must be called immediately after the get or put call that created the
# buffer.

procedure imbln1 (imdes, nx)

pointer	imdes
int	nx
pointer	bdes

begin
	# Get pointer to most recently used buffer descriptor.
	bdes = IM_LASTBDES(imdes)

	nx = abs (BD_VE(bdes,1) - BD_VS(bdes,1)) + 1
end
