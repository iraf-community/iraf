# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"qpio.h"

# QPIO_GETRANGE -- Get the current range in X and Y within which events will
# be extracted by qpio_getevents.

int procedure qpio_getrange (io, vs, ve, maxdim)

pointer	io			#I QPIO descriptor
int	vs[ARB]			#O start vector (lower left corner)
int	ve[ARB]			#O end vector (upper right corner)
int	maxdim			#I vector length (ndim=2 at present)

begin
	call amovi (IO_VS(io,1), vs, maxdim)
	call amovi (IO_VE(io,1), ve, maxdim)
	return (NDIM)
end
