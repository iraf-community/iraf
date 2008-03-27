# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"qpio.h"

# QPIO_GETRANGE -- Get the current range in X and Y within which events will
# be extracted by qpio_getevents.

int procedure qpio_getrange (io, vs, ve, maxdim)

pointer	io			#I QPIO descriptor
long	vs[ARB]			#O start vector (lower left corner)
long	ve[ARB]			#O end vector (upper right corner)
int	maxdim			#I vector length (ndim=2 at present)

size_t	sz_val

begin
	sz_val = maxdim
	call amovl (IO_VS(io,1), vs, sz_val)
	call amovl (IO_VE(io,1), ve, sz_val)
	return (NDIM)
end
