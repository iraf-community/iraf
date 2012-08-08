include "tcs.h"

# TCS_SHAPE -- Shape of column array

procedure tcs_shape (descrip, length, ndim, maxdimen)

pointer	descrip		# i: column selector
int	length[ARB]	# o: dimension lengths
int	ndim		# o: number of dimensions
int	maxdimen	# i: max number of dimensions 
#--
int	idim

begin
	ndim = TCS_DIMEN(descrip)
	do idim = 1, ndim {
	    if (idim > maxdimen)
		break

	    length[idim] = (((TCS_LAST(descrip,idim) - 
			      TCS_FIRST(descrip,idim)) / 
			     TCS_INC(descrip,idim)) + 1)
	}
end
