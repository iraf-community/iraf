include "tcs.h"

# TCS_TOTSIZE -- Get total length of array from column selector

int procedure tcs_totsize (descrip)

pointer	descrip		#i: column selector
#--
int	size, idim, ndim
pointer sp, length

begin
	call smark (sp)
	call salloc (length, MAXDIM, TY_INT)

	# Get length of each axis

	call tcs_shape (descrip, Memi[length], ndim, MAXDIM)

	# Multiply lengths together for total length

	size = 1
	do idim = 1, ndim
	    size = size * Memi[length+idim-1]

	call sfree (sp)
	return (size)
end
