include "tcs.h"

# TCS_TOTSIZE -- Get total length of array from column selector

long procedure tcs_totsize (descrip)

pointer	descrip		#i: column selector
#--
size_t	sz_val
long	size
int	idim, ndim
pointer sp, length

begin
	call smark (sp)
	sz_val = MAXDIM
	call salloc (length, sz_val, TY_LONG)

	# Get length of each axis

	call tcs_shape (descrip, Meml[length], ndim, MAXDIM)

	# Multiply lengths together for total length

	size = 1
	do idim = 1, ndim
	    size = size * Meml[length+idim-1]

	call sfree (sp)
	return (size)
end
