include "tcs.h"

# TCS_LINESIZE -- Size of a single line in a column array

long procedure tcs_linesize (descrip)

pointer	descrip		# i: column selector
#--
size_t	sz_val
long	size
int	ndim
pointer sp, length

begin
	call smark (sp)
	sz_val = MAXDIM
	call salloc (length, sz_val, TY_LONG)

	# Get length of each axis

	call tcs_shape (descrip, Meml[length], ndim, MAXDIM)

	# Return length of first

	size = Meml[length]
	call sfree (sp)

	return (size)
end
