include "tcs.h"

# TCS_LINESIZE -- Size of a single line in a column array

int procedure tcs_linesize (descrip)

pointer	descrip		# i: column selector
#--
int	size, ndim
pointer sp, length

begin
	call smark (sp)
	call salloc (length, MAXDIM, TY_INT)

	# Get length of each axis

	call tcs_shape (descrip, Memi[length], ndim, MAXDIM)

	# Return length of first

	size = Memi[length]
	call sfree (sp)

	return (size)
end
