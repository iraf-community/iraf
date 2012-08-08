# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>

# REALLOC -- Change the size of a previously allocated buffer, moving the
# buffer if necessary.  If there is no old buffer (NULL pointer) simply
# allocate a new buffer.

procedure realloc (ubufp, nelems, dtype)

pointer	ubufp			# buffer to be reallocated
int	nelems			# new size of buffer
int	dtype			# buffer datatype

int	krealloc()
int	zrtadr()
include	"memdbg.com"

begin
	retaddr = zrtadr()
	if (krealloc (ubufp, nelems, dtype) == ERR) {
	    ubufp = NULL
	    call syserr (SYS_MFULL)
	}
end
