# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>

# MFREE -- Free a previously allocated buffer.  If the buffer has already been
# returned (NULL pointer), ignore the request.  Once the buffer has been
# returned, the old pointer value is of not useful (and invalid), so set it
# to NULL.

procedure mfree (ptr, dtype)

pointer	ptr
int	fwa, dtype, status
int	mgtfwa()
errchk	mgtfwa

begin
	if (ptr != NULL) {
	    fwa = mgtfwa (ptr, dtype)

	    call zmfree (fwa, status)
	    if (status == ERR)
		call sys_panic (SYS_MCORRUPTED, "Memory has been corrupted")

	    ptr = NULL
	}
end
