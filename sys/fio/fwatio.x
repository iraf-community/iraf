# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<syserr.h>
include	<fio.h>

# FWATIO -- Wait for i/o to complete on the file buffer, update file
# buffer pointers.

procedure fwatio (fd)

int	fd
int	nchars, bufmode
int	await()
errchk	filerr
include	<fio.com>

begin
	fp = fiodes[fd]

	if (FBUFMODE(fp) == INACTIVE)
	    return

	else {
	    nchars = await (fd)

	    # Set the buffer mode flag to inactive regardless of the status
	    # returned by await.  

	    bufmode = FBUFMODE(fp)
	    FBUFMODE(fp) = INACTIVE

	    if (bufmode == READ_IN_PROGRESS) {
		if (nchars == ERR)
		    call filerr (FNAME(fp), SYS_FREAD)
		else
		    itop[fd] = bufptr[fd] + nchars

	    } else if (nchars == ERR) {
		# If an i/o error occurs on a write invalidate the buffer
		# else during error recovery close will try again to write
		# the data, probably causing another error.

		otop[fd] = bufptr[fd]
		call filerr (FNAME(fp), SYS_FWRITE)
	    }

	    otop[fd] = bufptr[fd]
	}
end
