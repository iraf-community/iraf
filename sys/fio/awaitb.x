# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	<fio.h>

# AWAITB -- Wait for any pending i/o operations on a file to complete.
# Must be called after an AREADB or AWRITEB (to check for an i/o error
# and for synchronization) or an abort will result.

int procedure awaitb (fd)

int	fd
int	nbytes, nchars
include	<fio.com>

begin
	fp = fiodes[fd]

	if (FFIOMODE(fp) == INACTIVE)
	    return (FNBYTES(fp))
	else
	    call zcall2 (ZAWTBF(fp), FCHAN(fp), nbytes)

	nchars = nbytes
	if (nbytes >= 0)
	    nchars = (nbytes + SZB_CHAR-1) / SZB_CHAR

	FNBYTES(fp)  = nbytes
	FILSTAT(fp)  = nchars

	FCIOMODE(fp) = INACTIVE			# clear channel
	FFIOMODE(fp) = INACTIVE			# complete fd request

	if (nbytes >= 0)
	    return (nbytes)
	else
	    return (ERR)
end
