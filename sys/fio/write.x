# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<syserr.h>
include	<fio.h>

# WRITE -- Write binary chars to a file.  The specified number of chars will
# always be written (with the file buffer being flushed as many times as
# necessary) unless an error occurs.

procedure write (fd, buffer, maxchars)

int	fd
char	buffer[ARB]
int	maxchars
int	nchars, chunk_size
errchk	flsbuf
include	<fio.com>

begin
	if (fd <= 0 || fiodes[fd] == NULL)
	    call syserr (SYS_FILENOTOPEN)

	nchars = 0

	while (nchars < maxchars) {
	    if (iop[fd] < bufptr[fd] || iop[fd] >= otop[fd])
		call flsbuf (fd, maxchars - nchars)
	    chunk_size = min (maxchars - nchars, otop[fd] - iop[fd])
	    if (chunk_size <= 0)
		break
	    else {
		call amovc (buffer[nchars+1], Memc[iop[fd]], chunk_size)
		iop[fd] = iop[fd] + chunk_size
		nchars = nchars + chunk_size
	    }
	}

	FNCHARS(fiodes[fd]) = nchars
end
