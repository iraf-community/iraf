# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<fio.h>

# GETCI -- Get a character (passed as an integer) from a file.

int procedure getci (fd, ch)

int	fd			# input file
int	ch			# character (output)
int	filbuf()
errchk	filbuf
include	<fio.com>

begin
 	if (iop[fd] < bufptr[fd] || iop[fd] >= itop[fd])
	    if (filbuf(fd) == EOF) {
		ch = EOF
		return (EOF)
	    }

	ch = Memc[iop[fd]]
	iop[fd] = iop[fd] + 1

	return (ch)
end
