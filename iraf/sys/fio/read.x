# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<syserr.h>
include	<fio.h>

# READ -- Read binary chars from a file.  Data is read in "chunks" from
# the file buffer into the output buffer supplied by the calling procedure,
# refilling the file buffer as necessary.  The read terminates, possibly
# returning fewer than the maximum number of chars, if the file buffer
# cannot be filled (as occurs at the EOF of a binary file, or when reading
# from a terminal or pipe).

int procedure read (fd, buffer, maxchars)

int	fd
char	buffer[ARB]
int	maxchars

int	maxch
bool	stream
int	nchars, chunk_size, nchars_read, filbuf()
errchk	filbuf, filerr
include	<fio.com>

begin
	if (fd <= 0 || fiodes[fd] == NULL)
	    call syserr (SYS_FILENOTOPEN)

	nchars = 0
	maxch = maxchars
	stream = (FBLKSIZE(fiodes[fd]) == 0)

	while (nchars < maxch) {
	    if (iop[fd] < bufptr[fd] || iop[fd] >= itop[fd]) {
		nchars_read = filbuf (fd)
		if (nchars_read == EOF)
		    break		# return EOF only if nchars = 0
		else {
		    # Don't loop if record structured device or EOF.
		    if (itop[fd] < buftop[fd] || stream)
			maxch = min (maxchars, nchars + nchars_read)
		}
	    }
	    chunk_size = min (maxch - nchars, itop[fd] - iop[fd])
	    if (chunk_size <= 0)
		break
	    else {
		call amovc (Memc[iop[fd]], buffer[nchars+1], chunk_size)
		iop[fd] = iop[fd] + chunk_size
		nchars = nchars + chunk_size
	    }
	}

	FILSTAT(fiodes[fd]) = nchars
	FNCHARS(fiodes[fd]) = nchars

	if (nchars == 0)
	    return (EOF)
	else
	    return (nchars)
end
