# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <fset.h>
include <mii.h>
include <mach.h>


# RFT_GETBUF -- Procedure to get the buffer.

long procedure rft_getbuf (fd, buf, sz_rec, bufsize, recptr)

int	fd		# file descriptor
char	buf[ARB]	# buffer to be filled
size_t	sz_rec		# size in chars of record to be read
size_t	bufsize		# buffer size in records
long	recptr		# last successful FITS record read

long	i, bsz
size_t	nchars
long	read(), lmod(), fstatl()
errchk	read

begin
	bsz = bufsize
	nchars = 0
	repeat {
	    iferr {
	        i = read (fd, buf[nchars+1], sz_rec - nchars)
	    } then {
	        call printf ("Error reading FITS record %d\n")
	        if (lmod(recptr + 1, bsz) == 0)
		    call pargl ((recptr + 1) / bsz)
	        else
		    call pargl ((recptr + 1) / bsz + 1)
	        call fsetl (fd, F_VALIDATE, fstatl (fd, F_SZBBLK) / SZB_CHAR)
	        i = read (fd, buf[nchars+1], sz_rec - nchars)
	    }

	    if (i == EOF)
		break
	    else
	        nchars = nchars + i

	} until (nchars >= sz_rec)

	if ((i == EOF) && (nchars == 0))
	    return (EOF)
	else {
	    recptr = recptr + 1
	    return (nchars)
	}
end
