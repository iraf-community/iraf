# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <fset.h>
include <mii.h>
include <mach.h>


# RFT_GETBUF -- Procedure to get the buffer.

long procedure rft_getbuf (fd, buf, sz_rec, bufsize, recptr)

int	fd		# file descriptor
char	buf[ARB]	# buffer to be filled
int	sz_rec		# size in chars of record to be read
int	bufsize		# buffer size in records
int	recptr		# last successful FITS record read

int	i, nchars
int	fstati()
long	read()
errchk	read

begin
	nchars = 0
	repeat {
	    iferr {
	        i = read (fd, buf[nchars+1], sz_rec - nchars)
	    } then {
	        call printf ("Error reading FITS record %d\n")
	        if (mod (recptr + 1, bufsize) == 0)
		    call pargi ((recptr + 1) / bufsize)
	        else
		    call pargi ((recptr + 1) / bufsize + 1)
	        call fseti (fd, F_VALIDATE, fstati (fd, F_SZBBLK) / SZB_CHAR)
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
