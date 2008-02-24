# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

define	SZ_NUMBUF	8		# encoded count for an XFER


# PSIO_XFER -- Transfer a data record to a process to complete an XFER
# request.  Write the byte count record followed by the data record.
# These must be written as two separate records or deadlock
# will occur (with the reader waiting for the second record).

procedure psio_xfer (fd, buf, nchars)

int	fd			# output file
char	buf[ARB]		# buffer containing record to be written
size_t	nchars			# length of record

long	lval
size_t	ndigits
char	numbuf[SZ_NUMBUF]
int	ltoc()

begin
	if (nchars >= 0) {
	    lval = nchars
	    ndigits = ltoc (lval, numbuf, SZ_NUMBUF)
	    numbuf[ndigits+1] = '\n'
	    call write (fd, numbuf, ndigits + 1)
	    call flush (fd)

	    if (nchars > 0) {
		call write (fd, buf, nchars)
		call flush (fd)
	    }
	}
end
