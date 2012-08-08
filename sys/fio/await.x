# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<config.h>
include	<mach.h>
include	<fio.h>

# AWAIT -- Wait for any pending i/o operations on a file to complete.
# Must be called after an AREAD or AWRITE (to check for an i/o error
# and for synchronization) or an abort will result.

int procedure await (fd)

int	fd
pointer	bufp
int	nbytes, nchars, nfill, loc_Mem, zero, mode
int	awaitb()
include	<fio.com>

data	loc_Mem /0/, zero /0/
errchk	syserr

begin
	fp = fiodes[fd]
	if (fd <= 0 || fp == NULL)
	    call syserr (SYS_FILENOTOPEN)

	# Read the i/o mode before awaitb clears it.
	mode = FFIOMODE(fp)

	# Wait for i/o.
	nbytes = awaitb (fd)
	if (nbytes <= 0)
	    return (nbytes)

	# Zero fill the last char of the output buffer if the last transfer was
	# a read and the number of bytes read was not commensurate with the
	# size of a char.

	if (mode == READ_IN_PROGRESS && nbytes > 0) {
	    nchars = (nbytes + SZB_CHAR-1) / SZB_CHAR
	    nfill  = nchars * SZB_CHAR - nbytes

	    if (nfill > 0) {
		if (loc_Mem == 0)
		    call zlocva (Memc, loc_Mem)
		bufp = FLOCBUF(fp) - loc_Mem + 1
		call bytmov (zero, 1, Memc[bufp], nbytes + 1, nfill)
	    }
	}

	# On exit from AWAITB, fp.filstat contains the number of chars
	# transferred in the last aread or awrite, or ERR.

	return (FILSTAT(fp))
end
