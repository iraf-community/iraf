# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	<fset.h>

# MT_SKIP_RECORD -- Skip records on an opened file.  Return the actual number
# of records skipped; stop if EOF is reached.

int procedure mt_skip_record (fd, nrecords)

int	fd
int	nrecords
pointer	buf
int	n, bufsize
int	await()
errchk	malloc, aread, await

begin
	bufsize = MT_SZBDEFIBUF / SZB_CHAR
	call malloc (buf, bufsize, TY_CHAR)

	for (n=1;  n <= nrecords;  n=n+1) {
	    call aread (fd, Memc[buf], bufsize, 0)
	    if (await (fd) == EOF)
		break
	}

	call mfree (buf, TY_CHAR)
	return (n-1)
end
