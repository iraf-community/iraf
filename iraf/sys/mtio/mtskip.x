# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<fset.h>

# MT_SKIP_RECORD -- Skip records on an opened file.  Return the actual number
# of records skipped; stop if EOF is reached.

long procedure mt_skip_record (fd, nrecords)

int	fd				#I magtape device
long	nrecords			#I number of records to skip

pointer	sp, buf
long	n, c_0
size_t	bufsize
errchk	aread, await
int	await(), fstati()

begin
	call smark (sp)
	bufsize = fstati (fd, F_BUFSIZE)
	call salloc (buf, bufsize, TY_CHAR)

	for (n=1;  n <= nrecords;  n=n+1) {
	    c_0 = 0
	    call aread (fd, Memc[buf], bufsize, c_0)
	    if (await (fd) == EOF)
		break
	}

	call sfree (sp)
	return (n-1)
end
