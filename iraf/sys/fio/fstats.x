# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<fset.h>
include	<syserr.h>
include	<fio.h>

# FSTATS -- Return a file status value of type string (s).

procedure fstats (fd, what, outstr, maxch)

int	fd, what, maxch
char	outstr[ARB]
pointer	ffp
errchk	syserr
include	<fio.com>

begin
	ffp = fiodes[fd]
	if (fd <= 0 || ffp == NULL)
	    call syserr (SYS_FILENOTOPEN)

	switch (what) {
	case F_FILENAME:
	    call strcpy (FNAME(ffp), outstr, maxch)
	default:
	    call filerr (FNAME(ffp), SYS_FSTATUNKPAR)
	}
end
