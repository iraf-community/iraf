# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<syserr.h>
include	<error.h>
include	<fset.h>
include	<fio.h>

# FSTATL -- Return a file status value of type long integer (l).

long procedure fstatl (fd, what)

int	fd, what
int	ffilsz()
include	<fio.com>

begin
	fp = fiodes[fd]
	if (fd <= 0 || fp == NULL)
	    iferr (call syserr (SYS_FILENOTOPEN))
		call erract (EA_FATAL)

	switch (what) {
	case F_FILESIZE:
	    FILSIZE(fp) = ffilsz (fd)
	    return (FILSIZE(fp))
	default:
	    iferr (call filerr (FNAME(fp), SYS_FSTATUNKPAR))
		call erract (EA_FATAL)
	}
end
