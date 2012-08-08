# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<syserr.h>
include	<fio.h>

# NOTE -- Note offset in file for a subsequent SEEK.  If text file, the offset
# of the current line is returned; it is only permissible to seek to the
# beginning of a line on a text file.  If binary file, the offset returned is
# the offset at which the next BUFFERED i/o transfer will occur.  If file is
# being accessed unbuffered random, the concept of file position is meaningless.

long procedure note (fd)

int	fd
errchk	filerr
include	<fio.com>

begin
	fp = fiodes[fd]
	if (fd <= 0 || fp == NULL)
	    call filerr (FNAME(fp), SYS_FILENOTOPEN)

	if (FTYPE(fp) == TEXT_FILE) {
	    call zcall2 (ZNOTTX(fp), FCHAN(fp), boffset[fd])
	    return (boffset[fd])
	} else
	    return (LNOTE(fd))
end
