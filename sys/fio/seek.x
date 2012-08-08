# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<syserr.h>
include	<fio.h>

# SEEK -- Position the i/o pointer (file offset at which the next i/o transfer
# will occur) for a file.  Note that ITOP may have to be adjusted before
# performing the seek, to make newly written data readable (as when writing at
# EOF, seeking backward within the same buffer, and reading).  A physical seek
# is performed for text files.  For binary files, a logical seek is performed,
# adjusting the i/o pointer.  Physical seeks on binary files are initiated
# by FFAULT, when filling or flushing a file buffer.

procedure seek (fd, offset)

int	fd			# file
long	offset			# offset == BOF,EOF, or char offset

pointer	bp
long	file_offset
int	status
long	ffilsz()
errchk	filerr, syserr, ffilsz
include	<fio.com>

begin
	fp = fiodes[fd]
	if (fd <= 0 || fp == NULL)
	    call syserr (SYS_FILENOTOPEN)

	call fcanpb (fd)	# cancel any pushback
	UPDATE_IOP(fd)		# make newly written data readable

	if (FTYPE(fp) == TEXT_FILE) {
	    # General seeks only permitted on text files opened for reading.
	    if (FMODE(fp) != READ_ONLY)
		if (offset != BOF && offset != EOF)
		    call filerr (FNAME(fp), SYS_FSEEKNTXF)

	    bp = bufptr[fd]
	    if (BUF_MODIFIED(fd)) {		# flush buffer?
		call fputtx (fd, Memc[bp], otop[fd] - bp, status)
		if (status != ERR)
		    call zcall2 (ZFLSTX(fp), FCHAN(fp), status)
		if (status == ERR)
		    call filerr (FNAME(fp), SYS_FWRITE)
	    }

	    iop[fd] = bp
	    itop[fd] = bp
	    otop[fd] = bp

	    call zcall3 (ZSEKTX(fp), FCHAN(fp), offset, status)
	    if (status == ERR)
		call filerr (FNAME(fp), SYS_FSEEK)

	} else {				# logical seek (binary files)
	    switch (offset) {
	    case BOF:
		file_offset = 1
	    case EOF:
		file_offset = ffilsz (fd) + 1
	    default:
		file_offset = offset
	    }
	    iop[fd] = file_offset - boffset[fd] + bufptr[fd]
	}
end
