# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<syserr.h>
include	<fio.h>

# FLUSH -- Flush any buffered output to the file.

procedure flush (fd)

int	fd
pointer	bp
int	status, and()
errchk	filerr, fflsbf, fwatio
include	<fio.com>

begin
	fp = fiodes[fd]
	if (fp == NULL)
	    return
	else if (FTYPE(fp) == STRING_FILE || FTYPE(fp) == SPOOL_FILE)
	    return
	bp = bufptr[fd]

	call fcanpb (fd)	# cancel any pushback
	UPDATE_IOP(fd)		# update the i/o pointers

	if (BUF_MODIFIED(fd)) {
	    # Buffer has been written into and must be flushed to disk.
	    if (and (FF_WRITE, fflags[fd]) == 0)
		call filerr (FNAME(fp), SYS_FNOWRITEPERM)

	    if (FTYPE(fp) == TEXT_FILE) {
		call fputtx (fd, Memc[bp], otop[fd] - bp, status)
		iop[fd] = bp
		itop[fd] = bp
	    } else {
		call fflsbf (fd, bp, otop[fd]-bp, boffset[fd])
		call fwatio (fd)
		if (FBLKSIZE(fp) == 0) {	# streaming device?
		    boffset[fd] = LNOTE(fd)
		    iop[fd] = bp
		    otop[fd] = bp
		    itop[fd] = bp
		}
		status = FILSTAT(fp)
	    }

	    if (status == ERR)
		call filerr (FNAME(fp), SYS_FWRITE)
	    otop[fd] = bp
	}

	if (FTYPE(fp) == TEXT_FILE && and (FF_WRITE, fflags[fd]) != 0)
	    call zcall2 (ZFLSTX(fp), FCHAN(fp), status)

	if (status == ERR)
	    call filerr (FNAME(fp), SYS_FWRITE)
end
