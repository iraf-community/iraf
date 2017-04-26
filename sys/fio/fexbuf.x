# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<fio.h>

define	INC_BUFSIZE		4096


# FEXBUF -- Expand the size of the file buffer.  Called by FLSBUF when the FIO
# buffer fills while writing to a file of type SPOOL_FILE.  Spool files are
# files of arbitrary size, buffered entirely in memory.  Typically, a finite
# amount of data is written into a spoolfile, the file is rewound, the data
# is read back out, and so on.  This makes it possible to use the file interface
# to pass data between program modules.

procedure fexbuf (fd)

int	fd			# file which needs a larger buffer

pointer	bp
int	offset
errchk	malloc, realloc
include	<fio.com>

begin
	fp = fiodes[fd]
	bp = bufptr[fd]
	offset = iop[fd] - bp

	if (bufptr[fd] == NULL) {
	    if (FBUFSIZE(fp) == 0)
		FBUFSIZE(fp) = SZ_SPOOLBUF
	    call malloc (bp, FBUFSIZE(fp), TY_CHAR)
	} else {
	    FBUFSIZE(fp) = FBUFSIZE(fp) + INC_BUFSIZE
	    call realloc (bp, FBUFSIZE(fp), TY_CHAR)
	}

	boffset[fd] = 1
	bufptr[fd]  = bp
	buftop[fd]  = bp + FBUFSIZE(fp)

	iop[fd]  = bp + offset  
	itop[fd] = iop[fd]
	otop[fd] = buftop[fd]
end
