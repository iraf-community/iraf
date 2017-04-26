# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<fio.h>

# FMKBFS -- Make file buffer.  Called by FILBUF or FLSBUF when i/o is first
# done on a file, to create the file buffer.  Note that the logical offset
# must be maintained when the buffer pointer is changed.

procedure fmkbfs (fd)

int	fd
pointer	bp
int	dev_blksz
long	offset
errchk	malloc, FBUF_ALLOC
include	<fio.com>

begin
	fp = fiodes[fd]

	# Apply constraints on the size of a file i/o buffer.

	if (FTYPE(fp) != TEXT_FILE) {
	    # Promote the input buffer size to the next integral number of
	    # device blocks.

	    dev_blksz = FBLKSIZE(fp)
	    if (dev_blksz > 1) {
		FBUFSIZE(fp) = (FBUFSIZE(fp) + dev_blksz-1) /
		    dev_blksz * dev_blksz
	    } else
		FBUFSIZE(fp) = max (1, FBUFSIZE(fp))

	    # There is no maximum buffer (i/o transfer) size if the value
	    # returned by the kernel is zero.

	    if (FMAXBUFSIZE(fp) > 0)
		FBUFSIZE(fp) = min (FMAXBUFSIZE(fp), FBUFSIZE(fp))
	}

	# Note file offset, allocate buffer and initialize i/o pointers,
	# restore seek offset (which depends on buffer pointer, buf offset).

	offset = LNOTE(fd)
	if (FTYPE(fp) == TEXT_FILE)
	    call malloc (bp, FBUFSIZE(fp), TY_CHAR)
	else
	    call FBUF_ALLOC (bp, FBUFSIZE(fp), TY_CHAR)

	boffset[fd] = NULL
	bufptr[fd] = bp
	buftop[fd] = bp + FBUFSIZE(fp)
	itop[fd] = bp
	otop[fd] = bp

	if (FTYPE(fp) == BINARY_FILE)
	    LSEEK (fd, offset)
	else
	    iop[fd] = bp
end
