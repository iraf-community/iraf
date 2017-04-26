# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<syserr.h>
include	<config.h>
include	<fio.h>

# AREADB -- Asychronous byte-oriented block read from a binary file.  Reads
# can only start at a character offset which is an integral multiple of the
# file block size.

procedure areadb (fd, buffer, maxbytes, byte_offset)

int	fd			# FIO file descriptor
int	maxbytes		# maximum number of machine bytes to read
char	buffer[ARB]		# buffer into to which data is to be read
long	byte_offset		# one-indexed byte offset in file

long	file_offset
int	junk, awaitb()
errchk	filerr, syserr
include	<fio.com>

begin
	fp = fiodes[fd]
	if (fd <= 0 || fp == NULL)
	    call syserr (SYS_FILENOTOPEN)

	# If channel is active, wait for completion.  Do not abort if await is
	# not called between i/o requests, since this is what happens when an
	# error occurs, ant it would lead to a file error following error
	# restart.

	if (FCIOMODE(fp) != INACTIVE)
	    junk = awaitb (FAFD(fp))

	if (FBLKSIZE(fp) == 0)				# streaming device
	    ;
	else if (byte_offset < 1)
	    call filerr (FNAME(fp), SYS_FARDOOB)
	else if (FILSIZE(fp) >= 0 && byte_offset - (FILSIZE(fp)*SZB_CHAR) > 1) {
	    FNBYTES(fp) = 0				# return EOF
	    return
	}

	# If not a streaming device, check alignment of block.  If streaming
	# device, file offset passed to z-routine must be zero.

	file_offset = byte_offset
	if (FBLKSIZE(fp) > 0) {
	    if (mod (byte_offset-1, (FBLKSIZE(fp)*SZB_CHAR)) != 0)
		call filerr (FNAME(fp), SYS_FARDALIGN)
	} else
	    file_offset = 0

	call zlocva (buffer, FLOCBUF(fp))
	FCIOMODE(fp) = READ_IN_PROGRESS
	FFIOMODE(fp) = READ_IN_PROGRESS
	FAFD(fp) = fd

	# If the CLOSE flag is set for the channel, open and close the channel
	# at the host level every time an i/o operation takes place.  (Used to
	# save host channel descriptors).  The code is structured so that the
	# FCLOSEFD flag may change state at any time, with the channel being
	# left either closed or open the next time we are called.  Any open or
	# read errors are reported as read errors when AWAITB is later called.

	if (FCLOSEFD(fp) == NO && FCHAN(fp) != ERR)
	    call zcall4 (ZARDBF(fp), FCHAN(fp), buffer, maxbytes, file_offset)
	else {
	    if (FCHAN(fp) == ERR)
		call zcall3 (FDEVOPEN(fp), FPKOSFN(fp), FMODE(fp), FCHAN(fp))
	    if (FCHAN(fp) != ERR) {
		call zcall4 (ZARDBF(fp),
		    FCHAN(fp), buffer, maxbytes, file_offset)
		if (FCLOSEFD(fp) == YES) {
		    junk = awaitb (FAFD(fp))
		    call zcall2 (ZCLSBF(fp), FCHAN(fp), junk)
		    FCHAN(fp) = ERR
		}
	    }
	}
end
