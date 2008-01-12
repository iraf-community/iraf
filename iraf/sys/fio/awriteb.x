# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<syserr.h>
include	<config.h>
include	<fio.h>

# AWRITEB -- Asychronous byte-oriented block write to a binary file.  Writes
# can only start at a byte offset which is an integral multiple of the file
# block size.

procedure awriteb (fd, buffer, nbytes, byte_offset)

int	fd			# FIO file descriptor
int	nbytes			# number of machine bytes to be written
char	buffer[ARB]		# buffer containing the data
long	byte_offset		# one-indexed byte offset in file

long	file_offset, szb_file
int	junk, awaitb()
errchk	filerr, syserr
include	<fio.com>

begin
	fp = fiodes[fd]
	if (fd <= 0 || fp == NULL)
	    call syserr (SYS_FILENOTOPEN)

	# Ignore null writes.
	if (nbytes == 0)
	    return

	# If channel is active, wait for completion.  Do not abort if await is
	# not called between i/o requests, since this is what happens when an
	# error occurs, ant it would lead to a file error following error
	# restart.

	if (FCIOMODE(fp) != INACTIVE)
	    junk = awaitb (FAFD(fp))

	if (FBLKSIZE(fp) > 0)				# not streaming device
	    if (byte_offset < 1 || byte_offset - (FILSIZE(fp)*SZB_CHAR) > 1)
		call filerr (FNAME(fp), SYS_FAWROOB)

	# If not a streaming device, check alignment of block.  If streaming
	# device, byte offset passed to z-routine must be zero.

	file_offset = byte_offset
	if (FBLKSIZE(fp) > 0) {
	    if (mod (byte_offset-1, (FBLKSIZE(fp)*SZB_CHAR)) != 0)
		call filerr (FNAME(fp), SYS_FAWRALIGN)
	} else
	    file_offset = 0

	# Keep track of file size if appending to file.  FIO keeps track of
	# the file size to the nearest char.  FILSIZE is negative for certain
	# types of files, in which case we do not know the file size.

	if (FILSIZE(fp) >= 0) {
	    szb_file = max (FILSIZE(fp) * SZB_CHAR, file_offset-1 + nbytes)
	    FILSIZE(fp) = (szb_file + SZB_CHAR-1) / SZB_CHAR
	}

	FCIOMODE(fp) = WRITE_IN_PROGRESS
	FFIOMODE(fp) = WRITE_IN_PROGRESS
	FAFD(fp) = fd

	# If the CLOSE flag is set for the channel, open and close the channel
	# at the host level every time an i/o operation takes place.  (Used to
	# save host channel descriptors).  The code is structured so that the
	# FCLOSEFD flag may change state at any time, with the channel being
	# left either closed or open the next time we are called.  Any open or
	# write errors are reported as write errors when AWAITB is later called.

	if (FCLOSEFD(fp) == NO && FCHAN(fp) != ERR)
	    call zcall4 (ZAWRBF(fp), FCHAN(fp), buffer, nbytes, file_offset)
	else {
	    if (FCHAN(fp) == ERR)
		call zcall3 (FDEVOPEN(fp), FPKOSFN(fp), FMODE(fp), FCHAN(fp))
	    if (FCHAN(fp) != ERR) {
		call zcall4 (ZAWRBF(fp),
		    FCHAN(fp), buffer, nbytes, file_offset)
		if (FCLOSEFD(fp) == YES) {
		    junk = awaitb (FAFD(fp))
		    call zcall2 (ZCLSBF(fp), FCHAN(fp), junk)
		    FCHAN(fp) = ERR
		}
	    }
	}
end
