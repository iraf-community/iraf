# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

# AREAD -- Asychronous block read from a binary file.  Reads can only
# start at a character offset which is an integral multiple of the file
# block size.

procedure aread (fd, buffer, maxchars, char_offset)

int	fd			# FIO file descriptor
int	maxchars		# maximum number of chars to be read
char	buffer[ARB]		# buffer into which data is to be read
long	char_offset		# one-indexed char offset in file

int	maxbytes
long	byte_offset

begin
	maxbytes = maxchars * SZB_CHAR
	byte_offset = (char_offset-1) * SZB_CHAR + 1

	call areadb (fd, buffer, maxbytes, byte_offset)
end
