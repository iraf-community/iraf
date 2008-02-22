# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

# AWRITE -- Asychronous block write to a binary file.  Writes can only
# start at a character offset which is an integral multiple of the file
# block size.

procedure awrite (fd, buffer, nchars, char_offset)

int	fd
char	buffer[ARB]
size_t	nchars
long	char_offset

size_t	nbytes
long	byte_offset

begin
	nbytes = nchars * SZB_CHAR
	byte_offset = (char_offset-1) * SZB_CHAR + 1

	call awriteb (fd, buffer, nbytes, byte_offset)
end
