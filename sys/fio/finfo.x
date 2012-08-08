# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>
include	<finfo.h>
include	<config.h>
include	<fio.h>

.help finfo
.nf ___________________________________________________________________________
FINFO -- Return information on the named file (directory entry).
See <finfo.h> for a definition of the contents of the output structure.

The times are returned in units of seconds from midnight on Jan 1, 1980,
local standard time.  Use CTIME to convert the integer time into a character
string.  The owner name is returned as a character string (stored as chars
in the long integer finfo array).  The owner permissions are bits 1-2 of the
FI_PERM field, group permissions are bits 3-4, world bits 5-6.  The meaning
of the bits are RW (read, write).  Execute permission is indicated by a
file type.  Note that the file size is returned in BYTES, rather than chars
(bytes are more desirable for directory listings).

Call ZFPROT to determine if a file has delete permission.  Call ACCESS to
determine if a "regular" file is of type text or binary.  This information
requires additional expense to obtain on some systems, is not required for
a simple directory listing, and hence is not provided by FINFO.
.endhelp ______________________________________________________________________

int procedure finfo (fname, ostruct)

char	fname[ARB]
long	ostruct[LEN_FINFO]
int	status
include	<fio.com>

begin
	iferr (call fmapfn (fname, pathname, SZ_PATHNAME))
	    return (ERR)

	call zfinfo (pathname, ostruct, status)

	# ZFINFO returns the file owner string as a packed string.
	if (status != ERR)
	    call strupk (FI_OWNER(ostruct), FI_OWNER(ostruct), FI_SZOWNER)

	return (status)
end
