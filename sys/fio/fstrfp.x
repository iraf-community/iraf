# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<fio.h>

# FSTRFP -- Get a dummy file descriptor for use by STROPEN "files".
# The static part of the descriptor is returned to later be allocated
# by STROPEN.  The dynamic part is permanently allocated, and is used to
# make the string look more like a regular file.

procedure fstrfp (newfp)

pointer	newfp
pointer	str_fp
int	fd, fgetfd()
data	str_fp /NULL/
include	<fio.com>

begin
	if (str_fp == NULL) {
	    fd = fgetfd ("String_File", STRING_FILE, STRING_FILE)
	    str_fp = fiodes[fd]
	    fiodes[fd] = NULL
	}

	newfp = str_fp
end
