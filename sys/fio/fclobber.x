# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<config.h>
include	<fio.h>

# FCLOBBER -- Clobber the named file if it exists.  Avoid clobbering a file
# which is already open.  File clobber is enabled by the environment variable
# of the same name.  If the file exists and clobber is disabled, it is an
# error unless multiple versions are permitted ("multversions").

procedure fclobber (fname)

char	fname[ARB]
int	fd
int	access()
bool	streq(), envgetb()
errchk	filerr, access, envgetb
include	<fio.com>

begin
	# Avoid clobbering a file which is already open.

	for (fd=FIRST_FD;  fd <= LAST_FD;  fd=fd+1)
	    if (fiodes[fd] != NULL)
		if (streq (fname, FNAME(fiodes[fd])))
		    call filerr (fname, SYS_FCLOBOPNFIL)

	# If file clobbering is disabled, make sure file does not exist,
	# otherwise try to clobber the file if it exists.  No clobber
	# checking is performed for special devices.  If "multversions" is
	# disabled we assume that the OS will open a new version of the
	# file rather than overwrite the old one, and the clobber error
	# is defeated.

	if (access (fname,0,0) == YES)
	    if (envgetb ("clobber")) {
		iferr (call delete (fname))
		    call filerr (fname, SYS_FCANTCLOB)
	    } else if (!envgetb ("multversions"))
		call filerr (fname, SYS_FCLOBBER)
end
