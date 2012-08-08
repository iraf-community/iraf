# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>
include	<config.h>


.help futime
.nf ___________________________________________________________________________
FUTIME -- Set the file access/modify times of a file.  Time arguments are
assumed to be in units of seconds from midnight on Jan 1, 1980, local standard
time.  A file may be "touched" to update it's modify time to the current
clock time using the CLKTIME function with a call such as

	stat = futime (fname, NULL, clktime(0))

Remote files are handled via the KI interface automatically.
.endhelp ______________________________________________________________________

int procedure futime (fname, atime, mtime)

char	fname[ARB]
long	atime, mtime
int	status
include	<fio.com>

begin
	iferr (call fmapfn (fname, pathname, SZ_PATHNAME))
	    return (ERR)

	# Update the time, let the HSI routine handle NULL values.
	call zfutim (pathname, atime, mtime, status)

	return (status)
end
