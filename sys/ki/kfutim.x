# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"ki.h"

# KFUTIM -- Set the file access/modify times of a file.   Time arguments are
# assumed to be in units of seconds from midnight on Jan 1, 1980, LST.
#
# NOTE:  Since the atime/mtime values are long but the p_arg[] is just int
# this code may need to be revised if the sizes of long/int change.

procedure kfutim (osfn, atime, mtime, status)

char	osfn[ARB]		# packed os filename
long	atime, mtime		# access and modify times
int	status			# answer; ok or err

int	server
int	ki_connect(), ki_sendrcv()
include	"kii.com"

begin
	server = ki_connect (osfn)

	if (server == NULL) {
	    call strpak (p_sbuf[p_arg[1]], p_sbuf, SZ_SBUF)
	    call zfutim (p_sbuf, atime, mtime, status)

	} else {
	    p_arg[2] = atime
	    p_arg[3] = mtime

	    if (ki_sendrcv (server, KI_ZFUTIM, 0) == ERR)
		status = ERR
	    else
		status = p_arg[1]
	}
end
