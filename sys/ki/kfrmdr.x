# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"ki.h"

# KFRMDR -- Remove a directory.

procedure kfrmdr (osfn, status)

char	osfn[ARB]		# packed os filename of directory
int	status			# ok or err

int	server
int	ki_connect(), ki_sendrcv()
include	"kii.com"

begin
	server = ki_connect (osfn)

	if (server == NULL) {
	    call strpak (p_sbuf[p_arg[1]], p_sbuf, SZ_SBUF)
	    call zfrmdr (p_sbuf, status)

	} else {
	    if (ki_sendrcv (server, KI_ZFRMDR, 0) == ERR)
		status = ERR
	    else
		status = p_arg[1]
	}
end
