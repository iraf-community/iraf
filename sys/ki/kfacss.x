# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"ki.h"

# KFACSS -- Determine the accessibility and type of a file.

procedure kfacss (osfn, mode, type, status)

char	osfn[ARB]		# packed os filename
int	mode			# access mode or null if don't care
int	type			# file type or null if don't care
int	status			# answer; yes or no

int	server
int	ki_connect(), ki_sendrcv()
include	"kii.com"

begin
	server = ki_connect (osfn)

	if (server == NULL) {
	    call strpak (p_sbuf[p_arg[1]], p_sbuf, SZ_SBUF)
	    call zfacss (p_sbuf, mode, type, status)

	} else {
	    p_arg[2] = mode
	    p_arg[3] = type

	    if (ki_sendrcv (server, KI_ZFACSS, 0) == ERR)
		status = ERR
	    else
		status = p_arg[1]
	}
end
