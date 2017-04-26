# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"ki.h"

# KFPROT -- Set or query file protection.

procedure kfprot (osfn, protflag, status)

char	osfn[ARB]		# packed os filename
int	protflag		# set/query flag
int	status			# answer; yes or no

int	server
int	ki_connect(), ki_sendrcv()
include	"kii.com"

begin
	server = ki_connect (osfn)

	if (server == NULL) {
	    call strpak (p_sbuf[p_arg[1]], p_sbuf, SZ_SBUF)
	    call zfprot (p_sbuf, protflag, status)

	} else {
	    p_arg[2]  = protflag

	    if (ki_sendrcv (server, KI_ZFPROT, 0) == ERR)
		status = ERR
	    else
		status = p_arg[1]
	}
end
