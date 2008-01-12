# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"ki.h"

# KFALOC -- Create and preallocate space for a binary file.

procedure kfaloc (osfn, nbytes, status)

char	osfn[ARB]		# packed os filename
int	nbytes			# nbytes of storage to allocate
int	status			# answer; ok or err

int	server
int	ki_connect(), ki_sendrcv()
include	"kii.com"

begin
	server = ki_connect (osfn)

	if (server == NULL) {
	    call strpak (p_sbuf[p_arg[1]], p_sbuf, SZ_SBUF)
	    call zfaloc (p_sbuf, nbytes, status)

	} else {
	    p_arg[2] = nbytes

	    if (ki_sendrcv (server, KI_ZFALOC, 0) == ERR)
		status = ERR
	    else
		status = p_arg[1]
	}
end
