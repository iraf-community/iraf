# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"ki.h"

# KDVALL -- Allocate/deallocate a device.

procedure kdvall (device, allflag, status)

char	device[ARB]		# device name or alias list
int	allflag			# flag: allocate=1, deallocate=0
int	status			# return status

int	server
int	ki_connect(), ki_sendrcv()
include	"kii.com"

begin
	server = ki_connect (device)

	if (server == NULL) {
	    call strpak (p_sbuf[p_arg[1]], p_sbuf, SZ_SBUF)
	    call zdvall (p_sbuf, allflag, status)

	} else {
	    p_arg[2] = allflag
	    if (ki_sendrcv (server, KI_ZDVALL, 0) == ERR)
		status = ERR
	    else
		status = p_arg[1]
	}
end
