# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"ki.h"

# KDVOWN -- Query device allocation; return the owner name if the device is
# allocated to someone else.

procedure kdvown (device, owner, maxch, status)

char	device[ARB]		# packed device name string
char	owner[ARB]		# receives owner name string
int	maxch			# max chars out
int	status			# allocation status (<xalloc.h>)

int	server
int	ki_connect(), ki_sendrcv()
include	"kii.com"

begin
	server = ki_connect (device)

	if (server == NULL) {
	    call strpak (p_sbuf[p_arg[1]], p_sbuf, SZ_SBUF)
	    call zdvown (p_sbuf, owner, maxch, status)

	} else {
	    p_arg[2] = maxch
	    if (ki_sendrcv (server, KI_ZDVOWN, 0) == ERR) {
		owner[1] = EOS
		status = ERR
	    } else {
		status = p_arg[1]
		call strpak (p_sbuf, owner, maxch)
	    }
	}
end
