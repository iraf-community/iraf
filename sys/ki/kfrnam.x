# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"ki.h"

define	OFFSET		128	# offset to second filename
define	MAXCH		128	# max chars in filename


# KFRNAM -- Rename a file.  Both filenames must refer to the same node.

procedure kfrnam (old_osfn, new_osfn, status)

char	old_osfn[ARB]		# packed old os filename
char	new_osfn[ARB]		# packed new os filename
int	status			# answer; ok or err

int	server1, server2, old, new
int	ki_connect(), ki_sendrcv()
include	"kii.com"

begin
	server2 = ki_connect (new_osfn)
	call strcpy (p_sbuf[p_arg[1]], p_sbuf[OFFSET], MAXCH)
	server1 = ki_connect (old_osfn)

	old = p_arg[1]
	new = OFFSET

	if (server1 == NULL && server2 == NULL) {
	    # Both files reside on the local node.

	    call strpak (p_sbuf[old], p_sbuf[old], MAXCH)
	    call strpak (p_sbuf[new], p_sbuf[new], MAXCH)
	    call zfrnam (p_sbuf[old], p_sbuf[new], status)

	} else if (server1 == server2) {
	    # Both files reside on the same remote node.
	    p_arg[2]  = new
	    p_sbuflen = SZ_SBUF

	    if (ki_sendrcv (server1, KI_ZFRNAM, 0) == ERR)
		status = ERR
	    else
		status = p_arg[1]

	} else {
	    # One file resides on a remote node.
	    status = ERR
	}
end
