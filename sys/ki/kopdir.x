# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	<diropen.h>
include	"ki.h"

# KOPDIR -- Open a directory file for reading.

procedure kopdir (osfn, chan)

char	osfn[ARB]		# packed os filename or directory name
int	chan			# channel assigned for reading filenames

int	server
pointer	dp
int	ki_connect(), ki_sendrcv(), ki_getchan(), kmalloc()
include	"kichan.com"
include	"kii.com"

begin
	server = ki_connect (osfn)

	if (server == NULL) {
	    call strpak (p_sbuf[p_arg[1]], p_sbuf, SZ_SBUF)
	    call zopdir (p_sbuf, chan)

	} else {
	    p_arg[2] = PASS_HIDDEN_FILES

	    if (ki_sendrcv (server, KI_ZOPDIR, 0) == ERR)
		chan = ERR
	    else {
		chan = p_arg[1]
		if (kmalloc (dp, LEN_DIRBDES, TY_STRUCT) == ERR)
		    chan = ERR
		else {
		    D_IP(dp)      = D_DATA(dp)
		    D_ITOP(dp)    = D_DATA(dp)
		    D_EOFSEEN(dp) = NO
		}
	    }
	}

	if (chan != ERR) {
	    chan = ki_getchan (server, chan)
	    if (server != NULL)
		k_bufp[chan] = dp
	}
end
