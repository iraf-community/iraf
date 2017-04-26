# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	"ki.h"

# KCLDIR -- Close a directory file.

procedure kcldir (chan, status)

int	chan			# channel descriptor
int	status			# answer; ok or err

int	server
int	ki_sendrcv()
include	"kichan.com"
include	"kii.com"

begin
	# Possible if an abort occurs during the open.
	if (chan <= 0) {
	    status = OK
	    return
	}

	server = k_node[chan]

	if (server == NULL) {
	    call zcldir (k_oschan[chan], status)

	} else {
	    p_arg[1]  = k_oschan[chan]
	    p_sbuflen = 0

	    if (ki_sendrcv (server, KI_ZCLDIR, 0) == ERR)
		status = ERR
	    else
		status = p_arg[1]

	    call mfree (k_bufp[chan], TY_STRUCT)
	}

	call ki_freechan (chan)
end
