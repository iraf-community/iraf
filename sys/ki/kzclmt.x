# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	<fio.h>
include	"ki.h"

# KZCLMT -- Close a magtape file.

procedure kzclmt (chan, devpos, status)

int	chan			#I channel to be closed
int	devpos[ARB]		#O receives position information
int	status			#O close status

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

	if (k_node[chan] == NULL)
	    call zzclmt (k_oschan[chan], devpos, status)
	else {
	    server   = k_node[chan]
	    p_arg[1] = k_oschan[chan]

	    if (ki_sendrcv (server, KI_ZFIOMT, MT_CL) == ERR)
		status = ERR
	    else {
		status = p_arg[1]
		call amovi (p_arg[2], devpos, LEN_MTDEVPOS)
	    }

	    call mfree (k_bufp[chan], TY_INT)
	}

	call ki_freechan (chan)
end
