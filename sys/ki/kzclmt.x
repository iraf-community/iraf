# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	"ki.h"

# KZCLMT -- Close a magtape file.

procedure kzclmt (chan, mode, nrecords, nfiles, status)

int	chan			# channel to be closed
int	mode			# access mode of channel when opened
int	nrecords		# receives nrecords moved by close
int	nfiles			# receives nfiles moved by close
int	status

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
	    call zzclmt (k_oschan[chan], mode, nrecords, nfiles, status)
	else {
	    server   = k_node[chan]
	    p_arg[1] = k_oschan[chan]
	    p_arg[2] = mode

	    if (ki_sendrcv (server, KI_ZFIOMT, MT_CL) == ERR)
		status = ERR
	    else {
		status   = p_arg[1]
		nrecords = p_arg[2]
		nfiles   = p_arg[3]
	    }
	}

	call ki_freechan (chan)
end
