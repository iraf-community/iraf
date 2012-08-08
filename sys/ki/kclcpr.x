# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	"ki.h"

# KCLCPR -- Close a connected subprocess.

procedure kclcpr (pid, exit_status)

int	pid			# channel descriptor
int	exit_status		# exit status of the job

int	server, inchan, outchan
int	ki_sendrcv()
include	"kichan.com"
include	"kii.com"

begin
	# Possible if an abort occurs during the open.
	if (pid <= 0) {
	    exit_status = OK
	    return
	}

	server = k_node[pid]

	if (server == NULL) {
	    call zclcpr (k_oschan[pid], exit_status)
	} else {
	    p_arg[1]  = k_oschan[pid]
	    p_sbuflen = 0

	    if (ki_sendrcv (server, KI_ZCLCPR, 0) == ERR)
		exit_status = ERR
	    else
		exit_status = p_arg[1]
	}

	# The channel descriptor numbers of the two cds used for the i/o
	# streams are encoded in the k_status field of the PID cd.

	inchan  = k_status[pid] / MAX_CHANNELS
	outchan = mod (k_status[pid], MAX_CHANNELS)

	# Free the 3 channel descriptors used by the subprocess.
	call ki_freechan (pid)
	call ki_freechan (inchan)
	call ki_freechan (outchan)
end
