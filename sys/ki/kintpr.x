# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	"ki.h"

# KINTPR -- Send a signal (interrupt) to a connected subprocess.

procedure kintpr (pid, vex, status)

int	pid			# process id
int	vex			# virtual exception
int	status			# exit status of the job

int	server
int	ki_sendrcv()
include	"kichan.com"
include	"kii.com"

begin
	server = k_node[pid]

	if (server == NULL) {
	    call zintpr (k_oschan[pid], vex, status)

	} else {
	    p_arg[1]  = k_oschan[pid]
	    p_arg[2]  = vex
	    p_sbuflen = 0

	    if (ki_sendrcv (server, KI_ZINTPR, 0) == ERR)
		status = ERR
	    else
		status = p_arg[1]
	}
end
