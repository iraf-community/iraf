# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	"ki.h"

# KCLDPR -- Close a detached process.

procedure kcldpr (jobcode, killflag, exit_status)

int	jobcode			# channel descriptor
int	killflag		# kill job or just wait for it to terminate
int	exit_status		# exit status of the job

int	server
int	ki_sendrcv()
include	"kichan.com"
include	"kii.com"

begin
	# Possible if an abort occurs during the open.
	if (jobcode <= 0) {
	    exit_status = OK
	    return
	}

	server = k_node[jobcode]

	if (server == NULL) {
	    call zcldpr (k_oschan[jobcode], killflag, exit_status)

	} else {
	    p_arg[1]  = k_oschan[jobcode]
	    p_arg[2]  = killflag
	    p_sbuflen = 0

	    if (ki_sendrcv (server, KI_ZCLDPR, 0) == ERR)
		exit_status = ERR
	    else
		exit_status = p_arg[1]
	}

	call ki_freechan (jobcode)
end
