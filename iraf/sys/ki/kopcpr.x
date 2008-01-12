# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	"ki.h"

# KOPCPR -- Open a connected subprocess.

procedure kopcpr (process, inchan, outchan, pid)

char	process[ARB]		# packed osfn of process executable
int	inchan, outchan		# receives process input output channels
int	pid			# receives process id

int	server
int	ki_connect(), ki_sendrcv(), ki_getchan()
include	"kichan.com"
include	"kii.com"

begin
	server = ki_connect (process)

	if (server == NULL) {
	    call strpak (p_sbuf[p_arg[1]], p_sbuf, SZ_SBUF)
	    call zopcpr (p_sbuf, inchan, outchan, pid)
	} else {
	    if (ki_sendrcv (server, KI_ZOPCPR, 0) == ERR)
		pid = ERR
	    else {
		pid     = p_arg[1]
		inchan  = p_arg[2]
		outchan = p_arg[3]
	    }
	}

	# Allocate 3 channel descriptors, one for the each of the i/o
	# channels and another for the PID.  Save the channel descriptor
	# numbers of the i/o channels in the status field of the PID
	# descriptor to permit freeing the lot at disconnect time.

	if (pid != ERR) {
	    pid     = ki_getchan (server, pid)
	    inchan  = ki_getchan (server, inchan)
	    outchan = ki_getchan (server, outchan)
	    k_status[pid] = inchan * MAX_CHANNELS + outchan
	}
end
