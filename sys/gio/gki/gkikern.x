# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<gki.h>

# GKI_SUBKERNEL -- Identify a graphics stream for use with a kernel in a
# connected subprocess of the current process.  This type of kernel is
# equivalent to a file for all of the output instructions, but the input
# instructions (e.g., read cursor) must fiddle with process i/o and need
# additional information to do so, i.e., the process id number of the kernel
# process, and the entry point address of the PR_PSIO procedure.  We do not
# wish to directly reference the latter procedure as this would require
# all processes which use GKI to link in the process control code, even if
# they never talk directly to a process.  Note that processes which talk to
# an external kernel via the CL do so with the normal file interface, hence
# do not need to call us.  We are called by the GIOTR (cursor mode) code in
# the CL process when an external kernel is spawned.

procedure gki_subkernel (stream, pid, prpsio_epa)

int	stream			# graphics stream to be redirected
int	pid			# process id of kernel process
int	prpsio_epa		# epa of the etc$prpsio procedure.
include	"gki.com"

begin
	gk_type[stream] = pid
	gk_fd[stream] = stream
	gk_prpsio = prpsio_epa
end
