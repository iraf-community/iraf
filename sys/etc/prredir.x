# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<syserr.h>

# PRREDIR -- Redirect the pseudofile stream of a connected subprocess.  A newly
# connected subprocess inherits the pseudofile streams of the parent, i.e.,
# a write to STDOUT by the child will be directed to the STDOUT of the parent.
# Note that unlike FREDIR, the destination stream must already be open and
# is unaffected by the redirection of the pseudofile (the pseudofile stream is
# redirected into the existing stream).  The destination file need not be of
# the same type (binary) as the pseudofile, unless the pseudofile stream
# contains binary data.

procedure prredir (pid, stream, new_fd)

int	pid			# process-id of child
int	stream			# stream to be redirected (STDIN, STDOUT, etc)
int	new_fd			# destination FD (already opened)

int	pr
int	pr_findproc()
include	"prc.com"
errchk	syserr

begin
	pr = pr_findproc (pid)
	if (pr == ERR)
	    call syserr (SYS_PRNOTFOUND)

	pr_pstofd[pr,stream] = new_fd
end
