# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>

# PR_GETREDIR -- Get the pseudofile redirection code for a process.

int procedure pr_getredir (pid, stream)

int	pid			# process id
int	stream			# stream for which redirection info is needed

int	pr
int	pr_findproc()
include	"prc.com"

begin
	pr = pr_findproc (pid)
	return (pr_pstofd[pr,stream])
end
