# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>

# PROSCMD -- Process an OS escape command from a subprocess.  Execute the
# command and return the exit status to the subprocess via IPC.

procedure proscmd (pr, cmd)

int	pr		# subprocess process slot number
char	cmd[ARB]	# host command to be executed

char	statbuf[5]
int	fd, status, op
int	itoc(), oscmd()
include	"prc.com"

begin
	fd = pr_outfd[pr]

	# Execute the command (waits for completion).
	status = oscmd (cmd, "", "", "")

	# Encode the return status.
	op = itoc (status, statbuf, 5) + 1
	statbuf[op] = '\n'
	statbuf[op+1] = EOS

	# Return the status to the subprocess.
	call write (fd, statbuf, op)
	call flush (fd)
end
