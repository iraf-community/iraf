# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<error.h>

# PRUPDATE -- Broadcast a message to a process, or if pid=0, to all connected
# subprocesses.  Used primarily to incrementally pass SET and CHDIR commands to
# subprocesses, eliminating the need to reconnect each process.  Note that the
# child process does not return "bye" in response to one of the builtin
# functions SET and CHDIR.  NOTE: we assume that we are called when all child
# processes are idle.

procedure prupdate (pid, message)

int	pid			# process to be updated, or 0 for all procs
char	message[ARB]		# message to be broadcast to each child

int	pr
pointer	sp, cmd, op
int	gstrcpy()
include	"prc.com"

begin
	call smark (sp)
	call salloc (cmd, SZ_COMMAND, TY_CHAR)

	# Make sure that the message string is non-null and is newline
	# delimited.

	op = cmd + gstrcpy (message, Memc[cmd], SZ_COMMAND)
	if (op == cmd) {
	    call sfree (sp)
	    return
	} else if (Memc[op-1] != '\n') {
	    Memc[op] = '\n'
	    Memc[op+1] = EOS
	}

	# Broadcast the message.  If the child fails to process the command
	# and returns the ERROR statement, the error will not be detected until
	# the next user command is sent to the process (and indeed may corrupt
	# the protocol).  The parent should execute the SET or CHDIR prior
	# to sending it to the child to make sure it is valid.

	for (pr=1;  pr <= MAX_CHILDPROCS;  pr=pr+1)
	    if ((pid != NULL && pr_pid[pr] == pid) ||
		(pid == NULL && pr_pid[pr] != NULL))
		iferr (call putline (pr_outfd[pr], Memc[cmd]))
		    call erract (EA_WARN)

	call sfree (sp)
end
