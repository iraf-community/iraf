# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>

# PROPEN -- Open a connected subprocess.  Call PROPCPR to spawn the child
# process and connect the input and output IPC channels to FIO file
# descriptors, then pass the environment list and current working directory
# to the child.  The child is left waiting for the next command from the
# parent, which is eventually written to the OUT channel by the parent.
#
# N.B.: If the child cannot process a SET or CHDIR command it is will take
# a panic exit, raising the X_IPC exception in the parent.  This is necessary
# to avoid filling the IN (childs out) IPC channel, which would cause deadlock
# if the parent were to fill the other channel with SET commands.  Output of
# SET commands w/o handshaking is desirable to minimize context switches and
# IPC records and hence speed up process startup.

int procedure propen (process, in, out)

char	process[ARB]		# filename of executable file
int	in, out			# input, output file descriptors to child

int	pid, print_redefined_variables, status
pointer	sp, cwd
int	propcpr()
data	print_redefined_variables /NO/
errchk	propcpr, envlist, putline, putci

begin
	call smark (sp)
	call salloc (cwd, SZ_PATHNAME, TY_CHAR)

	# Connect the subprocess with read and write IPC channels.

	pid = propcpr (process, in, out)

	# Pass the environment list to the child, omitting all but the most
	# recent definitions of each variable.  The list is passed in the
	# opposite order from which it was redefined, but it does not matter
	# since there is only one entry for each variable.

	call envlist (out, "set ", print_redefined_variables)

	# Set the current working directory in the child, in case the OS does
	# not do so, and to save the child the need to ask the kernel for the
	# cwd name, an expensive operation on some systems.

	call zfgcwd (Memc[cwd], SZ_PATHNAME, status)
	call strupk (Memc[cwd], Memc[cwd], SZ_PATHNAME)
	call putline (out, "chdir ")
	call putline (out, Memc[cwd])
	call putci (out, '\n')

	# The command "_go_" must be sent to the child to signal that process
	# startup is completed.  The process STDOUT and STDERR are redirected
	# into the nullfile during startup, hence we will see no output if
	# this command is not sent.

	call putline (out, "_go_\n")

	# Flush the output so the child can munch on this while we go off and
	# do something else.
	call flush (out)

	call sfree (sp)
	return (pid)
end
