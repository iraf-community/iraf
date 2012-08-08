# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<clset.h>

# CLCMD -- Send a command line to the CL.  Virtually any general command
# may be sent to the CL, providing a great deal of high level power at the
# compiled task level.  Sending an explicit command to the CL, however,
# requires that the task have detailed knowledge of the capabilities of
# the CL and of the syntax of the command language.  This means that the task
# is very dependent on the CL and may no longer work if the CL is modified,
# or if there is more than one version of the CL in use in a system.  For
# this reason CLCMD should only be used where it is truely necessary,
# usually only in system utilities (for example, in a task like MAKE).

procedure clcmd (cmd)

char	cmd[ARB]

int	junk
int	oscmd(), clstati()
errchk	syserr

begin
	if (cmd[1] == '!')
	    junk = oscmd (cmd[2], "", "", "")
	else if (clstati (CL_PRTYPE) != PR_CONNECTED)
	    call syserr (SYS_CLCMDNC)
	else {
	    call flush (STDOUT)
	    call putline (CLOUT, cmd)
	    call putci (CLOUT, '\n')
	    call flush (CLOUT)
	}
end
