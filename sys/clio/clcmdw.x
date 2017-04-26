# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<clset.h>

# CLCMDW -- Send a command line to the CL and wait for completion.

procedure clcmdw (cmd)

char	cmd[ARB]
char	junkstr[1]

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
	    call clgstr ("cl.version", junkstr, 1)	# wait for completion
	}
end
