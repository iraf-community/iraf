# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<clset.h>

# XTTYSIZE -- Query the size of the terminal screen in characters.  This is
# different than simply reading the screen size from the environment or from
# termcap, because the screen size will be queried at runtime if the terminal
# has a screen which can change size at runtime.  Note that when this routine
# is called, the variables ttyncols and ttynlines are updated in the IRAF
# environment, allowing ordinary envgeti calls to be used thereafter to query
# the screen size.  The XTTYSIZE routine should not be called all over the
# place, because it may involve i/o to the terminal.

procedure xttysize (width, height)

int	width			# width of screen (out)
int	height			# height of screen (out)

int	junk, i
pointer	sp, buf, tty
pointer	ttyodes()
int	clstati(), getline(), envgeti(), envscan()
errchk	clcmd, getline, envgeti, ttyodes

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)

	# If we are a connected subprocess it is difficult to write directly
	# to the terminal, since the terminal is opened by the CL.  Hence we
	# have the CL run the STTY task instead to reset the screen size
	# parameters in the environment.  If we are not a connected subprocess
	# we query the terminal size directly, assuming that the terminal is
	# opened on the process standard input and output.

	if (clstati (CL_PRTYPE) == PR_CONNECTED) {
	    call clcmd ("stty resize")
	    do i = 1, 2
		if (getline (CLIN, Memc[buf]) != EOF)
		    junk = envscan (Memc[buf])
	    width  = envgeti ("ttyncols")
	    height = envgeti ("ttynlines")

	} else {
	    tty = ttyodes ("terminal")
	    call ttygsize (STDIN, STDOUT, tty, width, height)
	    call ttycdes (tty)
	}

	call sfree (sp)
end
