# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>
include	<clset.h>
include	<fset.h>

define	NFD		2

# ONENTRY -- Default procedure called by the IRAF Main during process startup,
# before entering the interpreter loop.  If desired the user can supply their
# own ONENTRY procedure and the standard library version will not be linked.
# This procedure is a no-op for a connected or host process.  For a detached
# process the default action is to redirect the standard input to the bkgfile,
# which is assumed to be a text file containing commands to be executed by
# the Main.

int procedure onentry (prtype, bkgfile)

int	prtype			# process type (connected, detached, host)
char	bkgfile[ARB]		# osfn of bkg file, if detached process

char	osfn[SZ_FNAME]
int	chan, loc_zgettx, i, fd[NFD]
extern	zgettx()
data	fd[1] /CLIN/, fd[2] /STDIN/

begin
	if (prtype == PR_DETACHED) {
	    # Open the bkgfile and connect it to CLIN and STDIN.  The stdin
	    # supplied by the process main is not used in this mode.
	    # We assume that no i/o has yet occurred on either file.  Note
	    # that we do not wish to use FREDIR as that would preclude
	    # redirection on the command line.

	    call strpak (bkgfile, osfn, SZ_FNAME)
	    call zopntx (osfn, READ_ONLY, chan)
	    if (chan == ERR)
		call sys_panic (0, "Cannot open bkgfile")
	    call zlocpr (zgettx, loc_zgettx)

	    do i = 1, NFD {
		call fseti (fd[i], F_CHANNEL, chan)
		call fseti (fd[i], F_DEVICE,  loc_zgettx)
		call fseti (fd[i], F_TYPE,    TEXT_FILE)
	    }
	}

	# If PR_EXIT is returned the interpreter loop is bypassed and process
	# shutdown occurs immediately.

	return (PR_NOEXIT)
end
