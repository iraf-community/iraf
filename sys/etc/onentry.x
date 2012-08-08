# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>
include	<error.h>
include	<clset.h>
include	<fset.h>

define	NFD		2

# ONENTRY -- Default procedure called by the IRAF Main during process startup,
# before entering the interpreter loop.  If desired the user can supply their
# own ONENTRY procedure; this will be used instead of the system default if
# specified on the link line before the iraf libraries are searched.
# This procedure is a no-op for a connected or host process.  For a detached
# process the default action is to redirect the standard input to the bkgfile,
# which is assumed to be a text file containing commands to be executed by
# the Main.
#
# The basic host calling sequence for an iraf process is as follows:
#
#	 x_file.e [-c | -d bkgfile ] [ command ]
#
# This is parsed by the zmain (host level main), returning the process type
# in PRTYPE, the bkgfile string in BKGFILE if the process type is detached,
# and anything remaining on the command line in CMD.  If a custom onentry
# procedure is used CMD can be anything; all the iraf main does is concatenate
# the arguments into a string and pass it to the onentry procedure as CMD.

int procedure onentry (prtype, bkgfile, cmd)

int	prtype			#I process type (connected, detached, host)
char	bkgfile[ARB]		#I osfn of bkg file, if detached process
char	cmd[ARB]		#I command argument string, if any

char	osfn[SZ_FNAME]
int	chan, loc_zgettx, i, fd[NFD]
data	fd[1] /CLIN/, fd[2] /STDIN/
extern	zgettx()

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
		call sys_panic (EA_FATAL, "Cannot open bkgfile")
	    call zlocpr (zgettx, loc_zgettx)

	    do i = 1, NFD {
		call fseti (fd[i], F_CHANNEL, chan)
		call fseti (fd[i], F_DEVICE, loc_zgettx)
		call fseti (fd[i], F_TYPE, TEXT_FILE)
	    }
	}

	# If PR_EXIT is returned the interpreter loop is bypassed and process
	# shutdown occurs immediately.

	return (PR_NOEXIT)
end
