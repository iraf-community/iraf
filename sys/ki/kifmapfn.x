# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<mach.h>
include	"ki.h"

# KI_FMAPFN -- Fully resolve a filename into its host equivalent, regardless
# of the node on which the file resides.  This is a temporary routine.
# The filename mapping primitives should probably perform their function when
# called for a remote file, rather than defer the mapping until later as they
# do now.  When this is changed, this routine will no longer be necessary and
# can be removed.  The output filename is returned as a packed filename with
# the node name stripped, as for FMAPFN.

procedure ki_fmapfn (vfn, pkosfn, maxch)

char	vfn[ARB]		# network filename
char	pkosfn[maxch]		# receives packed, fully resolved OS filename
int	maxch

int	server
int	ki_connect(), ki_sendrcv()
include	"kii.com"

begin
	call strpak (vfn, pkosfn, maxch)
	server = ki_connect (pkosfn)

	if (server == NULL)
	    call fmapfn (vfn, pkosfn, maxch)
	else {
	    p_arg[2] = maxch
	    if (ki_sendrcv (server, KI_FMAPFN, 0) == ERR)
		call syserrs (SYS_FNOSUCHFILE, vfn)
	    else
		call strpak (p_sbuf, pkosfn, maxch)
	}
end
