# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	<chars.h>
include	"mtio.h"

# MT_LOCKNAME -- Generate the file name of the magtape lock file, given the
# logical drive name.  We are called from a z-routine, so do not use any high
# level i/o routines.  The generated lockfile name is of the form
#
#		[node!]dir$mta.lok
#
# The lock file is maintained on the same node as the drive to which it
# refers.

procedure mt_lockname (device, lockfile, maxch)

char	device[ARB]		#I device name
char	lockfile[maxch]		#O receives generated lockfile name
int	maxch			#I max chars out

int	ip, op
int	gstrcpy(), strlen()

begin
	lockfile[1] = EOS

	# Copy the node name prefix, if any.
	call ki_xnode (device, lockfile, maxch)
	op = strlen (lockfile) + 1
	ip = op

	# Add the directory name prefix, "mt", and device name.
	op = op + gstrcpy (LOCKLDIR, lockfile[op], maxch-op+1)
	op = op + gstrcpy (LOCKFILE,  lockfile[op], maxch-op+1)
	op = op + gstrcpy (device[ip], lockfile[op], maxch-op+1)

	# Add file extension.
	op = op + gstrcpy (LOCKEXTN,  lockfile[op], maxch-op+1)
end
