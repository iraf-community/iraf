# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	<chars.h>
include	"mtio.h"

# MT_LOCKNAME -- Generate the file name of the magtape lock file, given the
# logical drive name.  We are called from a z-routine, so do not use any high
# level i/o routines.  The generated lockfile name is one of the forms
#
#		dir$mta.lok
# 	or	dir$mta_node.lok
#
# depending upon whether a node prefix is present in the drive name string,
# e.g., whether the drive is local or remote.  Note that the "lock" file is
# always maintained on the current host, even if the drive is remote, because
# the named directory is normally a user directory and may not exist on the
# remote host.

procedure mt_lockname (drive, lockfile, maxch)

char	drive[ARB]		# drive name
char	lockfile[maxch]		# receives generated lockfile name
int	maxch			# max chars out

pointer	sp, node, cp
int	ip, op, nchars
int	ki_extnode(), gstrcpy()

begin
	call smark (sp)
	call salloc (node, SZ_FNAME, TY_CHAR)

	# Extract the node name prefix, if any.
	ip = ki_extnode (drive, Memc[node], SZ_FNAME, nchars) + 1

	# Begin with the directory name prefix, "mt", and drive name.
	op = gstrcpy (LOCKLDIR, lockfile, maxch) + 1
	op = op + gstrcpy (LOCKFILE,  lockfile[op], maxch-op+1)
	op = op + gstrcpy (drive[ip], lockfile[op], maxch-op+1)

	# Add the node name as the root lockfile filename, minus any funny
	# characters that may be in the node name (such as - and the !).

	if (nchars > 0) {
	    lockfile[op] = '_'
	    op = min (maxch, op + 1)
	    for (cp=node;  Memc[cp] != EOS;  cp=cp+1)
		if (IS_ALNUM(Memc[cp])) {
		    lockfile[op] = Memc[cp]
		    op = min (maxch, op + 1)
		}
	}

	# Add file extension.
	op = op + gstrcpy (LOCKEXTN,  lockfile[op], maxch-op+1)

	call sfree (sp)
end
