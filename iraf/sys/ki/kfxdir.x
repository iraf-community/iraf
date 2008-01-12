# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<chars.h>
include	"ki.h"

# KFXDIR -- Extract the OSDIR prefix (if any) from a filename.  If the VFN
# has a node prefix and the node named is not the local node then the entire
# filename is treated as an OSDIR name.  We are called during filename mapping
# to determine if a VFN is an OSFN; returning nchars > 0 when there is a node
# name prefix causes filename mapping to be deferred until the filename is
# passed to the kernel server on the remote node.
#
# NOTE -- The "nchars" returned is the length of the osdir prefix portion of
# the VFN string, NOT the length of the returned string.  ZFXDIR is used to
# test if a VFN has an OSDIR prefix and if so, to determine the string offset
# of the root field. 

procedure kfxdir (vfn, osdir, maxch, nchars)

char	vfn[ARB]		# virtual filename
char	osdir[maxch]		# receives os directory prefix
int	maxch			# max chars out
int	nchars			# receives length of osdir prefix in VFN string

int	delim, op
int	ki_gnode(), gstrcpy(), ki_gethosts()
include	"kinode.com"

begin
	repeat {
	    if (ki_gnode (vfn, osdir, delim) == LOCAL) {
		# File is on the local node.  Must strip the node prefix,
		# if any, before calling zfxdir, but keep the node prefix
		# in the output pathname else the next operator will assume
		# the default node.

		for (op=1;  osdir[op] != EOS;  op=op+1)
		    ;
		if (op > 1) {
		    osdir[op] = FNNODE_CHAR
		    op = op + 1
		}

		call zfxdir (vfn[delim+1], osdir[op], maxch-op+1, nchars)
		if (nchars == 0)
		    osdir[1] = EOS
		else
		    nchars = nchars + delim

		break

	    } else {
		# Verify that the host name table has been read and if not,
		# read it and try again.

		if (n_nnodes == 0)
		    if (ki_gethosts() != ERR)
			next

		# File is on a remote node.  Concatenate node name and filename
		# and return the entire string as the "osdir" string, disabling
		# filename mapping on the local node.

		for (op=1;  osdir[op] != EOS;  op=op+1)
		    ;

		if (op > 1) {
		    osdir[op] = FNNODE_CHAR
		    op = op + 1
		}

		nchars = gstrcpy (vfn[delim+1], osdir[op], maxch-op+1) + delim
		break
	    }
	}
end
