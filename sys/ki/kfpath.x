# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<chars.h>
include	"ki.h"

# KFPATH -- Convert a VFN into a net pathname.  The VFN may contain a node
# pathname and the final pathname will always include the node name.	

procedure kfpath (vfn, osfn, maxch, nchars)

char	vfn[ARB]		# virtual filename
char	osfn[maxch]		# receives pathname
int	maxch			# max chars out
int	nchars			# receives length of osfn string

int	delim, op, nodeflag, junk
int	ki_gnode(), gstrcpy(), ki_gethosts()
include	"kinode.com"

begin
	# Read the host name table if it has not been read yet.
	if (n_nnodes == 0)
	    junk = ki_gethosts()

	# If no VFN is given return the current working directory.
	if (vfn[1] == EOS) {
	    call kfgcwd (osfn, maxch, nchars)
	    call strupk (osfn, osfn, maxch)
	    return
	}

	# Determine what node the given VFN resides on.
	nodeflag = ki_gnode (vfn, osfn, delim)

	# Append the node delimiter to the node name if there is a node name.
	for (op=1;  osfn[op] != EOS;  op=op+1)
	    ;
	if (op > 1) {
	    osfn[op] = FNNODE_CHAR
	    op = op + 1
	}

	if (nodeflag == LOCAL) {
	    # File is on the local node.  Return the mapped pathname with
	    # the node name of the local node prepended.

	    call zfpath (vfn[delim+1], osfn[op], maxch-op+1, nchars)
	    nchars = nchars + op - 1

	} else {
	    # File is on a remote node.  Do not map the filename; leave that
	    # to the kernel when the file is referenced at runtime.

	    nchars = gstrcpy (vfn[delim+1], osfn[op], maxch-op+1) + op - 1
	}
end
