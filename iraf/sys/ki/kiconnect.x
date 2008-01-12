# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	<chars.h>
include	"ki.h"

# KI_CONNECT -- Given a resource name (e.g., a VFN, OSFN, or any string
# which might begin with a node name prefix) extract the node name alias,
# if any, and return the node index of the kernel server for the node.
# NULL is returned if the resource does not reside on a remote node,
# if the named node is not recognized, if the resource resides on the local
# node, or if a kernel server cannot be spawned on the node.
#
# SIDE-EFFECTS:  In all cases the unpacked name, minus the node name prefix,
# is left in the field P_SBUF of the kschan common, i.e., in the packet to be
# sent to the node to service the request.  This stripped name should be used
# even if the resource is local since the node prefix must not be passed to the
# iraf kernel.  If the resource resides on a remote node the P_ARG[1] field is
# set to 1 (the index of the string in the string buffer) and the P_SBUFLEN
# field is set to the length of the unpacked string plus 1 for the EOS.

int procedure ki_connect (rname)

char	rname[ARB]		# packed resource name, e.g., a filename

pointer	sp, sbuf, op
char	alias[SZ_ALIAS]
int	node, delim, junk, nlookup
int	ki_findnode(), ki_openks(), ki_gnode(), ki_gethosts()
int	strlen(), gstrcpy()

include	"kinode.com"
include	"kichan.com"
include	"kii.com"
define	again_ 91

begin
	# Read dev$hosts if it has not already been read.
	if (n_nnodes == 0)
	    junk = ki_gethosts()

	# Unpack rname into the string buffer, search for the node character.
	# The call to KI_GETHOSTS will fail during process startup until the
	# environment variable "iraf" is defined.  This is harmless provided
	# only local files are referenced during process startup.  Do not
	# move this initialization code to ki_init() or the host name table
	# will never be read and networking will never be turned on.  Return
	# immediately if no nodechar found.
	#
	# NOTE: we are required to always leave the unpacked, node prefix
	# stripped resource name in p_sbuf, whether or not the resource is
	# on a remote node.

	call strupk (rname, p_sbuf, SZ_SBUF)
	nlookup = 0
again_
	if (ki_gnode (p_sbuf, alias, delim) == LOCAL) {
	    p_arg[1] = delim + 1
	    return (NULL)
	} else
	    p_arg[1] = delim + 1

	# Find node descriptor (initialized by ki_gethosts above).  NULL is
	# returned for ND if the node is not found or if the node is the
	# local node.

	node = ki_findnode (alias)
	if (node == n_local)
	    node = NULL
	else if (n_server[1,node] == '@') {
	    # The node entry is a route to another node.  This is an entry
	    # such as "node : @foo!node".  We replace the "node" prefix in
	    # rname by whatever is to the right of the @, and repeat the
	    # host lookup.  In the example this would have the effect of
	    # changing the "node!object" reference to "foo!node!object" and
	    # hence routing traffic for node "node" through node "foo".  
	    # This is often done when a node is not directly reachable on
	    # the local network.

	    call smark (sp)
	    call salloc (sbuf, SZ_SBUF, TY_CHAR)

	    op = sbuf + gstrcpy (n_server[2,node], Memc[sbuf], SZ_SBUF)
	    Memc[op] = '!';  op = op + 1
	    call strcpy (p_sbuf[delim+1], Memc[op], SZ_SBUF-(op-sbuf))
	    call strcpy (Memc[sbuf], p_sbuf, SZ_SBUF)

	    call sfree (sp)

	    nlookup = nlookup + 1
	    if (nlookup > MAX_INDIRECT)
		node = NULL
	    else
		goto again_
	}

	# Initialize the remainder of the packet descriptor variables dealing
	# with the resource name.  The node alias is left in the string buffer
	# at offset 1, now terminated with an EOS.  This is followed by RNAME
	# minus the node name prefix.  Note that RNAME may contain additional
	# indirection; only the first node name is processed locally.

	if (node != NULL) {
	    # Resource resides on a remote node.  Connect the kernel server
	    # if not already connected.

	    p_sbuflen = strlen (p_sbuf)
	    if (n_kschan[node] == NULL)
		if (ki_openks (node) == ERR)
		    node = NULL
	}

	return (node)
end
