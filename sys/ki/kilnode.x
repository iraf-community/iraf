# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "ki.h"

# KI_LOCALNODE -- Determine if the named node is the local node.

int procedure ki_localnode (node)

char	node[ARB]		# node name

int	ch, i
bool	streq()
int	ki_mapname()
include	"kinode.com"

begin
	if (n_local == NULL) {
	    # If local node is not identified in the host name table, this
	    # effectively disables networking.  All nodes names are assumed
	    # to be local.

	    return (YES)

	} else {
	    # We have a node name and the local node is identified in the
	    # node table.  Scan the aliases of the local node to see if the
	    # referenced node is the same.

	    # Map possible logical node name.
	    if (ki_mapname (node, n_nodename, SZ_ALIAS) <= 0)
		call strcpy (node, n_nodename, SZ_ALIAS)

	    ch = n_nodename[1]
	    do i = 1, n_nalias[n_local]
		if (n_alias[1,i,n_local] == ch)
		    if (streq (n_alias[1,i,n_local], n_nodename))
			return (YES)
	}

	return (NO)
end
