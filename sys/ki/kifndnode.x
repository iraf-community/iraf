# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "ki.h"

# KI_FINDNODE -- Given an alias for a node, search the node descriptor table
# and return the table index of the named node or NULL.

int procedure ki_findnode (alias)

char	alias[ARB]		# alias to search for

int	node, i
char	first_char
bool	streq()
int	ki_gethosts(), ki_mapname(), and()
include	"kinode.com"

begin
	# Read the host name table if it has not been read yet.
	if (n_nnodes == 0)
	    if (ki_gethosts() == ERR)
		return (NULL)

	# Map possible logical node name.
	if (ki_mapname (alias, n_nodename, SZ_ALIAS) <= 0)
	    call strcpy (alias, n_nodename, SZ_ALIAS)

	# Search the node descriptor table for a node with the given alias.
	# Do not use descriptors that have the IOERR flag set.

	first_char = n_nodename[1]
	for (node=1;  node <= n_nnodes;  node=node+1)
	    for (i=1;  i <= n_nalias[node];  i=i+1)
		if (first_char == n_alias[1,i,node])
		    if (streq (n_nodename, n_alias[1,i,node]))
			if (and (n_status[node], F_IOERR) == 0)
			    return (node)

	return (NULL)
end
