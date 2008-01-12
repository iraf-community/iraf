# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<chars.h>
include	"ki.h"

# KI_EXTNODE -- Extract the node name prefix from a resource name of the form
# "node!resource".  The entire prefix is returned, i.e., "node!".  The number
# of chars in the prefix string is returned as the function value; zero is
# returned if there is no node prefix.  The name of the node owning the named
# resource is returned in the output string.  If no node is named in the
# resource name supplied, the name of the default node (usually the local or
# host node) is returned instead.  Note that the function value refers to the
# prefix string, not the string length of the nodename string.

int procedure ki_extnode (resource, nodename, maxch, nchars)

char	resource[ARB]		# name of a resource, with opt. node prefix
char	nodename[maxch]		# receives node name
int	maxch			# max chars out
int	nchars			# receives nchars in nodename string

char	alias[SZ_ALIAS]
int	delim, op, junk, node
int	ki_gnode(), ki_findnode(), gstrcpy()
include	"kinode.com"

begin
	# Extract node name prefix, if any, and replace by primary alias.
	junk = ki_gnode (resource, alias, delim)
	if (delim > 0) {
	    node = ki_findnode (alias)
	    if (node > 0)
		op = gstrcpy (n_alias[1,1,node], nodename, maxch) + 1
	    else
		op = gstrcpy (alias, nodename, maxch) + 1
	} else
	    op = 1

	# Append the node prefix delimiter character.
	if (op > 1 && op <= maxch) {
	    nodename[op] = FNNODE_CHAR
	    op = op + 1
	    nodename[op] = EOS
	}

	nodename[op] = EOS
	nchars = op - 1

	return (delim)
end
