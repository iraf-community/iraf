# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	<chars.h>
include "ki.h"

# KI_GNODE -- Extract the node name prefix from a resource name.  The node
# name delimiter character may be escaped to be included in the resource name.
# If the resource name does not include an explicit node name the name of the
# default node is returned.  The function value is LOCAL if the resource resides
# on the local node.  NOTE: We can be called before initialization of the
# node descriptor table.

int procedure ki_gnode (rname, outstr, delim)

char	rname[ARB]		# resource name
char	outstr[SZ_ALIAS]	# receives node name
int	delim			# receives offset of delim char

int	ch, nchars
int	ip, op, off, i
bool	streq()
int	ki_mapname()
include	"kinode.com"
define	lookup_ 91

begin
	nchars = 0
	delim = 0
	op = 1

	# Skip leading whitespace.
	for (off=1;  IS_WHITE (rname[off]);  off=off+1)
	    ;

	# If the first char is the node char, there can be no node prefix.
	if (rname[off] == FNNODE_CHAR) {
	    outstr[1] = EOS
	    return (LOCAL)
	}

	# Extract explicit node name if given.
	do ip = off, off + SZ_ALIAS {
	    ch = rname[ip]

	    if (!IS_LOWER(ch) || !IS_DIGIT(ch))
		if (ch == EOS)
		    break
		else if (ch == FNNODE_CHAR) {
		    if (rname[ip-1] != '\\') {
			# Have node name.
			delim = ip
			nchars = (op - 1)
			goto lookup_
		    } else
			op = max (1, op - 1)
		}

	    outstr[op] = ch
	    op = op + 1
	}

	# No explicit node name given; use default node name.
	do i = 1, SZ_ALIAS + 1 {
	    outstr[i] = n_defaultnode[i]
	    if (outstr[i] == EOS) {
		nchars = (i - 1)
		break
	    }
	}

	
lookup_
	outstr[nchars+1] = EOS

	# Determine if the named node is the local or a remote node.  This must
	# work during process startup, before the network tables have been read,
	# or when networking is enabled but the tables have not beed edited for
	# a new host (in which case there will be no entry in the tables for
	# the local node as identified by ZGHOST).

	if (nchars <= 0) {
	    # No output node name.
	    outstr[1] = EOS
	    return (LOCAL)

	} else if (n_local == NULL) {
	    # If local node is not identified in the host name table, this
	    # effectively disables networking.  All nodes names are assumed
	    # to be local.

	    return (LOCAL)

	} else {
	    # We have a node name and the local node is identified in the
	    # node table.  Scan the aliases of the local node to see if the
	    # referenced node is the same.

	    # Map possible logical node name.
	    if (ki_mapname (outstr, n_nodename, SZ_ALIAS) <= 0)
		call strcpy (outstr, n_nodename, SZ_ALIAS)

	    ch = n_nodename[1]
	    do i = 1, n_nalias[n_local]
		if (n_alias[1,i,n_local] == ch)
		    if (streq (n_alias[1,i,n_local], n_nodename))
			return (LOCAL)
	}

	return (REMOTE)
end
