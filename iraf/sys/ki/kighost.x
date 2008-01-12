# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	"ki.h"

# KI_GETHOSTS -- Read the host name table (file) and initialize the node
# descriptor table (common).  The default hosts table is the file "dev$hosts";
# a different file may be specified with the environment variable
# "irafhostnametable".

int procedure ki_gethosts()

pointer	sp, lbuf, osfn, ip
int	chan, node, ch, op, junk, n, status, i, delim

bool	streq()
int	ctowrd(), envfind(), ki_gnode()
include	"kinode.com"

begin
	call smark (sp)
	call salloc (osfn, SZ_PATHNAME, TY_CHAR)
	call salloc (lbuf, SZ_LINE,  TY_CHAR)

	# Null selected node descriptor fields.
	call aclri (n_kschan, MAX_NODES)
	call aclri (n_nrefs,  MAX_NODES)
	call aclri (n_status, MAX_NODES)

	# Process the host name table, ignoring blank lines and comment lines,
	# until EOF is reached or the maximum number of nodes is exceeded.
	# The format of a line is the server name followed by the aliases.
	# Since this is a startup we require that the table file reside on
	# the local node (not absolutely necessary, but it simplifies things).
	# Note that since we are called by a z-routine at file open time,
	# we cannot use high level file i/o to read the HNT file without
	# reentrancy problems.

	if (envfind (HNT_ENVNAME, Memc[osfn], SZ_PATHNAME) <= 0) {
	    if (envfind ("iraf", Memc[osfn], SZ_PATHNAME) <= 0) {
		call sfree (sp)
		return (ERR)
	    }

	    # Strip any node prefix from the iraf$ pathname; it had better 
	    # reference the local node.

	    junk = ki_gnode (Memc[osfn], Memc[lbuf], delim)
	    if (delim > 0)
		call strcpy (Memc[osfn+delim], Memc[osfn], SZ_PATHNAME)

	    # Form filename "iraf$subdir/file".
	    call zfsubd (Memc[osfn], SZ_PATHNAME, HNT_SUBDIR, junk)
	    call strcat (HNT_FILENAME, Memc[osfn], SZ_PATHNAME)
	}

	# Open the table file, a text file.
	call strpak (Memc[osfn], Memc[osfn], SZ_PATHNAME)
	call zopntx (Memc[osfn], READ_ONLY, chan)
	if (chan == ERR) {
	    call sfree (sp)
	    return (ERR)
	}

	for (node=0;  node < MAX_NODES;  ) {
	    call zgettx (chan, Memc[lbuf], SZ_LINE, status)
	    if (status > 0)
		Memc[lbuf+status] = EOS
	    else
		break

	    # Get the next nonempty, noncomment line.
	    for (ip=lbuf;  IS_WHITE(Memc[ip]);  ip=ip+1)
		;
	    ch = Memc[ip]
	    if (ch == '\n' || ch == '#' || ch == EOS)
		next

	    node = node + 1

	    # Extract the whitespace delimited alias names.  The list of
	    # aliases is terminated by a colon.

	    n_nalias[node] = 0
	    n = 1

	    while (ctowrd (Memc, ip, n_alias[1,n,node], SZ_ALIAS) > 0) {
		while (IS_WHITE (Memc[ip]))
		    ip = ip + 1

		if (Memc[ip] == ':') {
		    ip = ip + 1
		    n_nalias[node] = n
		    break
		} else
		    n = min (MAX_ALIAS, n + 1)
	    }

	    while (IS_WHITE (Memc[ip]))
		ip = ip + 1

	    # Extract the kernel server name for the node.  The server name
	    # string may contain whitespace and is delimited by end of line.

	    for (op=1;  op <= SZ_SERVER;  op=op+1) {
		ch = Memc[ip]
		if (ch == '\n' || ch == EOS)
		    break

		n_server[op,node] = ch
		ip = ip + 1
	    }

	    # Strip any trailing whitespace.
	    while (op > 1 && IS_WHITE(n_server[op-1,node]))
		op = op - 1

	    # Make sure the server string is null terminated.
	    n_server[op,node] = EOS
	}

	n_nnodes = node
	call zclstx (chan, status)

	# Flag the local node.  One of the aliases must match the name returned
	# by ZGHOST else the local node will be accessed like a remote node
	# (you may wish to take advantage of that for debugging).  The default
	# node name is initialized to the local node.

	n_local = 0
	n_default = 0

	call strcpy (n_localnode, n_defaultnode, SZ_ALIAS)

	for (node=1;  node <= n_nnodes;  node=node+1)
	    for (i=1;  i <= n_nalias[node];  i=i+1)
		if (streq (n_alias[1,i,node], n_localnode)) {
		    n_local = node
		    n_default = node
		    break
		}

	if (n_local > 0) {
	    # Add the alias "0" to the alias list for this node.  This is a
	    # required alias and will overwrite the last alias for the node
	    # if the alias list is full.

	    n = n_nalias[n_local]
	    n = min (MAX_ALIAS, n + 1)
	    n_nalias[n_local] = n
	    call strcpy ("0", n_alias[1,n,n_local], SZ_ALIAS)
	}

	call sfree (sp)
	return (n_nnodes)
end
