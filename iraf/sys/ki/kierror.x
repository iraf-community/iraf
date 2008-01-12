# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <config.h>
include "ki.h"

# KI_ERROR -- Called when an i/o error occurs on the channel to a remote
# kernel server process.  Note that this is not the same as an i/o error
# on a channel accessed on the remote node via the KI; when we are called
# we assume that further communications with the kernel server are impossible.
#
# Error recover strategy:
#
# We shut down the ZFIOKS channel to the server and set the error bit in the
# status word for the node.  Note that we cannot return the node descriptor
# until all of the remote OS channels (pids, etc.) multiplexed to the remote
# node are closed on the local, client node.  Any further i/o requests on the
# node will cause ERR to be returned without any attempt to do i/o.  A connect
# request on the node will cause another kernel server to be spawned and 
# another node descriptor to be allocated.  If and when all kichan descriptors
# using the bad node are freed, the node descriptor will be freed.

procedure ki_error (server)

int	server			# kernel server node

int	junk, node, i
int	or(), and()
include	"kinode.com"

begin
	# Close the kernel server channel and set the error bit in the
	# node descriptor.

	call zclsks (n_kschan[server], junk)
	n_status[server] = or (n_status[server], F_IOERR)

	# Allocate a new node descriptor for use by the next connection
	# on the node.  If a node descriptor on which an error has occurred
	# is later freed the F_REUSE bit will have been set in the status
	# word (and the other bits cleared) so that we may reuse the
	# descriptor.

	node = 0
	do i = 1, n_nnodes
	    if (and (n_status[i], F_REUSE) != 0) {
		node = i
		break
	    }
	if (node == 0) {
	    n_nnodes = n_nnodes + 1
	    if (n_nnodes > MAX_NODES)
		return
	    node = n_nnodes
	}

	# Initialize the new node descriptor.  It is not necessary to transfer
	# the n_local index since an i/o error cannot occur on the local node.

	n_kschan[node] = NULL
	n_status[node] = 0
	n_nalias[node] = n_nalias[server]

	call strcpy (n_server[1,server], n_server[1,node], SZ_SERVER)
	do i = 1, n_nalias[node]
	    call strcpy (n_alias[1,i,server], n_alias[1,i,node], SZ_ALIAS)
end
