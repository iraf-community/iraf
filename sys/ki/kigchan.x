# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <config.h>
include "ki.h"

# KI_GETCHAN -- Find the first empty slot in the channel descriptor table
# and return its index, initializing the server and oschan fields with the
# values given.

int procedure ki_getchan (server, oschan)

int	server			# kernel server node
int	oschan			# os channel (iraf kernel channel)

int	i
include	"kichan.com"
include	"kinode.com"

begin
	# Server=0 if local node.
	if (server > 0)
	    n_nrefs[server] = n_nrefs[server] + 1

	do i = FIRST_CHAN, MAX_CHANNELS
	    if (k_oschan[i] == NULL) {
		# Initialize channel descriptor.

		k_node[i]   = server
		k_oschan[i] = oschan
		k_status[i] = 0
		k_bufp[i]   = NULL

		return (i)
	    }

	# The following cannot happen unless something is very wrong.
	call sys_panic (0, "ki_getchan: out of channel slots")
end
