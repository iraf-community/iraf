# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <config.h>
include "ki.h"

# KI_FREECHAN -- Free a channel descriptor slot.  Decrement the reference
# count on the associated node descriptor slot; when the ref count reaches
# zero, free the node descriptor if the error bit is set.  If the error
# bit is not set the node descriptor is not freed because the kernel server
# remains connected, ready for reuse.

procedure ki_freechan (chan)

int	chan			# kichan channel descriptor

int	server, and()
include	"kichan.com"
include	"kinode.com"

begin
	server = k_node[chan]

	# Server=0 if local node.
	if (server > 0) {
	    n_nrefs[server] = n_nrefs[server] - 1
	    if (and (n_status[server], F_IOERR) != 0)
		if (n_nrefs[server] == 0)
		    n_status[server] = F_IOERR + F_REUSE
	}
	    
	k_oschan[chan] = NULL
end
