# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<mach.h>
include	<knet.h>
include	"ki.h"

# KI_MAPCHAN -- Return the OS channel number or pid and the node name of the
# resource associated with a given KI channel.  This procedure may be called
# whether or not networking is enabled.

int procedure ki_mapchan (chan, nodename, maxch)

int	chan			# KI channel (ret. by kopnbf, etc.)
char	nodename[maxch]		# receives node name
int	maxch

int	node
bool	netenab
data	netenab /KNET/
include	"kichan.com"
include	"kinode.com"

begin
	if (netenab) {
	    # Networking is enabled.  Every channel or pid returned to the VOS
	    # by the kernel is actually a KI channel index.

	    node = k_node[chan]
	    if (node == NULL || n_nnodes == 0)
		call strcpy (n_localnode, nodename, maxch)
	    else
		call strcpy (n_alias[1,1,node], nodename, maxch)

	    return (k_oschan[chan])

	} else {
	    # Networking is disabled.  Return the name of the local node
	    # and return the channel argument unchanged.

	    call strcpy (n_localnode, nodename, maxch)
	    return (chan)
	}
end
