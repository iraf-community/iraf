# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	"ki.h"

# KI_INIT -- Called by the IRAF Main during process startup to initialize
# the kernel interface to the process i/o channels.  INCHAN, OUTCHAN, and
# ERRCHAN are the host system i/o channel codes for the process standard
# input, output, and error channels.  DEVICE is a ZLOCPR entry point address
# (kernel level) identifing the device driver to be used.  To use the KI we
# must allocate channel descriptors for the host system i/o channels, and
# the device entry point address must be mapped to the corresponding KI
# procedure.

procedure ki_init (inchan, outchan, errchan, device, devtype)

int	inchan			#RW process input channel
int	outchan			#RW process output channel
int	errchan			#RW process error output channel
int	device			#RW device driver epa
int	devtype			#R  device type (not modified)

int	locpr()
extern	zgettx(), zgetty(), zardbf(), zardpr()
extern	kgettx(), kgetty(), kardbf(), kardpr()
include	"kichan.com"
include	"kinode.com"

begin
	# Initialize the ki channel descriptors.
	call amovki (NULL, k_oschan, MAX_CHANNELS)
	call amovki (ERR,  k_status, MAX_CHANNELS)

	# Assign KI channels for the 3 OS channels.

	k_node[1]   = NULL
	k_oschan[1] = inchan
	inchan      = 1

	k_node[2]   = NULL
	k_oschan[2] = outchan
	outchan     = 2

	k_node[3]   = NULL
	k_oschan[3] = errchan
	errchan     = 3

	# Map device codes.

	if (     device == locpr (zgettx))
	    device = locpr (kgettx)
	else if (device == locpr (zgetty))
	    device = locpr (kgetty)
	else if (device == locpr (zardbf))
	    device = locpr (kardbf)
	else if (device == locpr (zardpr))
	    device = locpr (kardpr)
	
	# Initialize node descriptor.

	call zghost (n_localnode, SZ_ALIAS)
	call strupk (n_localnode, n_localnode, SZ_ALIAS)

	n_defaultnode[1] = EOS
	n_default = NULL
	n_local	  = NULL
end
