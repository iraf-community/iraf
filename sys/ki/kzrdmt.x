# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	"ki.h"

# KZRDMT -- Asynchronous read from a magtape file.

procedure kzrdmt (chan, obuf, max_bytes)

int	chan			# magtape channel
char	obuf[ARB]		# buffer to receive data
int	max_bytes		# max bytes to read

int	server, status, nrecords, nfiles
int	ki_send(), ki_receive()
include	"kichan.com"
include	"kii.com"

begin
	server = k_node[chan]

	if (server == NULL) {
	    call zzrdmt (k_oschan[chan], obuf, max_bytes)
	    return
	}

	# Ignore zero reads and requests on a node closed by an error.
	if (max_bytes <= 0) {
	    k_status[chan] = 0
	    return
	}

	# Send the request to initiate the read.

	p_arg[1] = k_oschan[chan]
	p_arg[2] = max_bytes

	if (ki_send (server, KI_ZFIOMT, MT_RD) == ERR) {
	    status = ERR
	} else {
	    # Wait for the ZAWT packet.
	    if (ki_receive (server, KI_ZFIOMT, MT_WT) == ERR)
		status = ERR
	    else {
		status   = p_arg[1]
		nrecords = p_arg[2]
		nfiles   = p_arg[3]
	    }

	    # Read the data block (if any) directly into caller's buffer.
	    if (status > 0) {
		call ks_aread (server, obuf, status)
		call ks_await (server, status)
	    }
	}

	# For convenience we encode the nfiles and nrecords parameters in
	# k_bufp, for return by the WT call.  k_bufp is not otherwise used
	# for a magtape device.  The values of nfiles and nrecords are
	# assumed to be either 0 or 1.

	k_status[chan] = status
	k_bufp[chan] = nfiles * 10 + nrecords
end
