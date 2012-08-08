# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	<fio.h>
include	"ki.h"

# KZRDMT -- Asynchronous read from a magtape file.

procedure kzrdmt (chan, obuf, max_bytes, offset)

int	chan			#I magtape channel
char	obuf[ARB]		#O buffer to receive data
int	max_bytes		#I max bytes to read
long	offset			#I file offset

pointer	bd
int	server, status
int	ki_send(), ki_receive()
include	"kichan.com"
include	"kii.com"

begin
	server = k_node[chan]

	if (server == NULL) {
	    call zzrdmt (k_oschan[chan], obuf, max_bytes, offset)
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
	p_arg[3] = offset

	if (ki_send (server, KI_ZFIOMT, MT_RD) == ERR) {
	    status = ERR
	} else {
	    bd = k_bufp[chan]

	    # Wait for the ZAWT packet.
	    if (ki_receive (server, KI_ZFIOMT, MT_WT) == ERR)
		status = ERR
	    else {
		status = p_arg[1]
		call amovi (p_arg[2], Memi[bd], LEN_MTDEVPOS)
	    }

	    # Read the data block (if any) directly into caller's buffer.
	    if (status > 0) {
		call ks_aread (server, obuf, status)
		call ks_await (server, status)
	    }
	}

	k_status[chan] = status
end
