# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	"ki.h"

# KZWRMT -- Asynchronous write to a magtape file.

procedure kzwrmt (chan, buf, nbytes)

int	chan			# magtape channel
char	buf[ARB]		# buffer containing data
int	nbytes			# nbytes to write

int	server
int	ki_send()
include	"kichan.com"
include	"kii.com"

begin
	server = k_node[chan]

	if (server == NULL) {
	    call zzwrmt (k_oschan[chan], buf, nbytes)
	    return
	}

	# Ignore zero writes and requests on a node closed by an error.
	if (nbytes <= 0) {
	    k_status[chan] = 0
	    return
	}

	# Send the request followed by the data block.  We do not read anything
	# back from the remote server until ZAWT is called.  Set k_status[chan]
	# to WRITE_IN_PROGRESS to tell ZAWT that an await call is needed.

	p_arg[1] = k_oschan[chan]
	p_arg[2] = nbytes

	if (ki_send (server, KI_ZFIOMT, MT_WR) == ERR)
	    k_status[chan] = ERR
	else {
	    call ks_awrite (server, buf, nbytes)
	    call ks_await  (server, k_status[chan])
	    k_bufp[chan] = 1	# 1 = ((nfiles=0) * 10 + (nrecords=1))
	}
end
