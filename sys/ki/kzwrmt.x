# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	"ki.h"

# KZWRMT -- Asynchronous write to a magtape file.

procedure kzwrmt (chan, buf, nbytes, offset)

int	chan			#I magtape channel
char	buf[ARB]		#I buffer containing data
int	nbytes			#I nbytes to write
long	offset			#I file offset

int	server
int	ki_send()
include	"kichan.com"
include	"kii.com"

begin
	server = k_node[chan]

	if (server == NULL) {
	    call zzwrmt (k_oschan[chan], buf, nbytes, offset)
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
	p_arg[3] = offset

	if (ki_send (server, KI_ZFIOMT, MT_WR) == ERR)
	    k_status[chan] = ERR
	else {
	    call ks_awrite (server, buf, nbytes)
	    call ks_await  (server, k_status[chan])
	}
end
