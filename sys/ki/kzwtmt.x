# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	"ki.h"

# KZWTMT -- Wait for i/o to complete on a magtape channel.

procedure kzwtmt (chan, nrecords, nfiles, status)

int	chan			# active magtape channel
int	nrecords		# nrecords skipped in last transfer
int	nfiles			# nfiles skipped in last transfer
int	status			# receives nbytes transferred or ERR

include	"kichan.com"

begin
	if (k_node[chan] == NULL)
	    call zzwtmt (k_oschan[chan], nrecords, nfiles, status)
	else {
	    # The file positioning information is returned in the not otherwise
	    # used k_bufp pointer for a magtape device.

	    status   = k_status[chan]
	    nrecords = mod (k_bufp[chan], 10)
	    nfiles   = k_bufp[chan] / 10
	}
end
