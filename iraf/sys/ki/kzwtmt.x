# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<fio.h>
include	"ki.h"

# KZWTMT -- Wait for i/o to complete on a magtape channel.

procedure kzwtmt (chan, devpos, status)

int	chan			#I active magtape channel
long	devpos[ARB]		#O device position structure
long	status			#O receives nbytes transferred or ERR

pointer	bd
include	"kichan.com"

begin
	if (k_node[chan] == NULL)
	    call zzwtmt (k_oschan[chan], devpos, status)
	else {
	    bd = k_bufp[chan]
	    status = k_status[chan]
	    call amovl (Memi[bd], devpos, LEN_MTDEVPOS)
	}
end
