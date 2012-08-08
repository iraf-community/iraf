# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	"ki.h"

# KZSTMT -- Device status for a magtape device.

procedure kzstmt (chan, what, lvalue)

int	chan			#I active magtape channel
int	what			#I device parameter
long	lvalue			#O parameter value

include	"kichan.com"

begin
	if (k_node[chan] == NULL)
	    call zzstmt (k_oschan[chan], what, lvalue)
	else 
	    call kb_zstt (KI_ZFIOMT, chan, what, lvalue)
end
