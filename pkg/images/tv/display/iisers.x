# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"zdisplay.h"
include	"iis.h"

# IISERS -- Erase IIS frame.

procedure iisers (chan)

int	chan[ARB]
short	erase

int	status, tid
int	iisflu(), andi()
include	"iis.com"

begin
	call achtiu (andi (ERASE, 0177777B), erase, 1)

	# IMTOOL special - IIS frame bufrer configuration code.
	tid = IWRITE+BYPASSIFM+BLOCKXFER
	tid = tid + max (0, iis_config - 1)

	call iishdr (tid, 1, FEEDBACK, ADVXONTC, ADVYONXOV, iisflu(chan),
	    ALLBITPL)
	call iisio (erase, SZB_CHAR, status)
end
