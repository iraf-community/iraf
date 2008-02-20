# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>
include	<mach.h>
include	<config.h>
include	<fio.h>
include	"mtio.h"

# ZSTTMT -- Get magtape device or device driver parameters and settings.

procedure zsttmt (mtchan, what, lvalue)

int	mtchan			#I mtio descriptor
int	what			#I status parameter to be returned
long	lvalue			#O returned status value

int	chan
include	"mtio.com"

begin
	chan = MT_OSCHAN(mtchan)
	call zzstmt (chan, what, lvalue)
end
