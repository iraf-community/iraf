# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"fmio.h"

# FMIO_TICK -- Check the clock and do things that need to get done
# periodically, like sync the datafile.

procedure fmio_tick (fm)

pointer	fm			#I FMIO descriptor

long	clktime()

begin
	if (clktime(FM_LSYNCTIME(fm)) > SYNC_INTERVAL)
	    call fm_sync (fm)
end
