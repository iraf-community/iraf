# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"gkt.h"

# GKT_FLUSH -- Flush output.

procedure gkt_flush (dummy)

int	dummy			# not used at present
include	"gkt.com"

begin
	# Since the NSPP devices are not interactive, calls to FLUSH
	# are ignored.
end
