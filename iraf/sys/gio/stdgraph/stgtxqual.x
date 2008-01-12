# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"stdgraph.h"

# STG_TXQUALITY -- Select the type of character generator to be used.  If the
# selected flag value is 0 this decision will be deferred to the set text
# attribute instruction at runtime (default).

procedure stg_txquality (quality)

int	quality			# text generation quality flag
include	"stdgraph.com"

begin
	g_hardchar = quality
end
