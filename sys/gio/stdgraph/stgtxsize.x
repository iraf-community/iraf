# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>
include	"stdgraph.h"

# STG_TXSIZE -- Given the relative character size as a packed real, select
# the discreet closest device character size.

int procedure stg_txsize (pksize)

int	pksize			# packed real relative character size
int	i, best_size
real	txsize, diff, least_diff
include	"stdgraph.com"

begin
	txsize = GKI_UNPACKREAL (pksize)

	best_size = 1
	least_diff = abs (txsize - SG_CHARSIZE(g_sg,1))

	do i = 2, SG_NCHARSIZES(g_sg) {
	    diff = abs (txsize - SG_CHARSIZE(g_sg,i))
	    if (diff < least_diff) {
		best_size = i
		least_diff = diff
	    }
	}

	return (best_size)
end
