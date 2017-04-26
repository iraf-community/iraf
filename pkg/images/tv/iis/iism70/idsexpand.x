# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <gki.h>
include "../lib/ids.h"
include	"iis.h"

# IDS_EXPAND -- expand FRAME/BITPL if first element is IDS_EOD
# if the frames are not counted in order, as on the Model 75,
# that should be dealt with here (use the "flag" boolean).

procedure ids_expand(data, max, flag)

short	data[ARB]			# data
short	max				# max number of frames/bitplanes
bool	flag				# true if frames ... e.g. for Model 75

int	i

begin
	if ( data[1] != IDS_EOD )
	    return
	do i = 1, max {
	    data[i] = i
	}
	if ( flag) {
	    data[1+max] = GRCHNUM
	    data[2+max] = IDS_EOD
	} else
	   data[1+max] = IDS_EOD
end
