# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "../lib/ids.h"

# IISPACK -- Pack color or frame data into a single word.

short procedure iispack (data)

short	data[ARB]
int	value, bit, i
int	or()

begin
	value = 0
	for (i=1;  data[i] != IDS_EOD;  i=i+1) {
	    bit = data[i] - 1
	    value = or (value, 2 ** bit)
	}

	return (value)
end
