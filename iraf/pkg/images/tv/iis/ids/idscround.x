# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "../lib/ids.h"
include <gki.h>

# IDS_CROUND  --  coordinate rounding.  Since putcell and other similar
# calls are defined to include both the lower-left corner and the upper-right
# corners of the desired rectangle, it is necessary to "round" the
# coordinates so that adjacent rectangles do not have overlapping edges.
# This could have been done by agreeing that the top and right edges of the
# rectangle are not part of it, but this was not done in the GKI definition.
# Hence, here, we adopt the notion that if (for example) the upper y coordinate
# is in the top half of a pixel, that pixel is included and if the lower y
# coordinate is in the bottom half of a pixel, likewise, that pixel is included.
# Otherwise, the pixels are excluded from putcell.  The x coordinates are
# treated similarly.
# The code depends on the fact that lower is <= upper, that upper will be
# at most GKI_MAXNDC, and that the device resolution will never be as much
# as (GKI_MAXNDC+1)/2.  The last requirement stems from the fact that if
# the resolution were that high, each pixel would be 2 GKI units and 
# the "rounding" based on whether or not we are in the upper or lower half
# of a pixel would probably fail due to rounding/truncation errors.

procedure ids_cround(lower, upper, res)

int	lower, upper
real	res				# device resolution

real	low, up
real	factor

begin
	factor = res/(GKI_MAXNDC+1)
	low = real(lower) * factor
	up  = real(upper) * factor

	# if boundaries result in same row, return
	if ( int(low) == int(up) )
	    return

	# if low is in upper half of device pixel, round up
	if ( (low - int(low)) >= 0.5 ) {
	    low = int(low) + 1
	    # don't go to or beyond upper bound
	    if ( low < up ) {
		# low already incremented;
	        # ... 0.2 just for "rounding protection"
		lower = (low + 0.2)/factor
	        # if now reference same cell, return
		if ( int(low) == int(up) )
		    return
	    }
	}

	# if "up" in bottom half of pixel, drop down one.  Note that
	# due to two "==" tests above, upper will not drop below lower.
	# 0.2 means drop partway down into pixel below; calling code will
	# truncate.
	if ( (up - int(up)) < 0.5 )
	    upper = (real(int(up)) - 0.2)/factor
end
