include "iralign.h"

# IR_INDICES -- Given the number in the list for a missing subraster and
# information about how the subrasters were written return the i and j
# indices of the specified subrasters.

procedure ir_indices (num, i, j, nxsub, nysub, corner, raster, order)

int	num		# number of the subraster
int	i,j		# indices of the subraster
int	nxsub,nysub	# number of subrasters in x and y
int	corner		# starting corner
int	raster		# raster order
int	order		# column or row order

begin
	switch (corner) {
	case IR_LL:
	    if (order == IR_ROW) {
		if (mod (num, nxsub) == 0) {
		    j = num / nxsub
		    if (raster == YES && mod (j,2) == 0)
			i = 1
		    else
		        i = nxsub
		} else {
		    j = num / nxsub + 1
		    if (raster == YES && mod (j,2) == 0)
			i = nxsub - mod (num, nxsub) + 1
		    else
		        i = mod (num, nxsub)
		}
	    } else if (order == IR_COLUMN) {
		if (mod (num, nysub) == 0) {
		    i = num / nysub
		    if (raster == YES && mod (i,2) == 0)
			j = 1
		    else
		        j = nysub
		} else {
		    i = num / nysub + 1
		    if (raster == YES && mod (i,2) == 0)
			j = nysub - mod (num, nysub) + 1
		    else
		        j = mod (num, nysub)
		}
	    }
	case IR_LR:
	    if (order == IR_ROW) {
		if (mod (num, nxsub) == 0) {
		    j = num / nxsub
		    if (raster == YES && mod (j,2) == 0)
			i = nxsub
		    else
			i = 1
		} else {
		    j = num / nxsub + 1
		    if (raster == YES && mod (j,2) == 0)
			i = mod (num, nxsub)
		    else
			i = nxsub - mod (num, nxsub) + 1
		}
	    } else if (order == IR_COLUMN) {
		if (mod (num, nysub) == 0) {
		    i = nxsub - num / nysub + 1
		    if (raster == YES && mod (i,2) != 0)
			j = 1
		    else
		        j = nysub
		} else {
		    i = nxsub - num / nysub
		    if (raster == YES && mod (i,2) != 0)
			j = nysub - mod (num, nysub) + 1
		    else
		        j = mod (num, nysub)
		}
	    }
	case IR_UL:
	    if (order == IR_ROW) {
		if (mod (num, nxsub) == 0) {
		    j = nysub - num / nxsub + 1
		    if (raster == YES && mod (j,2) != 0)
			i = 1
		    else
		        i = nxsub
		} else {
		    j = nysub - num / nxsub
		    if (raster == YES && mod (j,2) != 0)
			i = nxsub - mod (num, nxsub) + 1
		    else
		        i = mod (num, nxsub)
		}
	    } else if (order == IR_COLUMN) {
		if (mod (num, nysub) == 0) {
		    i = num / nysub
		    if (raster == YES && mod (i,2) == 0)
			j = nysub
		    else
			j = 1
		} else {
		    i = num / nysub + 1
		    if (raster == YES && mod (i,2) == 0)
			j = mod (num, nysub)
		    else
			j = nysub - mod (num, nysub) + 1
		}
	    }
	case IR_UR:
	    if (order == IR_ROW) {
		if (mod (num, nxsub) == 0) {
		    j = nysub - num / nxsub + 1
		    if (raster == YES && mod (j,2) != 0)
			i = nxsub
		    else
			i = 1
		} else {
		    j = nysub - num / nxsub
		    if (raster == YES && mod (j,2) != 0)
			i = mod (num, nxsub)
		    else
			i = nxsub - mod (num, nxsub) + 1
		}
	    } else if (order == IR_COLUMN) {
		if (mod (num, nysub) == 0) {
		    i = nxsub - num / nysub + 1
		    if (raster == YES && mod (i,2) != 0)
			j = nysub
		    else
			j = 1
		} else {
		    i = nxsub - num / nysub
		    if (raster == YES && mod (i,2) != 0)
			j = mod (num, nysub)
		    else
			j = nysub - mod (num, nysub) + 1
		}
	    }
	}
end
