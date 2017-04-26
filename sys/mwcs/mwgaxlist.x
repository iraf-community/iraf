# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"mwcs.h"

# MW_GAXLIST -- Get the physical axis list.  The bitflags in AXBITS define the
# axes in the logical system; run these through the axis map (if enabled) to
# get the list of physical axes for which the transformation is to be prepared.

procedure mw_gaxlist (mw, axbits, axis, naxes)

pointer	mw			#I pointer to MWCS descriptor
int	axbits			#I bitflag marking the desired axes
int	axis[MAX_DIM]		#O output axis array
int	naxes			#O number of axes in axis array

int	bits, ax, i
int	bitupk()

begin
	bits = axbits
	if (bits == 0)
	    bits = 177B		# default to all axes

	naxes = 0
	do i = 1, MAX_DIM
	    if (bitupk (bits, i, 1) != 0) {
		if (MI_USEAXMAP(mw) == YES) {
		    if (i > MI_NLOGDIM(mw))
			break
		    # Map logical axis to physical axis.
		    ax = MI_PHYSAX(mw,i)
		} else {
		    if (i > MI_NDIM(mw))
			break
		    ax = i
		}

		# Add physical axis to axis list.
		naxes = naxes + 1
		axis[naxes] = ax
	    }
end
