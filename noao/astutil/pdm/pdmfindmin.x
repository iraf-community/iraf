include <mach.h>
include <ctype.h>
include <error.h>
include "pdm.h"

# PDM_FINDMIN -- Find the minimum value of the abscissa.

int procedure pdm_findmin (pdmp, ptype, startint, endint, is, ie)

pointer	pdmp			# pointer to PDM data structure
int	ptype			# plot type
double	startint, endint	# start and end ordinates
int	is, ie			# start and end indexs

double	miny, dy, dx
int	i, isave, npt
pointer	xpt, ypt

begin
	# Dereference npt.
	npt = PDM_NPT(pdmp)

	# Dereference the appropriate abcissa and ordinate.
	switch (ptype) {
	case DATAPLOT:
	    xpt = PDM_XP(pdmp)
	    ypt = PDM_DYP(pdmp)
	case THETAPPLOT:
	    xpt = PDM_XTHP(pdmp)
	    ypt = PDM_YTHP(pdmp)
	case THETAFPLOT:
	    ypt = PDM_YTHP(pdmp)
	    xpt = PDM_XTHP(pdmp)
	case PHASEPLOT:
	    ypt = PDM_YPHP(pdmp)
	    xpt = PDM_XPHP(pdmp)
	}

	# Search the abscissas between startint and endint
	# for the minimum value.

	isave = 1
	miny = MAX_DOUBLE
	do i = is, ie {
	    dx = Memd[xpt+i-1]
	    dy = Memd[ypt+i-1]
	    if (dx > startint && dx < endint) {
		if (dy < miny) {
	    	    miny = dy
	    	    isave = i
		}
	    }
	}

	# Return the corresponding index value.
	return (isave)
end
