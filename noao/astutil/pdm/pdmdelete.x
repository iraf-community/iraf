include <mach.h>
include <ctype.h>
include <error.h>
include <gset.h>
include "pdm.h"

define	MSIZE		2.		# Mark size

# PDM_DELETE -- Delete the point from the plot and set it's inuse array
# entry to zero.

int procedure pdm_delete (pdmp, cx, cy, ptype)

pointer	pdmp			# pointer to PDM data structure
real	cx, cy			# device cursor coordinates
int	ptype			# plot type

pointer	gp
int	npts, i, j, index
real	x0, y0, r2, r2min

begin
	gp = PDM_GP(pdmp)
	npts = PDM_NPT(pdmp)

	if (ptype == DATAPLOT) {
	    # Transform world cursor coordinates to NDC.
	    call gctran (gp, cx, cy, cx, cy, 1, 0)

	    # Search for nearest point in-use.
	    j = 0
	    r2min = MAX_REAL
	    do i = 1, npts {
	        if (PDM_INUSE(pdmp,i) == 0)
		    next

	        call gctran (gp, PDM_X(pdmp,i), PDM_DY(pdmp,i), x0, y0, 1, 0)
		
	        r2 = (x0 - cx) ** 2 + (y0 - cy) ** 2
	        if (r2 < r2min) {
		    r2min = r2
		    j = i
	        }
	    }

	    # Mark the deleted point with a cross and set the weight to zero.
	    if (j != 0) {
	        call gscur (gp, PDM_X(pdmp,j), PDM_DY(pdmp,j))
	        call gmark (gp, PDM_X(pdmp,j), PDM_DY(pdmp,j),
		    GM_CROSS, MSIZE, MSIZE)
	        PDM_INUSE(pdmp,j) = 0
	    }

	    return (j)

	} else if (ptype == PHASEPLOT) {
	    # Transform world cursor coordinates to NDC.
	    call gctran (gp, cx, cy, cx, cy, 1, 0)

	    # Search for nearest point in-use.
	    j = 0
	    r2min = MAX_REAL
	    do i = 1, npts {
		index = PDM_SORT(pdmp,i)
	        if (PDM_INUSE(pdmp,index) == 0)
		    next

	        call gctran (gp, PDM_XPH(pdmp,i), PDM_YPH(pdmp,i), x0, y0, 1, 0)
		
	        r2 = (x0 - cx) ** 2 + (y0 - cy) ** 2
	        if (r2 < r2min) {
		    r2min = r2
		    j = i
	        }
	    }

	    # Mark the deleted point with a cross and set the weight to zero.
	    # We mark two points since all of the points are displayed twice
	    # on the phase plot.
	    
	    if (j != 0) {
	        call gscur (gp, PDM_XPH(pdmp,j)+1.0, PDM_YPH(pdmp,j))
	        call gmark (gp, PDM_XPH(pdmp,j)+1.0, PDM_YPH(pdmp,j),
		    GM_CROSS, MSIZE, MSIZE)
	        call gscur (gp, PDM_XPH(pdmp,j), PDM_YPH(pdmp,j))
	        call gmark (gp, PDM_XPH(pdmp,j), PDM_YPH(pdmp,j),
		    GM_CROSS, MSIZE, MSIZE)
		
		# Calculate which point this corresponds to.
		index = PDM_SORT(pdmp,j)
	        PDM_INUSE(pdmp,index) = 0
	    }

	    return (index)
	} else
	    return (0)
end
