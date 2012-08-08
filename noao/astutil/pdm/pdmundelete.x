include <mach.h>
include <ctype.h>
include <error.h>
include <gset.h>
include "pdm.h"

define	MSIZE		2.0		# Mark size

# PDM_UNDELETE -- Undelete the nearest deleted point from the plot
# and set it's inuse array entry to one (in-use).

int procedure pdm_undelete (pdmp, cx, cy, ptype)

pointer	pdmp			# pointer to PDM data structure
double	cx, cy			# device coordinates of point to undelete
int	ptype			# plot type flag

pointer	gp
real	x, y
int	npt, i, j, index
real	x0, y0, r2, r2min

begin
	gp = PDM_GP(pdmp)
	npt = PDM_NPT(pdmp)

	if (ptype == DATAPLOT) {
	    # Transform world cursor coordinates to NDC.
	    call gctran (gp, real(cx), real(cy), x, y, 1, 0)

	    # Search for nearest point not in-use.
	    j = 0
	    r2min = MAX_REAL
	    do i = 1, npt {
	        if (PDM_INUSE(pdmp,i) == 1)
		    next

	        call gctran (gp, real(PDM_X(pdmp,i)), real(PDM_DY(pdmp,i)),
			     x0, y0, 1, 0)
		
	        r2 = (x0 - x) ** 2 + (y0 - y) ** 2
	        if (r2 < r2min) {
		    r2min = r2
		    j = i
	        }
	    }

	    # Unmark the deleted point and reset the weight.
	    if (j != 0) {
	        call gscur (gp, real(PDM_X(pdmp,j)), real(PDM_DY(pdmp,j)))
	        call gseti (gp, G_PMLTYPE, GL_CLEAR)
	        call gmark (gp, real(PDM_X(pdmp,j)), real(PDM_DY(pdmp,j)),
			    GM_CROSS, MSIZE, MSIZE)
	        call gseti (gp, G_PMLTYPE, GL_SOLID)
		if (PDM_NPT(pdmp) <= PDM_PLUSPOINT(pdmp))
	            call gpmark (gp, real(PDM_X(pdmp,j)), real(PDM_DY(pdmp,j)),
		        1, GM_PLUS, MSIZE, MSIZE)
		else
	            call gpmark (gp, real(PDM_X(pdmp,j)), real(PDM_DY(pdmp,j)),
		        1, GM_POINT, 1.0, 1.0)
	        PDM_INUSE(pdmp,j) = 1
		call gflush (gp)
	    }

	    return (j)
	} else if (ptype == PHASEPLOT) {
	    # Transform world cursor coordinates to NDC.
	    call gctran (gp, real(cx), real(cy), x, y, 1, 0)

	    # Search for nearest point not in-use.
	    j = 0
	    r2min = MAX_REAL
	    do i = 1, npt {
		index = PDM_SORT(pdmp,i)
	        if (PDM_INUSE(pdmp,index) == 1)
		    next

	        call gctran (gp, real(PDM_XPH(pdmp,i)), real(PDM_YPH(pdmp,i)),
			     x0, y0, 1, 0)
		
	        r2 = (x0 - x) ** 2 + (y0 - y) ** 2
	        if (r2 < r2min) {
		    r2min = r2
		    j = i
	        }
	    }

	    # Unmark the deleted point and reset the weight.
	    if (j != 0) {
	        call gscur (gp, real(PDM_XPH(pdmp,j))+1.0,
		    real(PDM_YPH(pdmp,j)))
	        call gseti (gp, G_PMLTYPE, GL_CLEAR)
	        call gmark (gp, real(PDM_XPH(pdmp,j))+1.0,
		    real(PDM_YPH(pdmp,j)), GM_CROSS, MSIZE, MSIZE)
	        call gseti (gp, G_PMLTYPE, GL_SOLID)
		if (PDM_NPT(pdmp) <= PDM_PLUSPOINT(pdmp))
	            call gpmark (gp, real(PDM_XPH(pdmp,j))+1.0,
		    real(PDM_YPH(pdmp,j)), 1, GM_PLUS, MSIZE, MSIZE)
		else
	            call gpmark (gp, real(PDM_XPH(pdmp,j))+1.0,
		    real(PDM_YPH(pdmp,j)), 1, GM_POINT, 1.0, 1.0)

	        call gscur (gp, real(PDM_XPH(pdmp,j)), real(PDM_YPH(pdmp,j)))
	        call gseti (gp, G_PMLTYPE, GL_CLEAR)
	        call gmark (gp, real(PDM_XPH(pdmp,j)), real(PDM_YPH(pdmp,j)),
			    GM_CROSS, MSIZE, MSIZE)
	        call gseti (gp, G_PMLTYPE, GL_SOLID)
		if (PDM_NPT(pdmp) <= PDM_PLUSPOINT(pdmp))
	            call gpmark (gp, real(PDM_XPH(pdmp,j)),
		    real(PDM_YPH(pdmp,j)), 1, GM_PLUS, MSIZE, MSIZE)
		else
	            call gpmark (gp, real(PDM_XPH(pdmp,j)),
		    real(PDM_YPH(pdmp,j)), 1, GM_POINT, 1.0, 1.0)
		
		# Calculate which point this corresponds to.
		index = PDM_SORT(pdmp,j)
	        PDM_INUSE(pdmp,index) = 1
		call gflush (gp)
	    }

	    return (index)
	} else
	    return (0)
end
