include <mach.h>
include <ctype.h>
include <error.h>
include <gset.h>
include "pdm.h"

define	MSIZE		2.		# Mark size

# PDM_PPLOT -- Plot the Phase curve.

procedure pdm_pplot (pdmp, period, filename, flip)

pointer	pdmp			# pointer to PDM data structure
real	period			# period for which to plot the phase curve
char	filename[SZ_FNAME]	# name of the input data file
bool	flip			# flip the y-axis scale

int	npt, i, index
real	frequency, x1, x2, y1, y2
pointer	gp
pointer	title, system_id, sp

begin
	call smark (sp)
	call salloc (system_id, SZ_LINE, TY_CHAR)
	call salloc (title, PDM_SZ_TITLE, TY_CHAR)

	npt = PDM_NPT(pdmp)
	gp = PDM_GP(pdmp)
	call gclear (PDM_GP(pdmp))

	# Scale the wcs.
	call gascale (gp, PDM_XPH(pdmp,1), 2*npt, 1)
	call gascale (gp, PDM_YPH(pdmp,1), 2*npt, 2)

	# Flip the y-axis scale if flip = TRUE.
	if (flip) {
	    call ggwind (gp, x1, x2, y1, y2)
	    call gswind (gp, x1, x2, y2, y1)
	}

	# Multiline title, save in an array and sprintf to it.
	# Get the system identification.

	call sysid (Memc[system_id], SZ_LINE)

	# Calculate the frequency.
	if (period != 0.0)
	    frequency = 1./period

	call sprintf (Memc[title], PDM_SZ_TITLE,
	    "%s\nPhase curve at period %g, frequency %g\nFile = %s, numpts = %d")
	    call pargstr (Memc[system_id])
	    call pargr (period)
	    call pargr (frequency)
	    call pargstr (filename)
	    call pargi (npt)

	# Draw the axes.
	call glabax (gp, Memc[title], "phase", "magnitude")

	# Make the plot.
	if (npt <= PDM_PLUSPOINT(pdmp)) {
	    call gpmark (gp, PDM_XPH(pdmp,1), PDM_YPH(pdmp,1),
			2*npt, "plus", MSIZE, MSIZE)
	} else {
	    call gpmark (gp, PDM_XPH(pdmp,1), PDM_YPH(pdmp,1),
			2*npt, "point", 1, 1)
	}

	# Draw an x over any deleted points.
	do i = 1, npt {
	    index = PDM_SORT(pdmp,i)
	    if (PDM_INUSE(pdmp,index) == 0) {
	        call gscur (gp, PDM_XPH(pdmp,i)+1, PDM_YPH(pdmp,i))
	        call gmark (gp, PDM_XPH(pdmp,i)+1, PDM_YPH(pdmp,i), GM_CROSS,
		    MSIZE, MSIZE)
	        call gscur (gp, PDM_XPH(pdmp,i), PDM_YPH(pdmp,i))
	        call gmark (gp, PDM_XPH(pdmp,i), PDM_YPH(pdmp,i), GM_CROSS,
		    MSIZE, MSIZE)
	    }
	}

	call sfree (sp)
end
