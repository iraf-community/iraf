include <mach.h>
include <ctype.h>
include <error.h>
include <gset.h>
include "pdm.h"

# PDM_TPLOT -- Plot the data on the screen.

procedure pdm_tplot (pdmp, porf, filename)

pointer	pdmp			# pointer to PDM data structure
int	porf			# period or frequency flag
char	filename[SZ_FNAME]	# name of the input data file

int	nthpt
pointer	gp
pointer	title
char	system_id[SZ_LINE]
int	indx, pdm_findmin()
errchk	malloc, pdm_findmin()

begin
	# Dereference some structure stuff.
	nthpt = PDM_NTHPT(pdmp)
	gp = PDM_GP(pdmp)

	call malloc (title, PDM_SZ_TITLE, TY_CHAR)
	call gclear (gp)

	if (porf == THETAPPLOT) {
	    # Scale the wcs.
	    call gascale (gp, PDM_XTH(pdmp,1), nthpt, 1)
	    call gascale (gp, PDM_YTH(pdmp,1), nthpt, 2)

	    # Find the minimum value and save it in the remembered minimum.
	    indx = pdm_findmin (pdmp, THETAPPLOT, PDM_PMIN(pdmp),
		PDM_PMAX(pdmp), 1, nthpt)
	    PDM_MINR(pdmp) = PDM_XTH(pdmp,indx)

	    # Multiline title, save in an array and sprintf to it.
	    # Get the system identification.

	    call sysid (system_id, SZ_LINE)
	    call sprintf (Memc[title], PDM_SZ_TITLE,
		"%s\nFile = %s, minimum = %g\n%s\nnumpts = %d")
		call pargstr (system_id)
	        call pargstr (filename)
		call pargr (PDM_MINR(pdmp))
	        call pargstr ("Theta vs Period")
		call pargi (nthpt)

	    # Draw the axes.
	    call glabax (gp, Memc[title], "period", "theta")

	    # Make the plot.
	    call gpline (gp, PDM_XTH(pdmp,1), PDM_YTH(pdmp,1), nthpt)

	    # Put the cursor at the minimum.
	    call gscur (gp, PDM_XTH(pdmp,indx), PDM_YTH(pdmp,indx))
	} else {
	    # Scale the wcs.
	    call gascale (gp, PDM_XTH(pdmp,1), nthpt, 1)
	    call gascale (gp, PDM_YTH(pdmp,1), nthpt, 2)

	    # Find the minimum value and save it in the remembered minimum.
	    indx = pdm_findmin (pdmp, THETAFPLOT, PDM_FMIN(pdmp),
		PDM_FMAX(pdmp), 1, nthpt)
	    if (PDM_XTH(pdmp,indx) > EPSILONR)
	        PDM_MINR(pdmp) = 1./PDM_XTH(pdmp,indx)

	    # Multiline title, save in an array and sprintf to it.
	    # Get the system identification.

	    call sysid (system_id, SZ_LINE)
	    call sprintf (Memc[title], PDM_SZ_TITLE,
	        "%s\nFile = %s, minimum = %g\n%s\nnumpts = %d")
		call pargstr (system_id)
	        call pargstr (filename)
		if (PDM_MINR(pdmp) > EPSILONR)
		    call pargr (1.0/PDM_MINR(pdmp))
		else
		    call pargr (0.0)
	        call pargstr ("Theta vs Frequency")
		call pargi (nthpt)

	    # Draw the axes.
	    call glabax (gp, Memc[title], "frequency", "theta")

	    # Make the plot.
	    call gpline (gp, PDM_XTH(pdmp,1), PDM_YTH(pdmp,1), nthpt)

	    # Put the cursor at the minimum.
	    call gscur (gp, PDM_XTH(pdmp,indx), PDM_YTH(pdmp,indx))
        }

	call mfree (title, TY_CHAR)
end
