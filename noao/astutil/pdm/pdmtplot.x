include <mach.h>
include <ctype.h>
include <error.h>
include <gset.h>
include "pdm.h"

define	EDGESCL	5.0			# Percent extra space around plot data

# PDM_TPLOT -- Plot the data on the screen.

procedure pdm_tplot (pdmp, porf, filename)

pointer	pdmp			# pointer to PDM data structure
int	porf			# period or frequency flag
char	filename[SZ_FNAME]	# name of the input data file

int	nthpt
real	x1, x2, y1, y2, scldif, sclspc
pointer	gp, xtemp, ytemp
pointer	title
char	system_id[SZ_LINE]
int	indx, pdm_findmin()
errchk	malloc, pdm_findmin()

begin
	# Dereference some structure stuff.
	nthpt = PDM_NTHPT(pdmp)
	gp = PDM_GP(pdmp)

	call malloc (title, PDM_SZ_TITLE, TY_CHAR)
	call malloc (xtemp, nthpt, TY_REAL)
	call malloc (ytemp, nthpt, TY_REAL)
	call gclear (gp)

	do indx = 1, nthpt {
	    Memr[xtemp+indx-1] = real(PDM_XTH(pdmp,indx))
	    Memr[ytemp+indx-1] = real(PDM_YTH(pdmp,indx))
	}

	if (porf == THETAPPLOT) {
	    # Scale the wcs.
	    call gascale (gp, Memr[xtemp], nthpt, 1)
	    call gascale (gp, Memr[ytemp], nthpt, 2)

	    # Get the X and Y boundary values.
	    call ggwind (gp, x1, x2, y1, y2)

	    # Add boundry space.
	    scldif = x2 - x1
	    sclspc = scldif * (EDGESCL / 100.)
	    x1 = x1 - sclspc
	    x2 = x2 + sclspc
	    scldif = y2 - y1
	    sclspc = scldif * (EDGESCL / 100.)
	    y1 = y1 - sclspc
	    y2 = y2 + sclspc

	    call gswind (gp, x1, x2, y1, y2)

	    # Find the minimum value and save it in the remembered minimum.
	    indx = pdm_findmin (pdmp, THETAPPLOT, PDM_PMIN(pdmp),
		PDM_PMAX(pdmp), 1, nthpt)
	    PDM_MINR(pdmp) = PDM_XTH(pdmp,indx)

	    # Multiline title, save in an array and sprintf to it.
	    # Get the system identification.

	    call sysid (system_id, SZ_LINE)
	    call sprintf (Memc[title], PDM_SZ_TITLE,
		"%s\nFile = %s, minimum = %12.12g\n%s\nnumpts = %d")
		call pargstr (system_id)
	        call pargstr (filename)
		call pargd (PDM_MINR(pdmp))
	        call pargstr ("Theta vs Period")
		call pargi (nthpt)

	    # Draw the axes.
	    call glabax (gp, Memc[title], "period", "theta")

	    # Make the plot.
	    call gpline (gp, Memr[xtemp], Memr[ytemp], nthpt)

	    # Put the cursor at the minimum.
	    call gscur (gp, Memr[xtemp+indx-1], Memr[ytemp+indx-1])
	} else {
	    # Scale the wcs.
	    call gascale (gp, Memr[xtemp], nthpt, 1)
	    call gascale (gp, Memr[ytemp], nthpt, 2)

	    # Get the X and Y boundary values.
	    call ggwind (gp, x1, x2, y1, y2)

	    # Add boundry space.
	    scldif = x2 - x1
	    sclspc = scldif * (EDGESCL / 100.)
	    x1 = x1 - sclspc
	    x2 = x2 + sclspc
	    scldif = y2 - y1
	    sclspc = scldif * (EDGESCL / 100.)
	    y1 = y1 - sclspc
	    y2 = y2 + sclspc

	    call gswind (gp, x1, x2, y1, y2)

	    # Find the minimum value and save it in the remembered minimum.
	    indx = pdm_findmin (pdmp, THETAFPLOT, PDM_FMIN(pdmp),
		PDM_FMAX(pdmp), 1, nthpt)
	    if (PDM_XTH(pdmp,indx) > EPSILOND)
	        PDM_MINR(pdmp) = 1./PDM_XTH(pdmp,indx)

	    # Multiline title, save in an array and sprintf to it.
	    # Get the system identification.

	    call sysid (system_id, SZ_LINE)
	    call sprintf (Memc[title], PDM_SZ_TITLE,
	        "%s\nFile = %s, minimum = %12.12g\n%s\nnumpts = %d")
		call pargstr (system_id)
	        call pargstr (filename)
		if (PDM_MINR(pdmp) > EPSILOND)
		    call pargd (1.0d+0/PDM_MINR(pdmp))
		else
		    call pargd (0.0d+0)
	        call pargstr ("Theta vs Frequency")
		call pargi (nthpt)

	    # Draw the axes.
	    call glabax (gp, Memc[title], "frequency", "theta")

	    # Make the plot.
	    call gpline (gp, Memr[xtemp], Memr[ytemp], nthpt)

	    # Put the cursor at the minimum.
	    call gscur (gp, Memr[xtemp+indx-1], Memr[ytemp+indx-1])
        }

	call mfree (title, TY_CHAR)
	call mfree (xtemp, TY_REAL)
	call mfree (ytemp, TY_REAL)
end
