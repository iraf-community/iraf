include <mach.h>
include <ctype.h>
include <error.h>
include <gset.h>
include "pdm.h"

define	MSIZE		2.0		# Mark size
define	EDGESCL	5.0			# Percent extra space around plot data

# PDM_PPLOT -- Plot the Phase curve.

procedure pdm_pplot (pdmp, period, filename, flip)

pointer	pdmp			# pointer to PDM data structure
double	period			# period for which to plot the phase curve
char	filename[SZ_FNAME]	# name of the input data file
bool	flip			# flip the y-axis scale

int	npt, i, index
double	frequency
real	x1, x2, y1, y2, scldif, sclspc
pointer	gp, xtemp, ytemp, etemp
pointer	title, system_id, sp

begin
	npt = PDM_NPT(pdmp)
	gp = PDM_GP(pdmp)
	call gclear (PDM_GP(pdmp))

	call smark (sp)
	call salloc (system_id, SZ_LINE, TY_CHAR)
	call salloc (title, PDM_SZ_TITLE, TY_CHAR)
	call salloc (xtemp, 2*npt, TY_REAL)
	call salloc (ytemp, 2*npt, TY_REAL)
	call salloc (etemp, 2*npt, TY_REAL)

	do i = 1, 2*npt {
	    Memr[xtemp+i-1] = PDM_XPH(pdmp,i)
	    Memr[ytemp+i-1] = PDM_YPH(pdmp,i)
	    Memr[etemp+i-1] = PDM_PHERR(pdmp,i)
	}

	# Scale the wcs.
	call gascale (gp, Memr[xtemp], 2*npt, 1)
	call gascale (gp, Memr[ytemp], 2*npt, 2)

	# Get the boundaries in X and Y.
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

	# Flip the y-axis scale if flip = TRUE.
	if (flip)
	    call gswind (gp, x1, x2, y2, y1)
	else
	    call gswind (gp, x1, x2, y1, y2)

	# Multiline title, save in an array and sprintf to it.
	# Get the system identification.

	call sysid (Memc[system_id], SZ_LINE)

	# Calculate the frequency.
	if (period != 0.0)
	    frequency = 1.0d+0/period

	call sprintf (Memc[title], PDM_SZ_TITLE,
"%s\nPhase curve at period %12.12g, frequency %12.12g\nFile = %s, numpts = %d")
	    call pargstr (Memc[system_id])
	    call pargd (period)
	    call pargd (frequency)
	    call pargstr (filename)
	    call pargi (npt)

	# Draw the axes.
	call glabax (gp, Memc[title], "phase", "magnitude")

	# Make the plot. If error bars are turned on, draw them.
	if (PDM_EB(pdmp) == YES && Memr[etemp] > EPSILONR) {
	    call gpmark (gp, Memr[xtemp], Memr[ytemp],
			2*npt, GM_CIRCLE, 1.0, 1.0)
	    call gpmark (gp, Memr[xtemp], Memr[ytemp],
			2*npt, GM_CIRCLE+GM_FILL, 1.0, 1.0)
	    do i = 1, 2*npt
		call gpmark (gp, Memr[xtemp+i-1], Memr[ytemp+i-1],
			     1, GM_VEBAR, MSIZE, -(2.0*Memr[etemp+i-1]))
	} else {

	    if (npt <= PDM_PLUSPOINT(pdmp)) {
		call gpmark (gp, Memr[xtemp], Memr[ytemp],
			    2*npt, GM_PLUS, MSIZE, MSIZE)
	    } else {
		call gpmark (gp, Memr[xtemp], Memr[ytemp],
			    2*npt, GM_POINT, 1.0, 1.0)
	    }
	}

	# Draw an x over any deleted points.
	do i = 1, npt {
	    index = PDM_SORT(pdmp,i)
	    if (PDM_INUSE(pdmp,index) == 0) {
	        call gscur (gp, Memr[xtemp+i-1]+1, Memr[ytemp+i-1])
	        call gmark (gp, Memr[xtemp+i-1]+1, Memr[ytemp+i-1], GM_CROSS,
		    MSIZE, MSIZE)
	        call gscur (gp, Memr[xtemp+i-1], Memr[ytemp+i-1])
	        call gmark (gp, Memr[xtemp+i-1], Memr[ytemp+i-1], GM_CROSS,
		    MSIZE, MSIZE)
	    }
	}

	call sfree (sp)
end
