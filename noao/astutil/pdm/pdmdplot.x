include <mach.h>
include <ctype.h>
include <error.h>
include <gset.h>
include "pdm.h"

define	MSIZE	2.0			# Mark size
define	EDGESCL	5.0			# Percent extra space around plot data

# PDM_DPLOT -- Plot the data on the screen.

procedure pdm_dplot (pdmp, filename, flip)

pointer	pdmp				# pointer to PDM data structure
char	filename[SZ_FNAME]		# name of the input data file
bool	flip				# flip the y-axis scale

int	npt, i
real	x1, x2, y1, y2, scldif, sclspc
pointer	gp, xtemp, ytemp
pointer	title
pointer	system_id, sp

begin
	call smark (sp)
	call salloc (system_id, SZ_LINE, TY_CHAR)
	call salloc (title, 4*SZ_LINE, TY_CHAR)
	call salloc (xtemp, PDM_NPT(pdmp), TY_REAL)
	call salloc (ytemp, PDM_NPT(pdmp), TY_REAL)

	npt = PDM_NPT(pdmp)
	gp = PDM_GP(pdmp)
	call gclear (gp)

	# Scale the wcs.
	do i = 1, PDM_NPT(pdmp) {
	    Memr[xtemp+i-1] = real(PDM_X(pdmp,i))
	    Memr[ytemp+i-1] = real(PDM_DY(pdmp,i))
	}
	call gascale (gp, Memr[xtemp], npt, 1)
	call gascale (gp, Memr[ytemp], npt, 2)

	# Get the X and Y boundaries.
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
	call sprintf (Memc[title], 4*SZ_LINE,
	    "%s\nFile = %s\n%s\nnumpts = %d")
	    call pargstr (Memc[system_id])
	    call pargstr (filename)
	    if (PDM_RESID(pdmp) == YES)
	        call pargstr ("Data with fit removed")
	    else
	        call pargstr ("Data")
	    call pargi (npt)

	# Draw the axes.
	call glabax (gp, Memc[title], "obs time", "magnitude")

	# Make the plot.
	if (npt <= PDM_PLUSPOINT(pdmp)) {
	    call gpmark (gp, Memr[xtemp], Memr[ytemp], npt,
			GM_PLUS, MSIZE, MSIZE)
	} else {
	    call gpmark (gp, Memr[xtemp], Memr[ytemp], npt,
			GM_POINT, 1.0, 1.0)
	}

	# Call the routine to mark the ranges if they are in effect.
	call rg_gxmarkd (gp, PDM_SAMPLE(pdmp), PDM_X(pdmp,1), npt, 1)

	# Draw an x over any deleted points.
	do i = 1, npt {
	    if (PDM_INUSE(pdmp,i) == 0) {
	        call gscur (gp, Memr[xtemp+i-1], Memr[ytemp+i-1])
	        call gmark (gp, Memr[xtemp+i-1], Memr[ytemp+i-1], GM_CROSS,
		    MSIZE, MSIZE)
	    }
	}

	call sfree (sp)
end
