include <mach.h>
include <ctype.h>
include <error.h>
include <gset.h>
include "pdm.h"

define	MSIZE	2.			# Mark size

# PDM_DPLOT -- Plot the data on the screen.

procedure pdm_dplot (pdmp, filename, flip)

pointer	pdmp				# pointer to PDM data structure
char	filename[SZ_FNAME]		# name of the input data file
bool	flip				# flip the y-axis scale

int	npt, i
real	x1,  x2, y1, y2
pointer	gp
pointer	title
pointer	system_id, sp

begin
	call smark (sp)
	call salloc (system_id, SZ_LINE, TY_CHAR)
	call salloc (title, 4*SZ_LINE, TY_CHAR)

	npt = PDM_NPT(pdmp)
	gp = PDM_GP(pdmp)
	call gclear (gp)

	# Scale the wcs.
	call gascale (gp, PDM_X(pdmp,1), npt, 1)
	call gascale (gp, PDM_DY(pdmp,1), npt, 2)

	# Flip the y-axis scale if flip = TRUE.
	if (flip) {
	    call ggwind (gp, x1, x2, y1, y2)
	    call gswind (gp, x1, x2, y2, y1)
	}

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
	    call gpmark (gp, PDM_X(pdmp,1), PDM_DY(pdmp,1), npt,
			"plus", MSIZE, MSIZE)
	} else {
	    call gpmark (gp, PDM_X(pdmp,1), PDM_DY(pdmp,1), npt,
			"point", 1, 1)
	}

	# Call the routine to mark the ranges if they are in effect.
	call rg_gxmarkr (gp, PDM_SAMPLE(pdmp), PDM_X(pdmp,1), npt, 1)

	# Draw an x over any deleted points.
	do i = 1, npt {
	    if (PDM_INUSE(pdmp,i) == 0) {
	        call gscur (gp, PDM_X(pdmp,i), PDM_DY(pdmp,i))
	        call gmark (gp, PDM_X(pdmp,i), PDM_DY(pdmp,i), GM_CROSS,
		    MSIZE, MSIZE)
	    }
	}

	call sfree (sp)
end
