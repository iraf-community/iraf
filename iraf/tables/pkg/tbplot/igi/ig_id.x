include <gset.h>
include "igi.h"

#  8/20/91 Removed ^Ls. ZGL
## 16 June 1992  Change HIGH_QUALITY to SOFT_FONTS.  ZGL
## 3/15/93  Add version.h include file.

procedure ig_id (igs)

pointer	igs		# igi parameters structure

begin
	call lcmdcat (igs, YES)
	call cmdcat  (igs, YES)

	call ii_id (igs)
end


#  II_ID -- Write the standard system id at the top of the plot

procedure ii_id (igs)

pointer	igs		# igi parameters structure

pointer	igps		# igi parameters structure
pointer	sp, title, id
real	delndc
real	xpos, ypos
real	size
real	angle
int	just
real	vl, vr, vb, vt
char	version[SZ_LINE]

string	prefix	"\\\\tigi"

begin
	igps = PLOT_PARMS(igs)

	call ggview (GIO_GP(igs), vl, vr, vb, vt)

	size = 1.5 * MG_CHARSIZE(igps)
	xpos = vr
	ypos = vt

	if (xpos + size >= MG_PAGERIGHT(igps))
	    # Off the page
	    return

	call smark  (sp)
	call salloc (id, SZ_LINE, TY_CHAR)
	call salloc (title, SZ_LINE, TY_CHAR)
	call sysid  (Memc[id], SZ_LINE)

	call clgstr ("Version", version, SZ_LINE)
	call sprintf (Memc[title], SZ_LINE, "%s %s %s")
	    call pargstr (prefix)
	    call pargstr (version)
	    call pargstr (Memc[id])

	xpos   = min (xpos, MG_PAGERIGHT(igps))
	delndc = MG_PAGERIGHT(igps) - xpos

	# Coordinates in WCS
	call ndc_wcs (igs, xpos, ypos, xpos, ypos)

	size  = min (size, delndc)
	size  = size / MG_CHARSIZE(igps)
	angle = 90.0
	just  = 1

	call setltype (igs, SOLID_LINE)

	call mgostr (igs, xpos, ypos, Memc[title], 
	    size, angle, just, SOFT_FONTS)

	call gflush (GIO_GP(igs))
	call sfree  (sp)
end
