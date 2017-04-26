# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<pmset.h>
include	<plio.h>

# PM_SETPLANE -- Set the 2-Dim plane to be referenced in calls to the pm_box,
# pm_circle, etc. geometric region masking operators.

procedure pm_setplane (pl, v)

pointer	pl			#I mask descriptor
long	v[ARB]			#I vector defining plane

include	"pmio.com"

begin
	if (PM_MAPXY(pl) == YES) {
	    call imaplv (PM_REFIM(pl), v, v1, PM_MAXDIM)
	    call pl_setplane (pl, v1)
	} else
	    call pl_setplane (pl, v)
end
