# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<pmset.h>
include	<plio.h>

# PM_CLEAR -- Clear a mask.  The entire surface is cleared.  This is equivalent
# to a full surface pm_rop with rop=PIX_CLR, but is more convenient and can be
# implemented more efficiently since the entire surface is cleared.

procedure pm_clear (pl)

pointer	pl			#I mask descriptor

include	"pmio.com"

begin
	if (PM_MAPXY(pl) == YES) {
	    call amovkl (1, v1, PM_MAXDIM)
	    call imaplv (PM_REFIM(pl), v1, v2, PM_MAXDIM)
	    call amovl (IM_LEN(PM_REFIM(pl),1), v3, PM_MAXDIM)
	    call imaplv (PM_REFIM(pl), v3, v4, PM_MAXDIM)

	    call pl_rop (NULL, NULL, pl, v2, v4, PIX_CLR)

	} else
	    call pl_clear (pl)
end
