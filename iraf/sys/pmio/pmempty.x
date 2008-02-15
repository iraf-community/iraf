# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<pmset.h>
include	<plio.h>

# PM_EMPTY -- Test whether a mask is empty, i.e., contains no nonzero pixels.

bool procedure pm_empty (pl)

pointer	pl			#I mask descriptor

size_t	sz_val
long	lg_val
bool	pl_empty(), pl_sectnotempty()
include	"pmio.com"

begin
	if (PM_MAPXY(pl) == YES) {
	    lg_val = 1
	    sz_val = PM_MAXDIM
	    call amovkl (lg_val, v1, sz_val)
	    call imaplv (PM_REFIM(pl), v1, v2, PM_MAXDIM)
	    sz_val = PM_MAXDIM
	    call amovl (IM_LEN(PM_REFIM(pl),1), v3, sz_val)
	    call imaplv (PM_REFIM(pl), v3, v4, PM_MAXDIM)

	    return (!pl_sectnotempty (pl, v2, v4, PM_MAXDIM))

	} else
	    return (pl_empty (pl))
end
