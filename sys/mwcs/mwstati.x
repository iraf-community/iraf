# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<mwset.h>
include	<mach.h>
include	"mwcs.h"

# MW_STATI -- Get the value of a MWCS interface parameter.

int procedure mw_stati (mw, param)

pointer	mw			#I pointer to MWCS descriptor
int	param			#I parameter code as defined in <mwset.h>

begin
	switch (param) {
	case MW_NDIM:
	    if (MI_USEAXMAP(mw) == NO)
		return (MI_NDIM(mw))
	    else
		return (MI_NLOGDIM(mw))
	case MW_NWCS:
	    return (MI_NWCS(mw))
	case MW_REFIM:
	    return (MI_REFIM(mw))
	case MW_USEAXMAP:
	    return (MI_USEAXMAP(mw))
	case MW_NPHYSDIM:
	    return (MI_NDIM(mw))
	case MW_SAVELEN:
	    return (MI_LEN(mw) * SZ_STRUCT + MI_DBUFUSED(mw) * SZ_DOUBLE +
		(MI_SBUFUSED(mw) + SZB_CHAR-1) / SZB_CHAR)
	default:
	    call syserr (SYS_MWSTAT)
	}
end
