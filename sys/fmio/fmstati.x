# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"fmset.h"
include	"fmio.h"

# FM_STATI -- Query an FMIO integer parameter.

int procedure fm_stati (fm, param)

pointer	fm		#I FMIO descriptor
int	param		#I parameter code from <fmset.h>

begin
	switch (param) {
	case FM_ACMODE:
	    return (FM_MODE(fm))
	case FM_MAXLFILES:
	    return (FM_NLFILES(fm))
	case FM_MAXPTPAGES:
	    return (FM_PTILEN(fm))
	case FM_OSCHAN:
	    return (FM_CHAN(fm))
	case FM_PAGESIZE:
	    return (FM_SZBPAGE(fm))
	case FM_VERSION:
	    return (FM_DFVERSION(fm))
	case FM_OPTFBSIZE:
	    return (FM_OPTBUFSIZE(fm))
	case FM_MAXFBSIZE:
	    return (FM_MAXBUFSIZE(fm))
	case FM_FCACHESIZE:
	    return (FM_SZFCACHE(fm))
	default:
	    return (ERR)
	}
end
