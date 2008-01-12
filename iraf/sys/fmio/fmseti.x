# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"fmset.h"
include	"fmio.h"

# FM_SETI -- Set the value of an FMIO integer parameter.

procedure fm_seti (fm, param, value)

pointer	fm		#I FMIO descriptor
int	param		#I parameter code from <fmset.h>
int	value		#I new parameter value

int	szbpage

begin
	szbpage = FM_SZBPAGE(fm)

	switch (param) {
	case FM_ACMODE:
	    ; # read-only
	case FM_MAXLFILES:
	    FM_NLFILES(fm) = value
	case FM_MAXPTPAGES:
	    FM_PTILEN(fm) = value
	case FM_OSCHAN:
	    FM_CHAN(fm) = value
	case FM_PAGESIZE:
	    FM_SZBPAGE(fm) = value
	case FM_VERSION:
	    ; # read-only
	case FM_OPTFBSIZE:
	    FM_OPTBUFSIZE(fm) = value
	case FM_MAXFBSIZE:
	    FM_MAXBUFSIZE(fm) = value
	case FM_FCACHESIZE:
	    FM_SZFCACHE(fm) = value
	}
end
