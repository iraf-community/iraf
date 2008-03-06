# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"fmset.h"
include	"fmio.h"

# FM_SET[IL] -- Set the value of an FMIO integer parameter.

procedure fm_setl (fm, param, lvalue)

pointer	fm		#I FMIO descriptor
int	param		#I parameter code from <fmset.h>
long	lvalue		#I new parameter value

long	szbpage

begin
	szbpage = FM_SZBPAGE(fm)

	switch (param) {
	case FM_ACMODE:
	    ; # read-only
	case FM_MAXLFILES:
	    FM_NLFILES(fm) = lvalue
	case FM_MAXPTPAGES:
	    FM_PTILEN(fm) = lvalue
	case FM_OSCHAN:
	    FM_CHAN(fm) = lvalue
	case FM_PAGESIZE:
	    FM_SZBPAGE(fm) = lvalue
	case FM_VERSION:
	    ; # read-only
	case FM_OPTFBSIZE:
	    FM_OPTBUFSIZE(fm) = lvalue
	case FM_MAXFBSIZE:
	    FM_MAXBUFSIZE(fm) = lvalue
	case FM_FCACHESIZE:
	    FM_SZFCACHE(fm) = lvalue
	}
end


procedure fm_seti (fm, param, value)

pointer	fm		#I FMIO descriptor
int	param		#I parameter code from <fmset.h>
int	value		#I new parameter value

long	lvalue

begin
	lvalue = value
	call fm_setl (fm, param, lvalue)
end
