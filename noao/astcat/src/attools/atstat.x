include "../../lib/astromdef.h"
include "../../lib/astrom.h"
include "../../lib/aimparsdef.h"
include "../../lib/aimpars.h"


# AT_STATI -- Get the value of an astrom integer parameter.

int procedure at_stati (at, parameter)

pointer at                      #I the pointer to the main astrom structure
int     parameter               #I the parameter to be set

pointer fcp, fsp, wcp
string	iestring "T_STATI: Cannot fetch undefined integer parameter"

begin
	fcp = AT_PRCENTER(at)
	fsp = AT_PFILTER(at)
	wcp = AT_PWCS(at)

	switch (parameter) {

	case RCRAUNITS:
	    if (fcp == NULL)
	        call error (0, iestring)
	    else
	        return (AT_RCRAUNITS(fcp))
	case RCDECUNITS:
	    if (fcp == NULL)
	        call error (0, iestring)
	    else
	        return (AT_RCDECUNITS(fcp))

	case FREVERSE:
	    if (fsp == NULL)
	        call error (0, iestring)
	    else
	        return (AT_FREVERSE(fsp))
	case FREPLACE:
	    if (fsp == NULL)
	        call error (0, iestring)
	    else
	        return (AT_FREPLACE(fsp))
	case FORAUNITS:
	    if (fsp == NULL)
	        call error (0, iestring)
	    else
	        return (AT_FORAUNITS(fsp))
	case FODECUNITS:
	    if (fsp == NULL)
	        call error (0, iestring)
	    else
	        return (AT_FODECUNITS(fsp))

	case WRAUNITS:
	    if (wcp == NULL)
	        call error (0, iestring)
	    else
	        return (AT_WRAUNITS(wcp))
	case WDECUNITS:
	    if (wcp == NULL)
	        call error (0, iestring)
	    else
	        return (AT_WDECUNITS(wcp))

	default:
	    call error (0, "AT_STATI: Cannot fetch unknown integer parameter")
	}
end


# AT_STATP -- Get the value of an astrom pointer parameter.

pointer procedure at_statp (at, parameter)

pointer at                      #I the pointer to the main astrom structure
int     parameter               #I the parameter to be set

pointer fcp, fsp, wcp, ipp
string	pestring "AT_STATP: Cannot fetch undefined pointer parameter"

begin
	fcp = AT_PRCENTER(at)
	fsp = AT_PFILTER(at)
	wcp = AT_PWCS(at)
	ipp = AT_PIMPARS(at)

	switch (parameter) {

	case PIO:
	    return (AT_PIO(at))
	case PRCENTER:
	    return (AT_PRCENTER(at))
	case PFILTER:
	    return (AT_PFILTER(at))
	case PWCS:
	    return (AT_PWCS(at))
	case PIMPARS:
	    return (AT_PIMPARS(at))

	#case RCCC:
	    #return (AT_RCCC(fcp))
	case RCST:
	    if (fcp == NULL)
	        call error (0, pestring)
	    else
	        return (AT_RCST(fcp))

	case WCST:
	    if (wcp == NULL)
		call error (0, pestring)
	    else
		return (AT_WCST(wcp))

	case IMST:
	    if (ipp == NULL)
		call error (0, pestring)
	    else
		return (AT_IMST(ipp))

	default:
	    call error (0,  pestring)
	}
end


# AT_STATR -- Get the value of an astrom real parameter.

real procedure at_statr (at, parameter)

pointer at                      #I the pointer to the main astrom structure
int     parameter               #I the parameter to be set

pointer fcp, ipp
string	restring "AT_STATR: Cannot fetch undefined real parameter"

begin
	fcp = AT_PRCENTER(at)
	ipp = AT_PIMPARS(at)

	switch (parameter) {

	case ESITEALT:
	    if (ipp == NULL)
		call error (0, restring)
	    else
		return (AT_ESITEALT(ipp))
	case ESITETZ:
	    if (ipp == NULL)
		call error (0, restring)
	    else
		return (AT_ESITETZ(ipp))
	#case EXPOSURE:
	    #if (ipp == NULL)
		#call error (0,
		    #"AT_STATR: Cannot fetch undefined real parameter")
	    #else
		#return (AT_EXPOSURE(ipp))
	case EDATAMIN:
	    if (ipp == NULL)
		call error (0, restring)
	    else
		return (AT_EDATAMIN(ipp))
	case EDATAMAX:
	    if (ipp == NULL)
		call error (0, restring)
	    else
		return (AT_EDATAMAX(ipp))
	case EGAIN:
	    if (ipp == NULL)
		call error (0, restring)
	    else
		return (AT_EGAIN(ipp))
	case ERDNOISE:
	    if (ipp == NULL)
		call error (0, restring)
	    else
		return (AT_ERDNOISE(ipp))
	case EWAVLEN:
	    if (ipp == NULL)
		call error (0, restring)
	    else
		return (AT_EWAVLEN(ipp))
	case ETEMP:
	    if (ipp == NULL)
		call error (0, restring)
	    else
		return (AT_ETEMP(ipp))
	case EPRESS:
	    if (ipp == NULL)
		call error (0, restring)
	    else
		return (AT_EPRESS(ipp))

	default:
	    call error (0, restring)
	}
end


# AT_STATD -- Get the value of an astrom double parameter.

double procedure at_statd (at, parameter)

pointer at                      #I the pointer to the main astrom structure
int     parameter               #I the parameter to be set

pointer fcp, wcp, ipp
string	destring "AT_STATD: Cannot fetch undefined double parameter"

begin
	fcp = AT_PRCENTER(at)
	wcp = AT_PWCS(at)
	ipp = AT_PIMPARS(at)

	switch (parameter) {

	case RCRA:
	    if (fcp == NULL)
	        call error (0, destring)
	    else
	        return (AT_RCRA(fcp))
	case RCDEC:
	    if (fcp == NULL)
	        call error (0, destring)
	    else
	        return (AT_RCDEC(fcp))
	case RCRAWIDTH:
	    if (fcp == NULL)
	        call error (0, destring)
	    else
	        return (AT_RCRAWIDTH(fcp))
	case RCDECWIDTH:
	    if (fcp == NULL)
	        call error (0, destring)
	    else
	        return (AT_RCDECWIDTH(fcp))

	case WXREF:
	    if (wcp == NULL)
	        call error (0, destring)
	    else
	        return (AT_WXREF(wcp))
	case WYREF:
	    if (wcp == NULL)
	        call error (0, destring)
	    else
	        return (AT_WYREF(wcp))
	case WXMAG:
	    if (wcp == NULL)
	        call error (0, destring)
	    else
	        return (AT_WXMAG(wcp))
	case WYMAG:
	    if (wcp == NULL)
	        call error (0, destring)
	    else
	        return (AT_WXMAG(wcp))
	case WXROT:
	    if (wcp == NULL)
	        call error (0, destring)
	    else
	        return (AT_WXROT(wcp))
	case WYROT:
	    if (wcp == NULL)
	        call error (0, destring)
	    else
	        return (AT_WYROT(wcp))
	case WRAREF:
	    if (wcp == NULL)
	        call error (0, destring)
	    else
	        return (AT_WRAREF(wcp))
	case WDECREF:
	    if (wcp == NULL)
	        call error (0, destring)
	    else
	        return (AT_WDECREF(wcp))

	case ESITELNG:
	    if (ipp == NULL)
	        call error (0, destring)
	    else
	        return (AT_ESITELNG(ipp))
	case ESITELAT:
	    if (ipp == NULL)
	        call error (0, destring)
	    else
	        return (AT_ESITELAT(ipp))
	case EMJDOBS:
	    if (ipp == NULL)
	        call error (0, destring)
	    else
	        return (AT_EMJDOBS(ipp))

	default:
	    call error (0, destring)
	}
end


# AT_STATS -- Get the value of an astrom string parameter.

procedure at_stats (at, parameter, value, maxch)

pointer at                      #I the pointer to the main astrom structure
int     parameter               #I the parameter to be set
char    value[ARB]              #O the value of the parameter to be set
int     maxch                   #I the maximum number of characters

pointer	fcp, iop, fsp, wcp, ipp
string sestring "AT_STATS: Cannot fetch undefined string parameter"

begin
	iop = AT_PIO(at)
	fcp = AT_PRCENTER(at)
	fsp = AT_PFILTER(at)
	wcp = AT_PWCS(at)
	ipp = AT_PIMPARS(at)

	switch (parameter) {

	case CATALOGS:
	    if (iop == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (AT_CATALOGS(iop), value, maxch)
	case SURVEYS:
	    if (iop == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (AT_SURVEYS(iop), value, maxch)
	case IMAGES:
	    if (iop == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (AT_IMAGES(iop), value, maxch)
	case INPUT:
	    if (iop == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (AT_INPUT(iop), value, maxch)
	case OUTPUT:
	    if (iop == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (AT_OUTPUT(iop), value, maxch)
	case CATNAME:
	    if (iop == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (AT_CATNAME(iop), value, maxch)
	case SVNAME:
	    if (iop == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (AT_SVNAME(iop), value, maxch)
	case IMNAME:
	    if (iop == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (AT_IMNAME(iop), value, maxch)
	case INFNAME:
	    if (iop == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (AT_INFNAME(iop), value, maxch)
	case OUTFNAME:
	    if (iop == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (AT_OUTFNAME(iop), value, maxch)
	case CATDB:
	    if (iop == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (AT_CATDB(iop), value, maxch)
	case IMDB:
	    if (iop == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (AT_IMDB(iop), value, maxch)

	case RCSYSTEM:
	    if (fcp == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (AT_RCSYSTEM(fcp), value, maxch)
	case RCSOURCE:
	    if (fcp == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (AT_RCSOURCE(fcp), value, maxch)

	case FSORT:
	    if (fsp == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (AT_FSORT(fsp), value, maxch)
	case FOSYSTEM:
	    if (fsp == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (AT_FOSYSTEM(fsp), value, maxch)
	case FIRA:
	    if (fsp == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (AT_FIRA(fsp), value, maxch)
	case FIDEC:
	    if (fsp == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (AT_FIDEC(fsp), value, maxch)
	case FORAFORMAT:
	    if (fsp == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (AT_FORAFORMAT(fsp), value, maxch)
	case FODECFORMAT:
	    if (fsp == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (AT_FODECFORMAT(fsp), value, maxch)
	case FIXP:
	    if (fsp == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (AT_FIXP(fsp), value, maxch)
	case FIYP:
	    if (fsp == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (AT_FIYP(fsp), value, maxch)
	case FIXC:
	    if (fsp == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (AT_FIXC(fsp), value, maxch)
	case FIYC:
	    if (fsp == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (AT_FIYC(fsp), value, maxch)
	case FOXFORMAT:
	    if (fsp == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (AT_FOXFORMAT(fsp), value, maxch)
	case FOYFORMAT:
	    if (fsp == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (AT_FOYFORMAT(fsp), value, maxch)
	case FIELDS:
	    if (fsp == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (AT_FIELDS(fsp), value, maxch)
	case FEXPR:
	    if (fsp == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (AT_FEXPR(fsp), value, maxch)
	case FNAMES:
	    if (fsp == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (AT_FNAMES(fsp), value, maxch)
	case FNTYPES:
	    if (fsp == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (AT_FNTYPES(fsp), value, maxch)
	case FNUNITS:
	    if (fsp == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (AT_FNUNITS(fsp), value, maxch)
	case FNFORMATS:
	    if (fsp == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (AT_FNFORMATS(fsp), value, maxch)

	case WPROJ:
	    if (wcp == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (AT_WPROJ(wcp), value, maxch)
	case WSYSTEM:
	    if (wcp == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (AT_WSYSTEM(wcp), value, maxch)

	case OBSERVAT:
	    if (ipp == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (AT_OBSERVAT(ipp), value, maxch)
	default:
	    call error (0, sestring)
	}
end
