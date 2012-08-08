include "../../lib/astromdef.h"
include "../../lib/astrom.h"
include "../../lib/aimparsdef.h"
include "../../lib/aimpars.h"

# AT_SETI -- Set the value of an astrom integer parameter.

procedure at_seti (at, parameter, value)

pointer at                      #I the pointer to the main astrom structure
int     parameter               #I the parameter to be set
int     value                   #I the value of the parameter to be set

pointer	fcp, fsp, wcp
string	iestring "AT_SETI: Cannot set undefined integer parameter"

begin
	fcp = AT_PRCENTER(at)
	fsp = AT_PFILTER(at)
	wcp = AT_PWCS(at)

	switch (parameter) {

        case RCRAUNITS:
	    if (fcp == NULL)
	        call error (0, iestring)
	    else
                AT_RCRAUNITS(fcp) = value
        case RCDECUNITS:
	    if (fcp == NULL)
	        call error (0, iestring)
	    else
                AT_RCDECUNITS(fcp) = value


	case FREVERSE:
	    if (fsp == NULL)
	        call error (0, iestring)
	    else
                AT_FREVERSE(fsp) = value
	case FREPLACE:
	    if (fsp == NULL)
	        call error (0, iestring)
	    else
                AT_FREPLACE(fsp) = value
	case FORAUNITS:
	    if (fsp == NULL)
	        call error (0, iestring)
	    else
                AT_FORAUNITS(fsp) = value
	case FODECUNITS:
	    if (fsp == NULL)
	        call error (0, iestring)
	    else
                AT_FODECUNITS(fsp) = value

	case WRAUNITS:
	    if (wcp == NULL)
	        call error (0, iestring)
	    else
                AT_WRAUNITS(wcp) = value
	case WDECUNITS:
	    if (wcp == NULL)
	        call error (0, iestring)
	    else
                AT_WDECUNITS(wcp) = value

	default:
	    call error (0, iestring)
	}
end


# AT_SETP -- Set the value of an astrom pointer parameter.

procedure at_setp (at, parameter, value)

pointer at                      #I the pointer to the main astrom structure
int     parameter               #I the parameter to be set
pointer value                   #I the value of the parameter to be set

pointer	fcp, wcp, ipp
string	pestring "AT_SETP: Cannot set undefined pointer parameter"

begin
	fcp = AT_PRCENTER(at)
	wcp = AT_PWCS(at)
	ipp = AT_PIMPARS(at)

	switch (parameter) {
	case PIO:
	    AT_PIO(at) = value
        case PRCENTER:
            AT_PRCENTER(at) = value
        case PFILTER:
            AT_PFILTER(at) = value
        case PWCS:
            AT_PWCS(at) = value
        case PIMPARS:
            AT_PIMPARS(at) = value

        #case RCCC:
            #AT_RCCC(fcp) = value
        case RCST:
	    if (fcp == NULL)
	        call error (0, pestring)
	    else
                AT_RCST(fcp) = value

        case WCST:
	    if (wcp == NULL)
	        call error (0, pestring)
	    else
                AT_WCST(wcp) = value

        case IMST:
	    if (ipp == NULL)
	        call error (0, pestring)
	    else
                AT_IMST(ipp) = value

	default:
	    call error (0, pestring)
	}
end


# AT_SETR -- Set the value of an astrom real parameter.

procedure at_setr (at, parameter, value)

pointer at                      #I the pointer to the main astrom structure
int     parameter               #I the parameter to be set
real    value                   #I the value of the parameter to be set

pointer	fcp, ipp
string	restring "AT_SETR: Cannot set undefined real parameter"	

begin
	fcp = AT_PRCENTER(at)
	ipp = AT_PIMPARS(at)

	switch (parameter) {

        case ESITEALT:
	    if (ipp == NULL)
	        call error (0, restring)
	    else
                AT_ESITEALT(ipp) = value
        case ESITETZ:
	    if (ipp == NULL)
	        call error (0, restring)
	    else
                AT_ESITETZ(ipp) = value
        case EDATAMIN:
	    if (ipp == NULL)
	        call error (0, restring)
	    else
                AT_EDATAMIN(ipp) = value
        case EDATAMAX:
	    if (ipp == NULL)
	        call error (0, restring)
	    else
                AT_EDATAMAX(ipp) = value
        case EGAIN:
	    if (ipp == NULL)
	        call error (0, restring)
	    else
                AT_EGAIN(ipp) = value
        case ERDNOISE:
	    if (ipp == NULL)
	        call error (0, restring)
	    else
                AT_ERDNOISE(ipp) = value
        case EWAVLEN:
	    if (ipp == NULL)
	        call error (0, restring)
	    else
                AT_EWAVLEN(ipp) = value
        case ETEMP:
	    if (ipp == NULL)
	        call error (0, restring)
	    else
                AT_ETEMP(ipp) = value
        case EPRESS:
	    if (ipp == NULL)
	        call error (0, restring)
	    else
                AT_EPRESS(ipp) = value

	default:
	    call error (0, restring)
	}
end


# AT_SETD -- Set the value of an astrom double parameter.

procedure at_setd (at, parameter, value)

pointer at                      #I the pointer to the main astrom structure
int     parameter               #I the parameter to be set
double  value                   #I the value of the parameter to be set

pointer	fcp, wcp, ipp
string	destring "AT_SETD: Cannot set undefined double parameter"

begin
	fcp = AT_PRCENTER(at)
	wcp = AT_PWCS(at)
	ipp = AT_PIMPARS(at)

	switch (parameter) {

        case RCRA:
	    if (fcp == NULL)
	        call error (0, destring)
	    else
                AT_RCRA(fcp) = value
        case RCDEC:
	    if (fcp == NULL)
	        call error (0, destring)
	    else
                AT_RCDEC(fcp) = value
        case RCRAWIDTH:
	    if (fcp == NULL)
	        call error (0, destring)
	    else
                AT_RCRAWIDTH(fcp) = value
        case RCDECWIDTH:
	    if (fcp == NULL)
	        call error (0, destring)
	    else
                AT_RCDECWIDTH(fcp) = value

        case WXREF:
	    if (wcp == NULL)
	        call error (0, destring)
	    else
                AT_WXREF(wcp) = value
        case WYREF:
	    if (wcp == NULL)
	        call error (0, destring)
	    else
                AT_WYREF(wcp) = value
        case WXMAG:
	    if (wcp == NULL)
	        call error (0, destring)
	    else
                AT_WXMAG(wcp) = value
        case WYMAG:
	    if (wcp == NULL)
	        call error (0, destring)
	    else
                AT_WYMAG(wcp) = value
        case WXROT:
	    if (wcp == NULL)
	        call error (0, destring)
	    else
                AT_WXROT(wcp) = value
        case WYROT:
	    if (wcp == NULL)
	        call error (0, destring)
	    else
                AT_WYROT(wcp) = value
        case WRAREF:
	    if (wcp == NULL)
	        call error (0, destring)
	    else
                AT_WRAREF(wcp) = value
        case WDECREF:
	    if (wcp == NULL)
	        call error (0, destring)
	    else
                AT_WDECREF(wcp) = value

        case ESITELNG:
	    if (ipp == NULL)
	        call error (0, destring)
	    else
                AT_ESITELNG(ipp) = value
        case ESITELAT:
	    if (ipp == NULL)
	        call error (0, destring)
	    else
                AT_ESITELAT(ipp) = value
        case EMJDOBS:
	    if (ipp == NULL)
	        call error (0, destring)
	    else
                AT_EMJDOBS(ipp) = value
        #case UT:
	    #if (ipp == NULL)
	        #call error (0,
		    #"AT_SETD: Cannot set undefined double parameter")
	    #else
                #AT_UT(ipp) = value

	default:
	    call error (0, destring)
	}
end


# AT_SETS -- Set the value of an astrom string parameter.

procedure at_sets (at, parameter, value)

pointer at                      #I the pointer to the main astrom structure
int     parameter               #I the parameter to be set
char    value[ARB]              #I the value of the parameter to be set

pointer	fcp, iop, fsp, wcp, ipp
string	sestring "AT_SETS: Cannot set undefined string parameter"

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
	        call strcpy (value, AT_CATALOGS(iop), SZ_FNAME)
	case SURVEYS:
	    if (iop == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (value, AT_SURVEYS(iop), SZ_FNAME)
	case IMAGES:
	    if (iop == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (value, AT_IMAGES(iop), SZ_FNAME)
	case INPUT:
	    if (iop == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (value, AT_INPUT(iop), SZ_FNAME)
	case OUTPUT:
	    if (iop == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (value, AT_OUTPUT(iop), SZ_FNAME)
	case CATNAME:
	    if (iop == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (value, AT_CATNAME(iop), SZ_FNAME)
	case SVNAME:
	    if (iop == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (value, AT_SVNAME(iop), SZ_FNAME)
	case IMNAME:
	    if (iop == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (value, AT_IMNAME(iop), SZ_FNAME)
	case INFNAME:
	    if (iop == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (value, AT_INFNAME(iop), SZ_FNAME)
	case OUTFNAME:
	    if (iop == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (value, AT_OUTFNAME(iop), SZ_FNAME)
	case CATDB:
	    if (iop == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (value, AT_CATDB(iop), SZ_FNAME)
	case IMDB:
	    if (iop == NULL)
	        call error (0, sestring)
	    else
	        call strcpy (value, AT_IMDB(iop), SZ_FNAME)

        case RCSYSTEM:
	    if (fcp == NULL)
	        call error (0, sestring)
	    else
                call strcpy (value, AT_RCSYSTEM(fcp), SZ_FNAME)
        case RCSOURCE:
	    if (fcp == NULL)
	        call error (0, sestring)
	    else
                call strcpy (value, AT_RCSOURCE(fcp), SZ_FNAME)


	case FSORT:
	    if (fsp == NULL)
	        call error (0, sestring)
	    else
		call strcpy (value, AT_FSORT(fsp), SZ_FNAME) 
	case FOSYSTEM:
	    if (fsp == NULL)
	        call error (0, sestring)
	    else
		call strcpy (value, AT_FOSYSTEM(fsp), SZ_FNAME) 
	case FIRA:
	    if (fsp == NULL)
	        call error (0, sestring)
	    else
		call strcpy (value, AT_FIRA(fsp), SZ_FNAME) 
	case FIDEC:
	    if (fsp == NULL)
	        call error (0, sestring)
	    else
		call strcpy (value, AT_FIDEC(fsp), SZ_FNAME) 
	case FORAFORMAT:
	    if (fsp == NULL)
	        call error (0, sestring)
	    else
		call strcpy (value, AT_FORAFORMAT(fsp), SZ_FNAME) 
	case FODECFORMAT:
	    if (fsp == NULL)
	        call error (0, sestring)
	    else
		call strcpy (value, AT_FODECFORMAT(fsp), SZ_FNAME) 
	case FIXP:
	    if (fsp == NULL)
	        call error (0, sestring)
	    else
		call strcpy (value, AT_FIXP(fsp), SZ_FNAME) 
	case FIYP:
	    if (fsp == NULL)
	        call error (0, sestring)
	    else
		call strcpy (value, AT_FIYP(fsp), SZ_FNAME) 
	case FIXC:
	    if (fsp == NULL)
	        call error (0, sestring)
	    else
		call strcpy (value, AT_FIXC(fsp), SZ_FNAME) 
	case FIYC:
	    if (fsp == NULL)
	        call error (0, sestring)
	    else
		call strcpy (value, AT_FIYC(fsp), SZ_FNAME) 
	case FOXFORMAT:
	    if (fsp == NULL)
	        call error (0, sestring)
	    else
		call strcpy (value, AT_FOXFORMAT(fsp), SZ_FNAME) 
	case FOYFORMAT:
	    if (fsp == NULL)
	        call error (0, sestring)
	    else
		call strcpy (value, AT_FOYFORMAT(fsp), SZ_FNAME) 
	case FIELDS:
	    if (fsp == NULL)
	        call error (0, sestring)
	    else
		call strcpy (value, AT_FIELDS(fsp), SZ_FNAME) 
	case FEXPR:
	    if (fsp == NULL)
	        call error (0, sestring)
	    else
		call strcpy (value, AT_FEXPR(fsp), SZ_FNAME) 
	case FNAMES:
	    if (fsp == NULL)
	        call error (0, sestring)
	    else
		call strcpy (value, AT_FNAMES(fsp), SZ_FNAME) 
	case FNTYPES:
	    if (fsp == NULL)
	        call error (0, sestring)
	    else
		call strcpy (value, AT_FNTYPES(fsp), SZ_FNAME) 
	case FNUNITS:
	    if (fsp == NULL)
	        call error (0, sestring)
	    else
		call strcpy (value, AT_FNUNITS(fsp), SZ_FNAME) 
	case FNFORMATS:
	    if (fsp == NULL)
	        call error (0, sestring)
	    else
		call strcpy (value, AT_FNFORMATS(fsp), SZ_FNAME) 

	case WPROJ:
	    if (wcp == NULL)
	        call error (0, sestring)
	    else
		call strcpy (value, AT_WPROJ(wcp), SZ_FNAME) 
	case WSYSTEM:
	    if (wcp == NULL)
	        call error (0, sestring)
	    else
		call strcpy (value, AT_WSYSTEM(wcp), SZ_FNAME) 

	case OBSERVAT:
	    if (ipp == NULL)
	        call error (0, sestring)
	    else
		call strcpy (value, AT_OBSERVAT(ipp), SZ_FNAME) 

	default:
	    call error (0, sestring)
	}
end
