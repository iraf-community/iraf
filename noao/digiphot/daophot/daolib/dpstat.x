include	"../lib/daophotdef.h"
include "../lib/apseldef.h"

# DP_STATS -- Fetch a daophot string parameter.

procedure dp_stats (dao, param, str, maxch)

pointer	dao		# pointer to daophot structure
int	param		# parameter
char	str[ARB]	# string value
int	maxch		# maximum number of characters

begin
	switch (param) {
	case INIMAGE:
	    call strcpy (DP_INIMAGE(dao), str, maxch)
	case INPHOTFILE:
	    call strcpy (DP_INPHOTFILE(dao), str, maxch)
	case COORDS:
	    call strcpy (DP_COORDS(dao), str, maxch)
	case PSFIMAGE:
	    call strcpy (DP_PSFIMAGE(dao), str, maxch)
	case OUTPHOTFILE:
	    call strcpy (DP_OUTPHOTFILE(dao), str, maxch)
	case OUTIMAGE:
	    call strcpy (DP_OUTIMAGE(dao), str, maxch)
	case OUTREJFILE:
	    call strcpy (DP_OUTREJFILE(dao), str, maxch)
	case IFILTER:
	    call strcpy (DP_IFILTER(dao), str, maxch)
	case OTIME:
	    call strcpy (DP_OTIME(dao), str, maxch)
	case CCDGAIN:
	    call strcpy (DP_CCDGAIN(dao), str, maxch)
	case CCDREAD:
	    call strcpy (DP_CCDREAD(dao), str, maxch)
	case EXPTIME:
	    call strcpy (DP_EXPTIME(dao), str, maxch)
	case OBSTIME:
	    call strcpy (DP_OBSTIME(dao), str, maxch)
	case FILTER:
	    call strcpy (DP_FILTER(dao), str, maxch)
	case AIRMASS:
	    call strcpy (DP_AIRMASS(dao), str, maxch)
	case FUNCTION:
	    call strcpy (DP_FUNCTION(dao), str, maxch)
	case FUNCLIST:
	    call strcpy (DP_FUNCLIST(dao), str, maxch)
	default:
	    call error (0, "DP_STATS: Unknown daophot string parameter")
	}
end


# DP_STATI -- Fetch a daophot integer parameter.

int procedure dp_stati (dao, param)

pointer	dao		# pointer to daophot structure
int	param		# parameter

pointer	apsel

begin
	apsel = DP_APSEL(dao)

	switch (param) {
	case MW:
	    return (DP_MW(dao))
	case WCSIN:
	    return (DP_WCSIN(dao))
	case WCSOUT:
	    return (DP_WCSOUT(dao))
	case WCSPSF:
	    return (DP_WCSPSF(dao))
	case CTIN:
	    return (DP_CTIN(dao))
	case CTOUT:
	    return (DP_CTOUT(dao))
	case CTPSF:
	    return (DP_CTPSF(dao))
	case MAXITER:
	    return (DP_MAXITER(dao))
	case VERBOSE:
	    return (DP_VERBOSE(dao))
	case TEXT:
	    return (DP_TEXT(dao))
	case MAXNSTAR:
	    return (DP_MAXNSTAR(dao))
	case MAXGROUP:
	    return (DP_MAXGROUP(dao))
	case CLIPEXP:
	    return (DP_CLIPEXP(dao))
	case RECENTER:
	    return (DP_RECENTER(dao))
	case FITSKY:
	    return (DP_FITSKY(dao))
	case GROUPSKY:
	    return (DP_GROUPSKY(dao))
	case VARORDER:
	    return (DP_VARORDER(dao))
	case FEXPAND:
	    return (DP_FEXPAND(dao))
	case SATURATED:
	    return (DP_SATURATED(dao))
	case NCLEAN:
	    return (DP_NCLEAN(dao))
	case APNUM:
	    return (DP_APNUM(apsel))
	default:
	    call error (0, "DP_STATI: Unknown integer daophot parameter")
	}
end


# DP_STATR -- Fetch a daophot real parameter.

real procedure dp_statr (dao, param)

pointer	dao		# pointer to daophot structure
int	param		# parameter

begin
	switch (param) {
	case SCALE:
	    return (DP_SCALE(dao))
	case FWHMPSF:
	    return (DP_FWHMPSF(dao))
	case SFWHMPSF:
	    return (DP_SFWHMPSF(dao))
	case MAXGDATA:
	    return (DP_MAXGDATA(dao))
	case MINGDATA:
	    return (DP_MINGDATA(dao))
	case READNOISE:
	    return (DP_READNOISE(dao))
	case PHOTADU:
	    return (DP_PHOTADU(dao))
	case RPSFRAD:
	    return (DP_RPSFRAD(dao))
	case SPSFRAD:
	    return (DP_SPSFRAD(dao))
	case PSFRAD:
	    return (DP_PSFRAD(dao))
	case SFITRAD:
	    return (DP_SFITRAD(dao))
	case FITRAD:
	    return (DP_FITRAD(dao))
	case SMATCHRAD:
	    return (DP_SMATCHRAD(dao))
	case MATCHRAD:
	    return (DP_MATCHRAD(dao))
	case SANNULUS:
	    return (DP_SANNULUS(dao))
	case ANNULUS:
	    return (DP_ANNULUS(dao))
	case SDANNULUS:
	    return (DP_SDANNULUS(dao))
	case DANNULUS:
	    return (DP_DANNULUS(dao))
	case CRITSNRATIO:
	    return (DP_CRITSNRATIO(dao))
	case CLIPRANGE:
	    return (DP_CLIPRANGE(dao))
	case XAIRMASS:
	    return (DP_XAIRMASS(dao))
	case ITIME:
	    return (DP_ITIME(dao))
	case FLATERR:
	    return (DP_FLATERR(dao))
	case PROFERR:
	    return (DP_PROFERR(dao))
	case SMERGERAD:
	    return (DP_SMERGERAD(dao))
	case MERGERAD:
	    return (DP_MERGERAD(dao))
	default:
	    call error (0, "DP_STATR: Unknown real daophot parameter")
	}
end
