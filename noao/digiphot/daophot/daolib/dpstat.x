include	"../lib/daophot.h"
include	"../lib/daophotdef.h"

# DP_STATS -- Procedure to set an daophot string parameter.

procedure dp_stats (dao, param, str, maxch)

pointer	dao		# pointer to daophot structure
int	param		# parameter
char	str[ARB]	# string value
int	maxch		# maximum number of characters

begin
	switch (param) {
	case IMNAME:
	    call strcpy (DP_IMNAME(dao), str, maxch)
	case PSFIMAGE:
	    call strcpy (DP_PSFIMAGE(dao), str, maxch)
	case SUBIMAGE:
	    call strcpy (DP_SUBIMAGE(dao), str, maxch)
	case ADDIMAGE:
	    call strcpy (DP_ADDIMAGE(dao), str, maxch)
	case ADDFILE:
	    call strcpy (DP_ADDFILE(dao), str, maxch)
	case APFILE:
	    call strcpy (DP_APFILE(dao), str, maxch)
	case GRPSFFILE:
	    call strcpy (DP_GRPSFFILE(dao), str, maxch)
	case GRPFILE:
	    call strcpy (DP_GRPFILE(dao), str, maxch)
	case PKFILE:
	    call strcpy (DP_PKFILE(dao), str, maxch)
	case NSTARFILE:
	    call strcpy (DP_NSTARFILE(dao), str, maxch)
	case ALLSTARFILE:
	    call strcpy (DP_ASTARFILE(dao), str, maxch)
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
	default:
	    call error (0, "DP_STATS: Unknown daophot string parameter")
	}
end


# DP_STATI -- Procedure to set an integer daophot parameter.

int procedure dp_stati (dao, param)

pointer	dao		# pointer to daophot structure
int	param		# parameter

begin
	switch (param) {
	case MAXITER:
	    return (DP_MAXITER(dao))
	case VERBOSE:
	    return (DP_VERBOSE(dao))
	case TEXT:
	    return (DP_TEXT(dao))
	case MAXSTAR:
	    return (DP_MAXSTAR(dao))
	case MAXGROUP:
	    return (DP_MAXGROUP(dao))
	case CLIPEXP:
	    return (DP_CLIPEXP(dao))
	case RECENTER:
	    return (DP_RECENTER(dao))
	case VARPSF:
	    return (DP_VARPSF(dao))
	default:
	    call error (0, "DP_STATI: Unknown integer daophot parameter")
	}
end


# DP_STATR -- Procedure to set a real daophot parameter.

real procedure dp_statr (dao, param)

pointer	dao		# pointer to daophot structure
int	param		# parameter

begin
	switch (param) {
	case SCALE:
	    return (DP_SCALE(dao))
	case MAXGDATA:
	    return (DP_MAXGDATA(dao))
	case MINGDATA:
	    return (DP_MINGDATA(dao))
	case READ_NOISE:
	    return (DP_READ_NOISE(dao))
	case PHOT_ADC:
	    return (DP_PHOT_ADC(dao))
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
	case CRITOVLAP:
	    return (DP_CRITOVLAP(dao))
	case CLIPRANGE:
	    return (DP_CLIPRANGE(dao))
	case XAIRMASS:
	    return (DP_XAIRMASS(dao))
	case ITIME:
	    return (DP_ITIME(dao))
	default:
	    call error (0, "DP_SETR: Unknown real daophot parameter")
	}
end
