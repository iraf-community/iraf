include "../lib/daophotdef.h"
include "../lib/psfdef.h"
#
## DP_PSTATS -- Fetch a psf fitting string parameter.
#
#procedure dp_pstats (dao, param, str, maxch)
#
#pointer	dao		# pointer to daophot structure
#int	param		# parameter
#char	str[ARB]	# string value
#int	maxch		# maximum number of characters
#
#begin
#	switch (param) {
#	default:
#	    call error (0, "DP_PSTATS: Unknown psf fitting string parameter")
#	}
#end
#

# DP_PSTATI -- Fetch a psf fitting integer parameter.

int procedure dp_pstati (dao, param)

pointer	dao		# pointer to daophot structure
int	param		# parameter

pointer	psf

begin
	psf = DP_PSF(dao)

	switch (param) {
	case CUR_PSF:
	    return (DP_CUR_PSF(psf))
	case CUR_PSFID:
	    return (DP_CUR_PSFID(psf))
	case PNUM:
	    return (DP_PNUM(psf))
	case PLOTTYPE:
	    return (DP_PLOTTYPE(psf))
	case LENUSERAREA:
	    return (DP_LENUSERAREA(psf))
	default:
	    call error (0, "DP_PSTATI: Unknown psf fitting integer parameter")
	}
end


# DP_PSTATR -- Fetch a psf fitting real parameter.

real procedure dp_pstatr (dao, param)

pointer	dao		# pointer to daophot structure
int	param		# parameter

pointer	psf

begin
	psf = DP_PSF(dao)

	switch (param) {
	case CUR_PSFX:
	    return (DP_CUR_PSFX(psf))
	case CUR_PSFY:
	    return (DP_CUR_PSFY(psf))
	case CUR_PSFSKY:
	    return (DP_CUR_PSFSKY(psf))
	case CUR_PSFMAG:
	    return (DP_CUR_PSFMAG(psf))
	case CUR_PSFMIN:
	    return (DP_CUR_PSFMIN(psf))
	case CUR_PSFMAX:
	    return (DP_CUR_PSFMAX(psf))
	case CUR_PSFGMAX:
	    return (DP_CUR_PSFGMAX(psf))
	default:
	    call error (0, "DP_PSTATR: Unknown psf fitting parameter")
	}
end
