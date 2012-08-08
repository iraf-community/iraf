include	"../lib/daophotdef.h"
include "../lib/psfdef.h"

# DP_PSETS -- Set a psf fitting string parameter.
#
#procedure dp_psets (dao, param, str)

#pointer	dao		# pointer to daophot structure
#int	param		# parameter
#char	str[ARB]	# string value

#begin
#	switch (param) {
#	default:
#	    call error (0, "DP_PSETS: Unknown psf fitting string parameter")
#	}
#end


# DP_PSETI -- Set a daophot psf fitting integer parameter.

procedure dp_pseti (dao, param, ival)

pointer	dao		# pointer to daophot structure
int	param		# parameter
int	ival		# integer value

pointer	psf

begin
	psf = DP_PSF(dao)

	switch (param) {
	case CUR_PSF:
	    DP_CUR_PSF(psf) = ival
	case CUR_PSFID:
	    DP_CUR_PSFID(psf) = ival
	case PNUM:
	    DP_PNUM(psf) = ival
	case PLOTTYPE:
	    DP_PLOTTYPE(psf) = ival
	case LENUSERAREA:
	    DP_LENUSERAREA(psf) = ival
	default:
	    call error (0, "DP_PSETI: Unknown integer psf fitting parameter")
	}
end


# DP_PSETR -- Set a real psf fitting parameter.

procedure dp_psetr (dao, param, rval)

pointer	dao		# pointer to daophot structure
int	param		# parameter
real	rval		# real value

pointer	psf

begin
	psf = DP_PSF(dao)

	switch (param) {
	case CUR_PSFX:
	    DP_CUR_PSFX(psf) = rval
	case CUR_PSFY:
	    DP_CUR_PSFY(psf) = rval
	case CUR_PSFSKY:
	    DP_CUR_PSFSKY(psf) = rval
	case CUR_PSFMAG:
	    DP_CUR_PSFMAG(psf) = rval
	case CUR_PSFMIN:
	    DP_CUR_PSFMIN(psf) = rval
	case CUR_PSFMAX:
	    DP_CUR_PSFMAX(psf) = rval
	case CUR_PSFGMAX:
	    DP_CUR_PSFGMAX(psf) = rval
	default:
	    call error (0, "DP_SETR: Unknown real psf fitting parameter")
	}
end
