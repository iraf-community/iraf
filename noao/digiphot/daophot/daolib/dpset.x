include	"../lib/daophot.h"
include	"../lib/daophotdef.h"

# DP_SETS -- Procedure to set an daophot string parameter.

procedure dp_sets (dao, param, str)

pointer	dao		# pointer to daophot structure
int	param		# parameter
char	str[ARB]	# string value

begin
	switch (param) {
	case IMNAME:
	    call strcpy (str, DP_IMNAME(dao), SZ_FNAME)
	case PSFIMAGE:
	    call strcpy (str, DP_PSFIMAGE(dao), SZ_FNAME)
	case SUBIMAGE:
	    call strcpy (str, DP_SUBIMAGE(dao), SZ_FNAME)
	case ADDIMAGE:
	    call strcpy (str, DP_ADDIMAGE(dao), SZ_FNAME)
	case ADDFILE:
	    call strcpy (str, DP_ADDFILE(dao), SZ_FNAME)
	case APFILE:
	    call strcpy (str, DP_APFILE(dao), SZ_FNAME)
	case GRPSFFILE:
	    call strcpy (str, DP_GRPSFFILE(dao), SZ_FNAME)
	case GRPFILE:
	    call strcpy (str, DP_GRPFILE(dao), SZ_FNAME)
	case PKFILE:
	    call strcpy (str, DP_PKFILE(dao), SZ_FNAME)
	case NSTARFILE:
	    call strcpy (str, DP_NSTARFILE(dao), SZ_FNAME)
	case ALLSTARFILE:
	    call strcpy (str, DP_ASTARFILE(dao), SZ_FNAME)
	case IFILTER:
	    call strcpy (str, DP_IFILTER(dao), SZ_FNAME)
	case OTIME:
	    call strcpy (str, DP_OTIME(dao), SZ_FNAME)
	case CCDGAIN:
	    call strcpy (str, DP_CCDGAIN(dao), SZ_FNAME)
	case CCDREAD:
	    call strcpy (str, DP_CCDREAD(dao), SZ_FNAME)
	case EXPTIME:
	    call strcpy (str, DP_EXPTIME(dao), SZ_FNAME)
	case OBSTIME:
	    call strcpy (str, DP_OBSTIME(dao), SZ_FNAME)
	case AIRMASS:
	    call strcpy (str, DP_AIRMASS(dao), SZ_FNAME)
	case FILTER:
	    call strcpy (str, DP_FILTER(dao), SZ_FNAME)
	default:
	    call error (0, "DP_SETS: Unknown daophot string parameter")
	}
end


# DP_SETI -- Procedure to set an integer daophot parameter.

procedure dp_seti (dao, param, ival)

pointer	dao		# pointer to daophot structure
int	param		# parameter
int	ival		# integer value

begin
	switch (param) {
	case MAXITER:
	    DP_MAXITER(dao) = ival
	case VERBOSE:
	    DP_VERBOSE(dao) = ival
	case TEXT:
	    DP_TEXT(dao) = ival
	case MAXSTAR:
	    DP_MAXSTAR(dao) = ival
	case MAXGROUP:
	    DP_MAXGROUP(dao) = ival
	case CLIPEXP:
	    DP_CLIPEXP(dao) = ival
	case RECENTER:
	    DP_RECENTER(dao) = ival
	case VARPSF:
	    DP_VARPSF(dao) = ival
	default:
	    call error (0, "DP_SETI: Unknown integer daophot parameter")
	}
end


# DP_SETR -- Procedure to set a real daophot parameter.

procedure dp_setr (dao, param, rval)

pointer	dao		# pointer to daophot structure
int	param		# parameter
real	rval		# real value

begin
	switch (param) {
	case SCALE:
	    DP_SCALE(dao) = rval
	case MAXGDATA:
	    DP_MAXGDATA(dao) = rval
	case MINGDATA:
	    DP_MINGDATA(dao) = rval
	case READ_NOISE:
	    DP_READ_NOISE(dao) = rval
	case PHOT_ADC:
	    DP_PHOT_ADC(dao) = rval
	case RPSFRAD:
	    DP_RPSFRAD(dao) = rval
	case SPSFRAD:
	    DP_SPSFRAD(dao) = rval
	case PSFRAD:
	    DP_PSFRAD(dao) = rval
	case SFITRAD:
	    DP_SFITRAD(dao) = rval
	case FITRAD:
	    DP_FITRAD(dao) = rval
	case SMATCHRAD:
	    DP_SMATCHRAD(dao) = rval
	case MATCHRAD:
	    DP_MATCHRAD(dao) = rval
	case CRITOVLAP:
	    DP_CRITOVLAP(dao) = rval
	case CLIPRANGE:
	    DP_CLIPRANGE(dao) = rval
	case XAIRMASS:
	    DP_XAIRMASS(dao) = rval
	case ITIME:
	    DP_ITIME(dao) = rval
	default:
	    call error (0, "DP_SETR: Unknown real daophot parameter")
	}
end
