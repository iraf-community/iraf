include	"../lib/daophotdef.h"
include "../lib/apseldef.h"

# DP_SETS -- Set a daophot string parameter.

procedure dp_sets (dao, param, str)

pointer	dao		# pointer to daophot structure
int	param		# parameter
char	str[ARB]	# string value

begin
	switch (param) {
	case INIMAGE:
	    call strcpy (str, DP_INIMAGE(dao), SZ_FNAME)
	case INPHOTFILE:
	    call strcpy (str, DP_INPHOTFILE(dao), SZ_FNAME)
	case COORDS:
	    call strcpy (str, DP_COORDS(dao), SZ_FNAME)
	case PSFIMAGE:
	    call strcpy (str, DP_PSFIMAGE(dao), SZ_FNAME)
	case OUTPHOTFILE:
	    call strcpy (str, DP_OUTPHOTFILE(dao), SZ_FNAME)
	case OUTREJFILE:
	    call strcpy (str, DP_OUTREJFILE(dao), SZ_FNAME)
	case OUTIMAGE:
	    call strcpy (str, DP_OUTIMAGE(dao), SZ_FNAME)
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
	case FUNCTION:
	    call strcpy (str, DP_FUNCTION(dao), SZ_FNAME)
	case FUNCLIST:
	    call strcpy (str, DP_FUNCLIST(dao), SZ_FNAME)
	default:
	    call error (0, "DP_SETS: Unknown daophot string parameter")
	}
end


# DP_SETI -- Set a daophot integer parameter.

procedure dp_seti (dao, param, ival)

pointer	dao		# pointer to daophot structure
int	param		# parameter
int	ival		# integer value

pointer	apsel

begin
	apsel = DP_APSEL(dao)

	switch (param) {
	case MW:
	    DP_MW(dao) = ival
	case WCSIN:
	    DP_WCSIN(dao) = ival
	case WCSOUT:
	    DP_WCSOUT(dao) = ival
	case WCSPSF:
	    DP_WCSPSF(dao) = ival
	case CTIN:
	    DP_CTIN(dao) = ival
	case CTOUT:
	    DP_CTOUT(dao) = ival
	case CTPSF:
	    DP_CTPSF(dao) = ival
	case MAXITER:
	    DP_MAXITER(dao) = ival
	case VERBOSE:
	    DP_VERBOSE(dao) = ival
	case TEXT:
	    DP_TEXT(dao) = ival
	case MAXNSTAR:
	    DP_MAXNSTAR(dao) = ival
	case MAXGROUP:
	    DP_MAXGROUP(dao) = ival
	case CLIPEXP:
	    DP_CLIPEXP(dao) = ival
	case RECENTER:
	    DP_RECENTER(dao) = ival
	case FITSKY:
	    DP_FITSKY(dao) = ival
	case GROUPSKY:
	    DP_GROUPSKY(dao) = ival
	case VARORDER:
	    DP_VARORDER(dao) = ival
	case FEXPAND:
	    DP_FEXPAND(dao) = ival
	case SATURATED:
	    DP_SATURATED(dao) = ival
	case NCLEAN:
	    DP_NCLEAN(dao) = ival
	case APNUM:
	    DP_APNUM(apsel) = ival
	default:
	    call error (0, "DP_SETI: Unknown integer daophot parameter")
	}
end


# DP_SETR -- Set a real daophot parameter.

procedure dp_setr (dao, param, rval)

pointer	dao		# pointer to daophot structure
int	param		# parameter
real	rval		# real value

begin
	switch (param) {
	case SCALE:
	    DP_SCALE(dao) = rval
	case SFWHMPSF:
	    DP_SFWHMPSF(dao) = rval
	case FWHMPSF:
	    DP_FWHMPSF(dao) = rval
	case MAXGDATA:
	    DP_MAXGDATA(dao) = rval
	case MINGDATA:
	    DP_MINGDATA(dao) = rval
	case READNOISE:
	    DP_READNOISE(dao) = rval
	case PHOTADU:
	    DP_PHOTADU(dao) = rval
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
	case SANNULUS:
	    DP_SANNULUS(dao) = rval
	case ANNULUS:
	    DP_ANNULUS(dao) = rval
	case SDANNULUS:
	    DP_SDANNULUS(dao) = rval
	case DANNULUS:
	    DP_DANNULUS(dao) = rval
	case CRITSNRATIO:
	    DP_CRITSNRATIO(dao) = rval
	case CLIPRANGE:
	    DP_CLIPRANGE(dao) = rval
	case XAIRMASS:
	    DP_XAIRMASS(dao) = rval
	case ITIME:
	    DP_ITIME(dao) = rval
	case FLATERR:
	    DP_FLATERR(dao) = rval
	case PROFERR:
	    DP_PROFERR(dao) = rval
	case SMERGERAD:
	    DP_SMERGERAD(dao) = rval
	case MERGERAD:
	    DP_MERGERAD(dao) = rval
	default:
	    call error (0, "DP_SETR: Unknown real daophot parameter")
	}
end
