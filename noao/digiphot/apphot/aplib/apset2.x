include "../lib/apphotdef.h"
include "../lib/apphot.h"
include "../lib/displaydef.h"
include "../lib/display.h"
include "../lib/noisedef.h"
include "../lib/noise.h"
include "../lib/polyphotdef.h"
include "../lib/polyphot.h"
include "../lib/radprofdef.h"
include "../lib/radprof.h"
include "../lib/finddef.h"
include "../lib/find.h"

# AP2SETS -- Procedure to set an apphot string parameter.

procedure ap2sets (ap, param, str)

pointer	ap		# pointer to apphot structure
int	param		# parameter
char	str[ARB]	# string parameter

pointer	nse, ply

begin
	nse = AP_NOISE(ap)
	ply = AP_POLY(ap)

	switch (param) {
	case PYNAME:
	    call strcpy (str, AP_PYNAME(ply), SZ_FNAME)
	case PYROOT:
	    call strcpy (str, AP_PYROOT(ply), SZ_FNAME)
	case GAIN:
	    call strcpy (str, AP_GAIN(nse), SZ_FNAME)
	case NSTRING:
	    call strcpy (str, AP_NSTRING(nse), SZ_FNAME)
	case CCDREAD:
	    call strcpy (str, AP_CCDREAD(nse), SZ_FNAME)
	default:
	    call error (0, "APSETS: Unknown apphot string parameter")
	}
end


# AP2SETI -- Procedure to set an integer apphot parameter.

procedure ap2seti (ap, param, ival)

pointer	ap		# pointer to apphot structure
int	param		# parameter
int	ival		# integer value

pointer	dsp, nse, ply, rprof

begin
	nse = AP_NOISE(ap)
	dsp = AP_PDISPLAY(ap)
	ply = AP_POLY(ap)
	rprof = AP_RPROF(ap)

	switch (param) {
	case RPORDER:
	    AP_RPORDER(rprof) = ival
	case RPNREJECT:
	    AP_RPNREJECT(rprof) = ival
	case PYNVER:
	    AP_PYNVER(ply) = ival
	case PYBADPIX:
	    AP_PYBADPIX(ply) = ival
	case MKSKY:
	    AP_MKSKY(dsp) = ival
	case MKCENTER:
	    AP_MKCENTER(dsp) = ival
	case MKAPERT:
	    AP_MKAPERT(dsp) = ival
	case MKPOLYGON:
	    AP_MKPOLYGON(dsp) = ival
	case MKDETECTIONS:
	    AP_MKDETECTIONS(dsp) = ival
	case NOISEFUNCTION:
	    AP_NOISEFUNCTION(nse) = ival
	case MKPSFBOX:
	    AP_MKPSFBOX(dsp) = ival
	case RADPLOTS:
	    AP_RADPLOTS(dsp) = ival
	case RPNPTS:
	    AP_RPNPTS(rprof) = ival
	case RPNDATA:
	    AP_RPNDATA(rprof) = ival
	case RPNDATAREJ:
	    AP_RPNDATAREJ(rprof) = ival
	default:
	    call error (0, "APSETI: Unknown apphot integer parameter")
	}
end


# AP2SETR -- Procedure to set a real apphot parameter.

procedure ap2setr (ap, param, rval)

pointer	ap		# pointer to apphot structure
int	param		# parameter
real	rval		# real value

pointer	nse, ply, rprof, fnd

begin
	nse = AP_NOISE(ap)
	ply = AP_POLY(ap)
	rprof = AP_RPROF(ap)
	fnd = AP_PFIND(ap)

	switch (param) {
	case RPFWHM:
	    AP_RPFWHM(rprof) = rval
	case INORM:
	    AP_INORM(rprof) = rval
	case TNORM:
	    AP_TINORM(rprof) = rval
	case DNORM:
	    AP_DNORM(rprof) = rval
	case RPXCUR:
	    AP_RPXCUR(rprof) = rval
	case RPYCUR:
	    AP_RPYCUR(rprof) = rval
	case ORPXCUR:
	    AP_ORPXCUR(rprof) = rval
	case ORPYCUR:
	    AP_ORPYCUR(rprof) = rval
	case RPRADIUS:
	    AP_RPRADIUS(rprof) = rval
	    AP_RPNPTS(rprof) = int (AP_RPRADIUS(rprof) / AP_RPSTEP(rprof)) + 1
	    call realloc (AP_RPDIST(rprof), AP_RPNPTS(rprof), TY_REAL)
	    call realloc (AP_INTENSITY(rprof), AP_RPNPTS(rprof), TY_REAL)
	    call realloc (AP_DINTENSITY(rprof), AP_RPNPTS(rprof), TY_REAL)
	    call realloc (AP_TINTENSITY(rprof), AP_RPNPTS(rprof), TY_REAL)
	case RPSTEP:
	    AP_RPSTEP(rprof) = rval
	    AP_RPNPTS(rprof) = int (AP_RPRADIUS(rprof) / AP_RPSTEP(rprof)) + 1
	    call realloc (AP_RPDIST(rprof), AP_RPNPTS(rprof), TY_REAL)
	    call realloc (AP_INTENSITY(rprof), AP_RPNPTS(rprof), TY_REAL)
	    call realloc (AP_DINTENSITY(rprof), AP_RPNPTS(rprof), TY_REAL)
	    call realloc (AP_TINTENSITY(rprof), AP_RPNPTS(rprof), TY_REAL)
	case RPKSIGMA:
	    AP_RPKSIGMA(rprof) = rval
	case PYZMAG:
	    AP_PYZMAG(ply) = rval
	case PYMAG:
	    AP_PYMAG(ply) = rval
	case PYMAGERR:
	    AP_PYMAGERR(ply) = rval
	case PYX:
	    AP_PYX(ply) = rval
	case PYY:
	    AP_PYY(ply) = rval
	case PYMINRAD:
	    AP_PYMINRAD(ply) = rval
	case PYCX:
	    AP_PYCX(ply) = rval
	case PYCY:
	    AP_PYCY(ply) = rval
	case OPYCX:
	    AP_OPYCX(ply) = rval
	case OPYCY:
	    AP_OPYCY(ply) = rval
	case PYXMEAN:
	    AP_PYXMEAN(ply) = rval
	case PYYMEAN:
	    AP_PYYMEAN(ply) = rval
	case OPYXMEAN:
	    AP_OPYXMEAN(ply) = rval
	case OPYYMEAN:
	    AP_OPYYMEAN(ply) = rval
	case SKYSIGMA:
	    AP_SKYSIGMA(nse) = rval
	case EPADU:
	    AP_EPADU(nse) = rval
	case READNOISE:
	    AP_READNOISE(nse) = rval
	case THRESHOLD:
	    AP_THRESHOLD(fnd) = rval
	case RATIO:
	    AP_RATIO(fnd) = rval
	case THETA:
	    AP_THETA(fnd) = rval
	case NSIGMA:
	    AP_NSIGMA(fnd) = rval
	case SHARPLO:
	    AP_SHARPLO(fnd) = rval
	case SHARPHI:
	    AP_SHARPHI(fnd) = rval
	case ROUNDLO:
	    AP_ROUNDLO(fnd) = rval
	case ROUNDHI:
	    AP_ROUNDHI(fnd) = rval
	default:
	    call error (0, "APSETR: Unknown apphot real parameter")
	}
end


# AP2SETD -- Procedure to set a double apphot parameter.

procedure ap2setd (ap, param, dval)

pointer	ap		# pointer to apphot structure
int	param		# parameter
double	dval		# double value

pointer	nse, ply, rprof, fnd

begin
	nse = AP_NOISE(ap)
	ply = AP_POLY(ap)
	rprof = AP_RPROF(ap)
	fnd = AP_PFIND(ap)

	switch (param) {
	case PYNPIX:
	    AP_PYNPIX(ply) = dval
	case PYFLUX:
	    AP_PYFLUX(ply) = dval
	default:
	    call error (0, "APSETD: Unknown apphot double parameter")
	}
end
