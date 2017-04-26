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

# AP2STATS -- Procedure to fetch an apphot string parameter.

procedure ap2stats (ap, param, str, maxch)

pointer	ap		# pointer to apphot structure
int	param		# parameter
char	str[ARB]	# string
int	maxch		# maximum number of characters

pointer	 nse, ply

begin
	nse = AP_NOISE(ap)
	ply = AP_POLY(ap)

	switch (param) {
	case PYNAME:
	    call strcpy (AP_PYNAME(ply), str, maxch)
	case PYROOT:
	    call strcpy (AP_PYROOT(ply), str, maxch)
	case GAIN:
	    call strcpy (AP_GAIN(nse), str, maxch)
	case NSTRING:
	    call strcpy (AP_NSTRING(nse), str, maxch)
	case CCDREAD:
	    call strcpy (AP_CCDREAD(nse), str, maxch)
	default:
	    call error (0, "APSTATS: Unknown apphot string parameter")
	}
end


# AP2STATI -- Procedure to set an integer apphot parameter.

int procedure ap2stati (ap, param)

pointer	ap		# pointer to apphot structure
int	param		# parameter

pointer dsp, nse, ply, rprof

begin
	dsp = AP_PDISPLAY(ap)
	nse = AP_NOISE(ap)
	ply = AP_POLY(ap)
	rprof = AP_RPROF(ap)

	switch (param) {
	case RPORDER:
	    return (AP_RPORDER(rprof))
	case RPNREJECT:
	    return (AP_RPNREJECT(rprof))
	case PYNVER:
	    return (AP_PYNVER(ply))
	case PYBADPIX:
	    return (AP_PYBADPIX(ply))
	case MKSKY:
	    return (AP_MKSKY(dsp))
	case MKCENTER:
	    return (AP_MKCENTER(dsp))
	case MKAPERT:
	    return (AP_MKAPERT(dsp))
	case MKPOLYGON:
	    return (AP_MKPOLYGON(dsp))
	case MKDETECTIONS:
	    return (AP_MKDETECTIONS(dsp))
	case NOISEFUNCTION:
	    return (AP_NOISEFUNCTION(nse))
	case MKPSFBOX:
	    return (AP_MKPSFBOX(dsp))
	case RADPLOTS:
	    return (AP_RADPLOTS(dsp))
	case RPNPTS:
	    return (AP_RPNPTS(rprof))
	case RPNDATA:
	    return (AP_RPNDATA(rprof))
	case RPNDATAREJ:
	    return (AP_RPNDATAREJ(rprof))
	default:
	    call error (0, "APSTATI: Unknown apphot integer parameter")
	}
end


# AP2STATR -- Procedure to set a real apphot parameter.

real procedure ap2statr (ap, param)

pointer	ap		# pointer to apphot structure
int	param		# parameter

pointer	nse, ply, rprof, fnd

begin
	nse = AP_NOISE(ap)
	ply = AP_POLY(ap)
	rprof = AP_RPROF(ap)
	fnd = AP_PFIND(ap)

	switch (param) {
	case RPFWHM:
	    return (AP_RPFWHM(rprof))
	case INORM:
	    return (AP_INORM(rprof))
	case TNORM:
	    return (AP_TINORM(rprof))
	case DNORM:
	    return (AP_DNORM(rprof))
	case RPXCUR:
	    return (AP_RPXCUR(rprof))
	case RPYCUR:
	    return (AP_RPYCUR(rprof))
	case ORPXCUR:
	    return (AP_ORPXCUR(rprof))
	case ORPYCUR:
	    return (AP_ORPYCUR(rprof))
	case RPRADIUS:
	    return (AP_RPRADIUS(rprof))
	case RPSTEP:
	    return (AP_RPSTEP(rprof))
	case RPKSIGMA:
	    return (AP_RPKSIGMA(rprof))
	case PYZMAG:
	    return (AP_PYZMAG(ply))
	case PYMAG:
	    return (AP_PYMAG(ply))
	case PYMAGERR:
	    return (AP_PYMAGERR(ply))
	case PYX:
	    return (AP_PYX(ply))
	case PYY:
	    return (AP_PYY(ply))
	case PYMINRAD:
	    return (AP_PYMINRAD(ply))
	case PYXMEAN:
	    return (AP_PYXMEAN(ply))
	case PYYMEAN:
	    return (AP_PYYMEAN(ply))
	case OPYXMEAN:
	    return (AP_OPYXMEAN(ply))
	case OPYYMEAN:
	    return (AP_OPYYMEAN(ply))
	case PYCX:
	    return (AP_PYCX(ply))
	case PYCY:
	    return (AP_PYCY(ply))
	case OPYCX:
	    return (AP_OPYCX(ply))
	case OPYCY:
	    return (AP_OPYCY(ply))
	case SKYSIGMA:
	    return (AP_SKYSIGMA(nse))
	case EPADU:
	    return (AP_EPADU(nse))
	case READNOISE:
	    return (AP_READNOISE(nse))
	case THRESHOLD:
	    return (AP_THRESHOLD(fnd))
	case RATIO:
	    return (AP_RATIO(fnd))
	case THETA:
	    return (AP_THETA(fnd))
	case NSIGMA:
	    return (AP_NSIGMA(fnd))
	case SHARPLO:
	    return (AP_SHARPLO(fnd))
	case SHARPHI:
	    return (AP_SHARPHI(fnd))
	case ROUNDLO:
	    return (AP_ROUNDLO(fnd))
	case ROUNDHI:
	    return (AP_ROUNDHI(fnd))
	default:
	    call error (0, "APSTATR: Unknown apphot real parameter")
	}
end


# AP2STATD -- Procedure to set a double apphot parameter.

double procedure ap2statd (ap, param)

pointer	ap		# pointer to apphot structure
int	param		# parameter

pointer	nse, ply, rprof, fnd

begin
	nse = AP_NOISE(ap)
	ply = AP_POLY(ap)
	rprof = AP_RPROF(ap)
	fnd = AP_PFIND(ap)

	switch (param) {
	case PYNPIX:
	    return (AP_PYNPIX(ply))
	case PYFLUX:
	    return (AP_PYFLUX(ply))
	default:
	    call error (0, "APSTATD: Unknown apphot double parameter")
	}
end
