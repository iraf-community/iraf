include "../lib/apphotdef.h"
include "../lib/apphot.h"
include "../lib/centerdef.h"
include "../lib/center.h"
include "../lib/fitskydef.h"
include "../lib/fitsky.h"
include "../lib/photdef.h"
include "../lib/phot.h"
include "../lib/fitpsfdef.h"
include "../lib/fitpsf.h"

# AP1SETS -- Procedure to set an apphot string parameter.

procedure ap1sets (ap, param, str)

pointer	ap		# pointer to apphot structure
int	param		# parameter
char	str[ARB]	# string parameter

int	naperts
pointer	ctr, sky, phot, psf
real	aperts[MAX_NAPERTS]
int	ap_getaperts()

begin
	ctr = AP_PCENTER(ap)
	sky = AP_PSKY(ap)
	phot = AP_PPHOT(ap)
	psf = AP_PPSF(ap)

	switch (param) {
	case IMNAME:
	    call strcpy (str, AP_IMNAME(ap), SZ_FNAME)
	case IMROOT:
	    call strcpy (str, AP_IMROOT(ap), SZ_FNAME)
	case CLNAME:
	    call strcpy (str, AP_CLNAME(ap), SZ_FNAME)
	case CLROOT:
	    call strcpy (str, AP_CLROOT(ap), SZ_FNAME)
	case PLOTFILE:
	    call strcpy (str, AP_PLOTFILE(ap), SZ_FNAME)
	case OUTNAME:
	    call strcpy (str, AP_OUTNAME(ap), SZ_FNAME)
	case EXPOSURE:
	    call strcpy (str, AP_EXPOSURE(ap), SZ_FNAME)
	case AIRMASS:
	    call strcpy (str, AP_AIRMASS(ap), SZ_FNAME)
	case FILTER:
	    call strcpy (str, AP_FILTER(ap), SZ_FNAME)
	case FILTERID:
	    call strcpy (str, AP_FILTERID(ap), SZ_FNAME)
	case OBSTIME:
	    call strcpy (str, AP_OBSTIME(ap), SZ_FNAME)
	case OTIME:
	    call strcpy (str, AP_OTIME(ap), SZ_FNAME)

	case CSTRING:
	    call strcpy (str, AP_CSTRING(ctr), SZ_FNAME)

	case SSTRING:
	    call strcpy (str, AP_SSTRING(sky), SZ_FNAME)

	case APSTRING:
	    call strcpy (str, AP_APSTRING(phot), SZ_FNAME)
	case APERTS:
	    naperts = ap_getaperts (str, aperts, MAX_NAPERTS)
	    if (naperts > 0) {
		call strcpy (str, AP_APSTRING(phot), SZ_LINE)
		AP_NAPERTS(phot) = naperts
	        call realloc (AP_APERTS(phot), AP_NAPERTS(phot), TY_REAL)
	        call realloc (AP_AREA(phot), AP_NAPERTS(phot), TY_DOUBLE)
	        call realloc (AP_SUMS(phot), AP_NAPERTS(phot), TY_DOUBLE)
	        call realloc (AP_MAGS(phot), AP_NAPERTS(phot), TY_REAL)
	        call realloc (AP_MAGERRS(phot), AP_NAPERTS(phot), TY_REAL)
	        call amovr (aperts, Memr[AP_APERTS(phot)], AP_NAPERTS(phot))
	        call asrtr (Memr[AP_APERTS(phot)], Memr[AP_APERTS(phot)],
	            AP_NAPERTS(phot))
	    }
	case PWSTRING:
	    call strcpy (str, AP_PWSTRING(phot), SZ_FNAME)

	case PSFSTRING:
	    call strcpy (str, AP_PSFSTRING(psf), SZ_FNAME)

	default:
	    call error (0, "APSETS: Unknown apphot string parameter")
	}
end


# AP1SETI -- Procedure to set an integer apphot parameter.

procedure ap1seti (ap, param, ival)

pointer	ap		# pointer to apphot structure
int	param		# parameter
int	ival		# integer value

pointer	cen, sky, phot, psf

begin
	cen = AP_PCENTER(ap)
	sky = AP_PSKY(ap)
	phot = AP_PPHOT(ap)
	psf = AP_PPSF(ap)

	switch (param) {
	case POSITIVE:
	    AP_POSITIVE(ap) = ival
	case WCSIN:
	    AP_WCSIN(ap) = ival
	case WCSOUT:
	    AP_WCSOUT(ap) = ival
	case MW:
	    AP_MW(ap) = ival
	case CTIN:
	    AP_CTIN(ap) = ival
	case CTOUT:
	    AP_CTOUT(ap) = ival

	case CENTERFUNCTION:
	    AP_CENTERFUNCTION(cen) = ival
	case CLEAN:
	    AP_CLEAN(cen) = ival
	case CMAXITER:
	    AP_CMAXITER(cen) = ival

	case SKYFUNCTION:
	    AP_SKYFUNCTION(sky) = ival
	case SMAXITER:
	    AP_SMAXITER(sky) = ival
	case SNREJECT:
	    AP_SNREJECT(sky) = ival
	case SMOOTH:
	    AP_SMOOTH(sky) = ival
	case NSKY:
	    AP_NSKY(sky) = ival
	case NSKY_REJECT:
	    AP_NSKY_REJECT(sky) = ival

	case PWEIGHTS:
	    AP_PWEIGHTS(phot) = ival

	case PSFUNCTION:
	    AP_PSFUNCTION(psf) = ival
	case NPARS:
	    AP_PSFNPARS(psf) = ival
	case MAXNPARS:
	    AP_MAXNPARS(psf) = ival
	case PMAXITER:
	    AP_PMAXITER(psf) = ival
	case PNREJECT:
	    AP_PNREJECT(psf) = ival

	default:
	    call error (0, "APSETI: Unknown apphot integer parameter")
	}
end


# AP1SETR -- Procedure to set a real apphot parameter.

procedure ap1setr (ap, param, rval)

pointer	ap		# pointer to apphot structure
int	param		# parameter
real	rval		# real value

pointer	cen, sky, phot, psf

begin
	cen = AP_PCENTER(ap)
	sky = AP_PSKY(ap)
	phot = AP_PPHOT(ap)
	psf = AP_PPSF(ap)

	switch (param) {

	case FWHMPSF:
	    AP_FWHMPSF(ap) = rval
	case SCALE:
	    AP_SCALE(ap) =  rval
	case WX:
	    AP_WX(ap) = rval
	case WY:
	    AP_WY(ap) = rval
	case ITIME:
	    AP_ITIME(ap) = rval
	case CWX:
	    AP_CWX(ap) = rval
	case CWY:
	    AP_CWY(ap) = rval
	case DATAMIN:
	    AP_DATAMIN(ap) = rval
	case DATAMAX:
	    AP_DATAMAX(ap) = rval
	case XAIRMASS:
	    AP_XAIRMASS(ap) = rval

	case CDATALIMIT:
	    AP_CDATALIMIT(cen) = rval
	case XSHIFT:
	    AP_XSHIFT(cen) = rval
	case YSHIFT:
	    AP_YSHIFT(cen) = rval
	case OXSHIFT:
	    AP_OXSHIFT(cen) = rval
	case OYSHIFT:
	    AP_OYSHIFT(cen) = rval
	case CXCUR:
	    AP_CXCUR(cen) = rval
	case CYCUR:
	    AP_CYCUR(cen) = rval
	case CAPERT:
	    AP_CAPERT(cen) = rval
	case CTHRESHOLD:
	    AP_CTHRESHOLD(cen) = rval
	case MAXSHIFT:
	    AP_MAXSHIFT(cen) = rval
	case MINSNRATIO:
	    AP_MINSNRATIO(cen) = rval
	case RCLEAN:
	    AP_RCLEAN(cen) = rval
	case RCLIP:
	    AP_RCLIP(cen) = rval
	case SIGMACLEAN:
	    AP_SIGMACLEAN(cen) = rval
	case OXINIT:
	    AP_OXINIT(cen) = rval
	case OYINIT:
	    AP_OYINIT(cen) = rval
	case XCENTER:
	    AP_XCENTER(cen) = rval
	case YCENTER:
	    AP_YCENTER(cen) = rval
	case OXCENTER:
	    AP_OXCENTER(cen) = rval
	case OYCENTER:
	    AP_OYCENTER(cen) = rval
	case XERR:
	    AP_XERR(cen) = rval
	case YERR:
	    AP_YERR(cen) = rval

	case ANNULUS:
	    AP_ANNULUS(sky) = rval
	case DANNULUS:
	    AP_DANNULUS(sky) = rval
	case SXCUR:
	    AP_SXCUR(sky) = rval
	case SYCUR:
	    AP_SYCUR(sky) = rval
	case OSXCUR:
	    AP_OSXCUR(sky) = rval
	case OSYCUR:
	    AP_OSYCUR(sky) = rval
	case K1:
	    AP_K1(sky) = rval
	case SLOREJECT:
	    AP_SLOREJECT(sky) = rval
	case SHIREJECT:
	    AP_SHIREJECT(sky) = rval
	case SLOCLIP:
	    AP_SLOCLIP(sky) = rval
	case SHICLIP:
	    AP_SHICLIP(sky) = rval
	case BINSIZE:
	    AP_BINSIZE(sky) = rval
	case RGROW:
	    AP_RGROW(sky) = rval
	case SKY_BACKGROUND:
	    AP_SKYBACKGROUND(sky) = rval
	case SKY_MODE:
	    AP_SKY_MODE(sky) = rval
	case SKY_SIGMA:
	    AP_SKY_SIG(sky) = rval
	case SKY_SKEW:
	    AP_SKY_SKEW(sky) = rval

	case PXCUR:
	    AP_PXCUR(phot) = rval
	case PYCUR:
	    AP_PYCUR(phot) = rval
	case OPXCUR:
	    AP_OPXCUR(phot) = rval
	case OPYCUR:
	    AP_OPYCUR(phot) = rval
	case ZMAG:
	    AP_ZMAG(phot) = rval

	case PK2:
	    AP_PK2(psf) = rval
	case PSFAPERT:
	    AP_PSFAPERT(psf) = rval
	case PFXCUR:
	    AP_PFXCUR(psf) = rval
	case PFYCUR:
	    AP_PFYCUR(psf) = rval
	case OPFXCUR:
	    AP_OPFXCUR(psf) = rval
	case OPFYCUR:
	    AP_OPFYCUR(psf) = rval

	default:
	    call error (0, "APSETR: Unknown apphot real parameter")
	}
end


# AP1SETD -- Procedure to set a double apphot parameter.

procedure ap1setd (ap, param, dval)

pointer	ap		# pointer to apphot structure
int	param		# parameter
double	dval		# double value

pointer	cen, sky, phot, psf

begin
	cen = AP_PCENTER(ap)
	sky = AP_PSKY(ap)
	phot = AP_PPHOT(ap)
	psf = AP_PPSF(ap)

	switch (param) {
	default:
	    call error (0, "APSETD: Unknown apphot double parameter")
	}
end
