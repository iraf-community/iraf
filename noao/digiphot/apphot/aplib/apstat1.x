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

# AP1STATS -- Procedure to fetch an apphot string parameter.

procedure ap1stats (ap, param, str, maxch)

pointer	ap		# pointer to apphot structure
int	param		# parameter
char	str[ARB]	# string
int	maxch		# maximum number of characters

pointer	cen, sky, phot, psf

begin
	cen = AP_PCENTER(ap)
	sky = AP_PSKY(ap)
	phot = AP_PPHOT(ap)
	psf = AP_PPSF(ap)

	switch (param) {
	case IMNAME:
	    call strcpy (AP_IMNAME(ap), str, maxch)
	case IMROOT:
	    call strcpy (AP_IMROOT(ap), str, maxch)
	case CLNAME:
	    call strcpy (AP_CLNAME(ap), str, maxch)
	case CLROOT:
	    call strcpy (AP_CLROOT(ap), str, maxch)
	case PLOTFILE:
	    call strcpy (AP_PLOTFILE(ap), str, maxch)
	case OUTNAME:
	    call strcpy (AP_OUTNAME(ap), str, maxch)
	case EXPOSURE:
	    call strcpy (AP_EXPOSURE(ap), str, maxch)
	case AIRMASS:
	    call strcpy (AP_AIRMASS(ap), str, maxch)
	case FILTER:
	    call strcpy (AP_FILTER(ap), str, maxch)
	case FILTERID:
	    call strcpy (AP_FILTERID(ap), str, maxch)
	case OBSTIME:
	    call strcpy (AP_OBSTIME(ap), str, maxch)
	case OTIME:
	    call strcpy (AP_OTIME(ap), str, maxch)

	case CSTRING:
	    call strcpy (AP_CSTRING(cen), str, maxch)

	case SSTRING:
	    call strcpy (AP_SSTRING(sky), str, maxch)

	case APSTRING:
	    call strcpy (AP_APSTRING(phot), str, maxch)
	case APERTS:
	    call strcpy (AP_APSTRING(phot), str, maxch)
	case PWSTRING:
	    call strcpy (AP_PWSTRING(phot), str, maxch)

	case PSFSTRING:
	    call strcpy (AP_PSFSTRING(psf), str, maxch)

	default:
	    call error (0, "APSTATS: Unknown apphot string parameter")
	}
end


# AP1STATI -- Procedure to set an integer apphot parameter.

int procedure ap1stati (ap, param)

pointer	ap		# pointer to apphot structure
int	param		# parameter

pointer cen, sky, phot, psf

begin
	cen = AP_PCENTER(ap)
	sky = AP_PSKY(ap)
	phot = AP_PPHOT(ap)
	psf = AP_PPSF(ap)

	switch (param) {
	case POSITIVE:
	    return (AP_POSITIVE(ap))
	case WCSIN:
	    return (AP_WCSIN(ap))
	case WCSOUT:
	    return (AP_WCSOUT(ap))
	case MW:
	    return (AP_MW(ap))
	case CTIN:
	    return (AP_CTIN(ap))
	case CTOUT:
	    return (AP_CTOUT(ap))

	case CENTERFUNCTION:
	    return (AP_CENTERFUNCTION(cen))
	case CLEAN:
	    return (AP_CLEAN(cen))
	case CMAXITER:
	    return (AP_CMAXITER(cen))

	case SKYFUNCTION:
	    return (AP_SKYFUNCTION(sky))
	case SMAXITER:
	    return (AP_SMAXITER(sky))
	case SNREJECT:
	    return (AP_SNREJECT(sky))
	case SMOOTH:
	    return (AP_SMOOTH(sky))
	case NSKY:
	    return (AP_NSKY(sky))
	case NSKY_REJECT:
	    return (AP_NSKY_REJECT(sky))

	case NAPERTS:
	    return (AP_NAPERTS(phot))
	case PWEIGHTS:
	    return (AP_PWEIGHTS(phot))

	case MAXNPARS:
	    return (AP_MAXNPARS(psf))
	case NPARS:
	    return (AP_PSFNPARS(psf))
	case PMAXITER:
	    return (AP_PMAXITER(psf))
	case PSFUNCTION:
	    return (AP_PSFUNCTION(psf))
	case PNREJECT:
	    return (AP_PNREJECT(psf))

	default:
	    call error (0, "APSTATI: Unknown apphot integer parameter")
	}
end


# AP1STATR -- Procedure to set a real apphot parameter.

real procedure ap1statr (ap, param)

pointer	ap		# pointer to apphot structure
int	param		# parameter

pointer	cen, sky, phot, psf

begin
	cen = AP_PCENTER(ap)
	sky = AP_PSKY(ap)
	phot = AP_PPHOT(ap)
	psf = AP_PPSF(ap)

	switch (param) {

	case FWHMPSF:
	    return (AP_FWHMPSF(ap))
	case SCALE:
	    return (AP_SCALE(ap))
	case WX:
	    return (AP_WX(ap))
	case WY:
	    return (AP_WY(ap))
	case ITIME:
	    return (AP_ITIME(ap))
	case CWX:
	    return (AP_CWX(ap))
	case CWY:
	    return (AP_CWY(ap))
	case DATAMIN:
	    return (AP_DATAMIN(ap))
	case DATAMAX:
	    return (AP_DATAMAX(ap))
	case XAIRMASS:
	    return (AP_XAIRMASS(ap))

	case CDATALIMIT:
	    return (AP_CDATALIMIT(cen))
	case XSHIFT:
	    return (AP_XSHIFT(cen))
	case YSHIFT:
	    return (AP_YSHIFT(cen))
	case OXSHIFT:
	    return (AP_OXSHIFT(cen))
	case OYSHIFT:
	    return (AP_OYSHIFT(cen))
	case CXCUR:
	    return (AP_CXCUR(cen))
	case CYCUR:
	    return (AP_CYCUR(cen))
	case CAPERT:
	    return (AP_CAPERT(cen))
	case CTHRESHOLD:
	    return (AP_CTHRESHOLD(cen))
	case MAXSHIFT:
	    return (AP_MAXSHIFT(cen))
	case MINSNRATIO:
	    return (AP_MINSNRATIO(cen))
	case RCLEAN:
	    return (AP_RCLEAN(cen))
	case RCLIP:
	    return (AP_RCLIP(cen))
	case SIGMACLEAN:
	    return (AP_SIGMACLEAN(cen))

	case OXINIT:
	    return (AP_OXINIT(cen))
	case OYINIT:
	    return (AP_OYINIT(cen))
	case XCENTER:
	    return (AP_XCENTER(cen))
	case YCENTER:
	    return (AP_YCENTER(cen))
	case OXCENTER:
	    return (AP_OXCENTER(cen))
	case OYCENTER:
	    return (AP_OYCENTER(cen))
	case XERR:
	    return (AP_XERR(cen))
	case YERR:
	    return (AP_YERR(cen))

	case SXCUR:
	    return (AP_SXCUR(sky))
	case SYCUR:
	    return (AP_SYCUR(sky))
	case OSXCUR:
	    return (AP_OSXCUR(sky))
	case OSYCUR:
	    return (AP_OSYCUR(sky))
	case ANNULUS:
	    return (AP_ANNULUS(sky))
	case DANNULUS:
	    return (AP_DANNULUS(sky))
	case K1:
	    return (AP_K1(sky))
	case SLOREJECT:
	    return (AP_SLOREJECT(sky))
	case SHIREJECT:
	    return (AP_SHIREJECT(sky))
	case SLOCLIP:
	    return (AP_SLOCLIP(sky))
	case SHICLIP:
	    return (AP_SHICLIP(sky))
	case BINSIZE:
	    return (AP_BINSIZE(sky))
	case RGROW:
	    return (AP_RGROW(sky))
	case SKY_BACKGROUND:
	    return (AP_SKYBACKGROUND(sky))
	case SKY_MODE:
	    return (AP_SKY_MODE(sky))
	case SKY_SIGMA:
	    return (AP_SKY_SIG(sky))
	case SKY_SKEW:
	    return (AP_SKY_SKEW(sky))

	case PXCUR:
	    return (AP_PXCUR(phot))
	case PYCUR:
	    return (AP_PYCUR(phot))
	case OPXCUR:
	    return (AP_OPXCUR(phot))
	case OPYCUR:
	    return (AP_OPYCUR(phot))
	case ZMAG:
	    return (AP_ZMAG(phot))

	case PSFAPERT:
	    return (AP_PSFAPERT(psf))
	case PK2:
	    return (AP_PK2(psf))
	case PFXCUR:
	    return (AP_PFXCUR(psf))
	case PFYCUR:
	    return (AP_PFYCUR(psf))
	case OPFXCUR:
	    return (AP_OPFXCUR(psf))
	case OPFYCUR:
	    return (AP_OPFYCUR(psf))

	default:
	    call error (0, "APSTATR: Unknown apphot real parameter")
	}
end


# AP1STATD -- Procedure to set a double apphot parameter.

double procedure ap1statd (ap, param)

pointer	ap		# pointer to apphot structure
int	param		# parameter

pointer	cen, sky, phot, psf

begin
	cen = AP_PCENTER(ap)
	sky = AP_PSKY(ap)
	phot = AP_PPHOT(ap)
	psf = AP_PPSF(ap)

	switch (param) {
	default:
	    call error (0, "APSTATD: Unknown apphot double parameter")
	}
end
