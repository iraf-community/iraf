include "../lib/apphot.h"

# APIMKEYS - Set the image name and keyword parameters after an image
# is mapped.

procedure apimkeys (ap, im, imname)

pointer	ap			# pointer to the apphot structure
pointer	im			# the image descriptor
char	imname[ARB]		# the input image name

pointer	sp, imroot, mw, ct
int	apstati()
pointer	mw_openim(), mw_sctran()
errchk	mw_openim(), mw_sctran()

begin
	call smark (sp)
	call salloc (imroot, SZ_FNAME, TY_CHAR)

	# Set the image and root names.
        call apsets (ap, IMNAME, imname)
	call apimroot (imname, Memc[imroot], SZ_FNAME)
	call apsets (ap, IMROOT, Memc[imroot])

	# Set the wcs descriptors.
	mw = apstati (ap, MW)
	if (mw != NULL)
	    call mw_close (mw)
	iferr {
	    mw = mw_openim (im) 
	} then {
	    call apseti (ap, MW, NULL)
	    call apseti (ap, CTIN, NULL)
	    call apseti (ap, CTOUT, NULL)
	} else {
	    call apseti (ap, MW, mw)
	    switch (apstati (ap, WCSIN)) {
	    case WCS_WORLD:
		iferr (ct = mw_sctran (mw, "world", "logical", 03B))
		    ct = NULL
	    case WCS_PHYSICAL:
		iferr (ct = mw_sctran (mw, "physical", "logical", 03B))
		    ct = NULL
	    case WCS_TV, WCS_LOGICAL:
		ct = NULL
	    default:
		ct = NULL
	    }
	    call apseti (ap, CTIN, ct)
	    switch (apstati (ap, WCSOUT)) {
	    case WCS_PHYSICAL:
		iferr (ct = mw_sctran (mw, "logical", "physical", 03B))
		    ct = NULL
	    case WCS_TV, WCS_LOGICAL:
		ct = NULL
	    default:
		ct = NULL
	    }
	    call apseti (ap, CTOUT, ct)
	}

	# Set the keywords.
	call ap_rdnoise (im, ap)
        call ap_padu (im, ap)
        call ap_itime (im, ap)
        call ap_airmass (im, ap)
        call ap_filter (im, ap)
        call ap_otime (im, ap)

	call sfree (sp)
end
