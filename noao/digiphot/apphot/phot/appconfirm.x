include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/center.h"
include "../lib/fitsky.h"
include "../lib/phot.h"

# AP_PCONFIRM -- Procedure to confirm the critical phot parameters.

procedure ap_pconfirm (ap, out, stid)

pointer	ap		# pointer to the apphot structure
int	out		# output file descriptor
int	stid		# output file sequence number

pointer	sp, cstr, sstr, aperts
real	fwhmpsf, capert, annulus, dannulus, skysigma
real	datamin, datamax
int	apstati()
real	apstatr(), ap_vcapert(), ap_vannulus(), ap_vdannulus(), ap_vsigma()
real	ap_vfwhmpsf(), ap_vdatamin(), ap_vdatamax

begin
	call smark (sp)
	call salloc (cstr, SZ_FNAME, TY_CHAR)
	call salloc (sstr, SZ_FNAME, TY_CHAR)
	call salloc (aperts, SZ_LINE, TY_CHAR)

	call printf ("\n")

	# Confirm the centering algorithm.
	call ap_vcstring (ap, Memc[cstr], SZ_FNAME)

	if (apstati (ap, CENTERFUNCTION) != AP_NONE) {

	    # Confirm the fwhmpsf.
	    if (apstati (ap, CENTERFUNCTION) != AP_CENTROID1D)
		fwhmpsf = ap_vfwhmpsf (ap)
	    else
		fwhmpsf = apstatr (ap, FWHMPSF)

	    # Confirm the centering box.
	    capert = 2.0 * ap_vcapert (ap)

	} else {

	    fwhmpsf = apstatr (ap, FWHMPSF)
	    capert = 2.0 * apstatr (ap, CAPERT)
	}

	# Confirm the sky fitting algorithm.
	call ap_vsstring (ap, Memc[sstr], SZ_FNAME)

	# Confirm the sky annulus.
	if (apstati (ap, SKYFUNCTION) != AP_CONSTANT &&
	    apstati (ap, SKYFUNCTION) != AP_SKYFILE) {
	    annulus = ap_vannulus (ap)
	    dannulus = ap_vdannulus (ap)
	} else {
	    annulus = apstatr (ap, ANNULUS)
	    dannulus = apstatr (ap, DANNULUS)
	}

	# Confirm the aperture radii parameter.
	call ap_vaperts (ap, Memc[aperts], SZ_LINE)

	# Confirm the datamin and datamax parameters.
	skysigma = ap_vsigma (ap)
	datamin = ap_vdatamin (ap)
	datamax = ap_vdatamax (ap)

	call printf ("\n")

	# Update the database file.
	if (out != NULL && stid > 1) {
	    call ap_sparam (out, KY_CSTRING, Memc[cstr], UN_CALGORITHM,
		"centering aperture")
	    call ap_rparam (out, KY_FWHMPSF, fwhmpsf, UN_ASCALEUNIT,
	        "full width half maximum of the psf")
	    call ap_rparam (out, KY_CAPERT, capert, UN_CSCALEUNIT,
	        "centering box width")
	    call ap_sparam (out, KY_SSTRING, Memc[sstr], UN_SALGORITHM,
		"sky fitting algorithm")
	    call ap_rparam (out, KY_ANNULUS, annulus, UN_SSCALEUNIT,
	        "inner radius of the sky annulus")
	    call ap_rparam (out, KY_DANNULUS, dannulus, UN_SSCALEUNIT,
	        "width of the sky annulus")
	    call ap_rparam (out, KY_SKYSIGMA, skysigma, UN_NCOUNTS,
	        "standard deviation of 1 sky pixel")
	    call ap_sparam (out, KY_APERTS, Memc[aperts], UN_PSCALEUNIT,
		"list of apertures")
	    call ap_rparam (out, KY_DATAMIN, datamin, UN_ACOUNTS,
	        "minimum good data value")
	    call ap_rparam (out, KY_DATAMAX, datamax, UN_ACOUNTS,
	        "maximum good data value")
	}

	call sfree (sp)
end
