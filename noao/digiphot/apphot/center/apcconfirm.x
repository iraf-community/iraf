include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/center.h"

# AP_CCONFIRM -- Procedure to confirm the critical center parameters.

procedure ap_cconfirm (ap, out, stid)

pointer	ap		# pointer to the apphot structure
int	out		# output file descriptor
int	stid		# output file sequence number

pointer	sp, str
real	fwhmpsf, capert, cthreshold, skysigma
int	apstati()
real	apstatr(), ap_vfwhmpsf(), ap_vcapert(), ap_vcthreshold(), ap_vsigma()

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	call printf ("\n")

	# Confirm the centering algorithm.
	call ap_vcstring (ap, Memc[str], SZ_FNAME)

	if (apstati (ap, CENTERFUNCTION) != AP_NONE) {

	    # Confirm the fwhmpsf.
	    if (apstati (ap, CENTERFUNCTION) != AP_CENTROID1D) {
		fwhmpsf = ap_vfwhmpsf (ap)
	    } else
	    	fwhmpsf = apstatr (ap, FWHMPSF)

	    # Confirm the centering box.
	    capert = 2.0 * ap_vcapert (ap)

	    # Confirm the centering threshold parameter.
	    cthreshold = ap_vcthreshold (ap)

	} else {

	    fwhmpsf = apstatr (ap, FWHMPSF)
	    capert = 2.0 * apstatr (ap, CAPERT)
	    cthreshold = apstatr (ap, CTHRESHOLD)

	}

	# Confirm the sky sigma parameter.
	skysigma = ap_vsigma (ap)

	call printf ("\n")

	# Update the database file.
	if (out != NULL && stid > 1) {
	    call ap_sparam (out, KY_CSTRING, Memc[str], UN_CSTRING,
		"centering algorithm")
	    call ap_rparam (out, KY_FWHMPSF, fwhmpsf, UN_FWHMPSF,
	        "full width half maximum of the psf")
	    call ap_rparam (out, KY_CAPERT, capert, UN_CAPERT,
	        "centering box width")
	    call ap_rparam (out, KY_CTHRESHOLD, cthreshold, UN_CTHRESHOLD,
	        "threshold for centering ")
	    call ap_rparam (out, KY_SKYSIGMA, skysigma, UN_SKYSIGMA,
	        "standard deviation of 1 sky pixel")
	}

	call sfree (sp)
end
