include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/center.h"

# AP_CCONFIRM -- Procedure to confirm the critical center parameters.

procedure ap_cconfirm (ap, out, stid)

pointer	ap		# pointer to the apphot structure
int	out		# output file descriptor
int	stid		# output file sequence number

pointer	sp, str
real	fwhmpsf, capert, skysigma, datamin, datamax
int	apstati()
real	apstatr(), ap_vfwhmpsf(), ap_vcapert(), ap_vsigma()
real	ap_vdatamin(), ap_vdatamax()

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

	    # Confirm the sky sigma parameter.
	    skysigma = ap_vsigma (ap)

	} else {

	    fwhmpsf = apstatr (ap, FWHMPSF)
	    capert = 2.0 * apstatr (ap, CAPERT)
	    skysigma = apstatr (ap, SKYSIGMA)

	}

	# Confirm the sky sigma parameter.

	# Confirm the minimum and maximum good data values.
	datamin = ap_vdatamin (ap)
	datamax = ap_vdatamax (ap)

	call printf ("\n")

	# Update the database file.
	if (out != NULL && stid > 1) {
	    call ap_sparam (out, KY_CSTRING, Memc[str], UN_CALGORITHM,
		"centering algorithm")
	    call ap_rparam (out, KY_FWHMPSF, fwhmpsf, UN_ASCALEUNIT,
	        "full width half maximum of the psf")
	    call ap_rparam (out, KY_CAPERT, capert, UN_CSCALEUNIT,
	        "centering box width")
	    call ap_rparam (out, KY_SKYSIGMA, skysigma, UN_NCOUNTS,
	        "standard deviation of 1 sky pixel")
	    call ap_rparam (out, KY_DATAMIN, datamin, UN_ACOUNTS,
	        "minimum good data value")
	    call ap_rparam (out, KY_DATAMAX, datamax, UN_ACOUNTS,
	        "maximum good data value")
	}

	call sfree (sp)
end
