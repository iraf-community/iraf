include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/center.h"
include "../lib/fitsky.h"
include "../lib/phot.h"
include "../lib/radprof.h"

# AP_RCONFIRM -- Procedure to confirm the critical phot parameters.

procedure ap_rconfirm (ap, out, stid)

pointer	ap		# pointer to the apphot structure
int	out		# output file descriptor
int	stid		# output file sequence number

pointer	sp, cstr, sstr, aperts
real	fwhmpsf, capert, cthreshold, annulus, dannulus, skysigma, radius, step
int	apstati()
real	apstatr(), ap_vfwhmpsf(), ap_vcthreshold(), ap_vcapert()
real	ap_vannulus(), ap_vdannulus(), ap_vsigma(), ap_vstep(), ap_vrpradius()

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

	    # Confirm the centering threshold parameter.
	    cthreshold = ap_vcthreshold (ap)

	} else {
	    fwhmpsf = apstatr (ap, FWHMPSF)
	    capert = 2.0 * apstatr (ap, CAPERT)
	    cthreshold = apstatr (ap, CTHRESHOLD)
	}

	# Confirm the sky fitting algorithm.
	call ap_vsstring (ap, Memc[sstr], SZ_FNAME)

	if (apstati (ap, SKYFUNCTION) != AP_CONSTANT &&
	    apstati (ap, SKYFUNCTION) != AP_SKYFILE) {

	    # Confirm the sky annulus parameter.
	    annulus = ap_vannulus (ap)

	    # Confirm the width of the sky annulus.
	    dannulus = ap_vdannulus (ap)

	} else {
	    annulus = apstatr (ap, ANNULUS)
	    dannulus = apstatr (ap, DANNULUS)
	}

	# Confirm the sky sigma parameter.
	if (apstati (ap, SKYFUNCTION) != AP_SKYFILE)
	    skysigma = ap_vsigma (ap)
	else
	    skysigma = apstatr (ap, SKYSIGMA)

	# Confirm the aperture radii parameter.
	call ap_vaperts (ap, Memc[aperts], SZ_LINE)

	# Confirm the radius of profile
	radius = ap_vrpradius (ap)

	# Confirm the step size of profile
	step = ap_vstep (ap)

	call printf ("\n")

	# Update the database file.
	if (out != NULL && stid > 1) {
	    call ap_sparam (out, KY_CSTRING, Memc[cstr], UN_CSTRING,
		"centering algorithm")
	    call ap_rparam (out, KY_FWHMPSF, fwhmpsf, UN_FWHMPSF,
	        "full width half maximum of the psf")
	    call ap_rparam (out, KY_CAPERT, capert, UN_CAPERT,
	        "centering box width")
	    call ap_sparam (out, KY_SSTRING, Memc[sstr], UN_SSTRING,
		"sky fitting algorithm")
	    call ap_rparam (out, KY_ANNULUS, annulus, UN_ANNULUS,
	        "inner radius of the sky annulus")
	    call ap_rparam (out, KY_DANNULUS, dannulus, UN_DANNULUS,
	        "width of the sky annulus")
	    call ap_rparam (out, KY_CTHRESHOLD, cthreshold, UN_CTHRESHOLD,
	        "centering threshold")
	    call ap_rparam (out, KY_SKYSIGMA, skysigma, UN_SKYSIGMA,
	        "standard deviation of 1 sky pixel")
	    call ap_sparam (out, KY_APERTS, Memc[aperts], UN_APERTS,
		"list of apertures")
	    call ap_rparam (out, KY_RPRADIUS, radius, UN_RPRADIUS,
	        "fitting radius")
	    call ap_rparam (out, KY_RPSTEP, step, UN_RPSTEP,
	        "step size in radius")
	}

	call sfree (sp)
end
