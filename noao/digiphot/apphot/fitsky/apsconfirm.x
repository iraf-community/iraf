include "../lib/noise.h"
include "../lib/fitsky.h"

# AP_SCONFIRM -- Procedure to confirm the critical fitsky parameters.

procedure ap_sconfirm (ap, out, stid)

pointer	ap		# pointer to the apphot structure
int	out		# output file descriptor
int	stid		# output file sequence number

pointer	sp, str
real	annulus, dannulus, skysigma
int	apstati()
real	apstatr(), ap_vannulus(), ap_vdannulus(), ap_vsigma()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	call printf ("\n")

	# Confirm the sky fitting algorithm.
	call ap_vsstring (ap, Memc[str], SZ_FNAME)

	# Confirm the remaining parameters.
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

	call printf ("\n")

	# Update the database file.
	if (out != NULL && stid > 1) {
	    call ap_sparam (out, KY_SSTRING, Memc[str], UN_SSTRING,
		"sky fitting algorithm")
	    call ap_rparam (out, KY_ANNULUS, annulus, UN_ANNULUS,
	        "inner radius of sky annulus")
	    call ap_rparam (out, KY_DANNULUS, dannulus, UN_DANNULUS,
	        "width of the sky annulus")
	    call ap_rparam (out, KY_SKYSIGMA, skysigma, UN_SKYSIGMA,
	        "standard deviation of 1 sky pixel") 
	}

	call sfree (sp)
end
