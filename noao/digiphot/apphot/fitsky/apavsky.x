include "../lib/display.h"
include "../lib/fitsky.h"

# AP_AVSKY -- Compute an estimate of the sky value by averaging several
# individual sky values measured in different parts of the frame.

int procedure ap_avsky (ap, im, stid, sd, id, gd, interactive)

pointer	ap			# the pointer to the main apphot data structure
pointer	im			# the pointer to the input image
int	stid			# the current sequence number
int	sd			# the sky file descriptor
pointer	id			# the display stream descriptor
pointer	gd			# the graphics stream descriptor
int	interactive		# interactive mode

int	wcs, key, nmsky, nmsigma, nmskew, nsky, nrej, sier, ier
pointer	sp, cmd
real	wx, wy, msky, msigma, mskew
int	clgcur(), apfitsky(), apstati()
real	apstatr()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Initialize the skyvalues.
	msky = 0.0
	nmsky = 0
	msigma = 0.0
	nmsigma = 0
	mskew = 0.0
	nmskew = 0
	nsky = 0
	nrej = 0
	ier = AP_OK

	call printf (
	"\nMeasure sky around several cursor positions (t=measure, q=quit)\n")
	while (clgcur ("icommands", wx, wy, wcs, key, Memc[cmd], SZ_LINE) !=
	    EOF) {

	    switch (key) {
	    case 'q':
		break

	    case 't':
		sier = apfitsky (ap, im, wx, wy, sd, gd)
		if (sier != AP_OK)
		    ier = sier

		if (id != NULL) {
		    call apmark (ap, id, NO, apstati (ap, MKSKY), NO)
		    if (id == gd)
			call gflush (id)
		    else
		        call gframe (id)
		}
		call ap_splot (ap, stid, gd, apstati (ap, RADPLOTS))
		if (interactive == YES)
		    call ap_qspsky (ap, sier)

		if (! IS_INDEFR(apstatr (ap, SKY_MODE))) {
		    msky = msky + apstatr (ap, SKY_MODE)
		    nmsky = nmsky + 1
		}
		if (! IS_INDEFR(apstatr (ap, SKY_SIGMA))) {
		    msigma = msigma + apstatr (ap, SKY_SIGMA)
		    nmsigma = nmsigma + 1
		}
		if (! IS_INDEFR(apstatr (ap, SKY_SKEW))) {
		    mskew = mskew + apstatr (ap, SKY_SKEW)
		    nmskew = nmskew + 1
		}
		nsky = nsky + apstati (ap, NSKY)
		nrej = nrej + apstati (ap, NSKY_REJECT)

	    default:
		;
	    }
	}

	# Compute the average values.
	if (nmsky > 0)
	    msky = msky / nmsky
	else
	    msky = INDEFR
	if (nmsigma > 0)
	    msigma = msigma / nmsigma
	else
	    msigma = INDEFR
	if (nmskew > 0)
	    mskew = mskew / nmskew
	else
	    mskew = INDEFR

	# Store the average values.
	call apsetr (ap, SKY_MODE, msky)
	call apsetr (ap, SKY_SIGMA, msigma)
	call apsetr (ap, SKY_SKEW, mskew)
	call apseti (ap, NSKY, nsky)
	call apseti (ap, NSKY_REJECT, nrej)

	call sfree (sp)

	return (ier)
end
