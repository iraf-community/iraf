include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/display.h"
include "../lib/find.h"

# AP_FCOLON -- Process colon commands for setting the find algorithm
# parameters.

procedure ap_fcolon (ap, out, stid, cmdstr, newbuf, newfit)

pointer	ap			# pointer to the apphot structure
pointer	out			# output file descriptor
int	stid			# file number id
char	cmdstr[ARB]		# command string
int	newbuf, newfit		# change magnitude parameters

bool	bval
int	ncmd
pointer	sp, cmd, str
real	rval

bool	itob()
int	strdic(), nscan(), btoi(), apstati()
real	apstatr()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Get the command.
	call sscan (cmdstr)
	    call gargwrd (Memc[cmd], SZ_LINE)
	if (Memc[cmd] == EOS)
	    return

	# Process the command.
	ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, FCMDS)
	switch (ncmd) {
	case FCMD_NSIGMA:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_NSIGMA)
		    call pargr (apstatr (ap, NSIGMA))
		    call pargstr (UN_NSIGMA)
	    } else {
		call apsetr (ap, NSIGMA, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_NSIGMA, rval, UN_NSIGMA,
			"size of kernel in sigma")
		newbuf = YES; newfit = YES
	    }
	case FCMD_RATIO:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_RATIO)
		    call pargr (apstatr (ap, RATIO))
		    call pargstr (UN_RATIO)
	    } else {
		call apsetr (ap, RATIO, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_RATIO, rval, UN_RATIO,
			"sigma y / x of Gaussian kernel")
		newbuf = YES; newfit = YES
	    }
	case FCMD_SHARPLO:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_SHARPLO)
		    call pargr (apstatr (ap, SHARPLO))
		    call pargstr (UN_SHARPLO)
	    } else {
		call apsetr (ap, SHARPLO, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_SHARPLO, rval, UN_SHARPLO,
			"lower sharpness bound")
		newfit = YES
	    }
	case FCMD_SHARPHI:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_SHARPHI)
		    call pargr (apstatr (ap, SHARPHI))
		    call pargstr (UN_SHARPHI)
	    } else {
		call apsetr (ap, SHARPHI, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_SHARPHI, rval, UN_SHARPHI,
			"upper sharpness bound")
		newfit = YES
	    }
	case FCMD_ROUNDLO:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_ROUNDLO)
		    call pargr (apstatr (ap, ROUNDLO))
		    call pargstr (UN_ROUNDLO)
	    } else {
		call apsetr (ap, ROUNDLO, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_ROUNDLO, rval, UN_ROUNDLO,
			"lower roundness bound")
		newfit = YES
	    }
	case FCMD_ROUNDHI:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_ROUNDHI)
		    call pargr (apstatr (ap, ROUNDHI))
		    call pargstr (UN_ROUNDHI)
	    } else {
		call apsetr (ap, ROUNDHI, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_ROUNDHI, rval, UN_ROUNDHI,
			"upper roundness bound")
		newfit = YES
	    }
	case FCMD_MKDETECTIONS:
	    call gargb (bval)
	    if (nscan () == 1) {
		call printf ("%s = %b\n")
		    call pargstr (KY_MKDETECTIONS)
		    call pargb (itob (apstati (ap, MKDETECTIONS)))
	    } else
		call apseti (ap, MKDETECTIONS, btoi (bval))
	case FCMD_THETA:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_THETA)
		    call pargr (apstatr (ap, THETA))
		    call pargstr (UN_THETA)
	    } else {
		call apsetr (ap, THETA, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_THETA, rval, UN_THETA,
			"position angle")
		newfit = YES
	    }
	default:
	    call printf ("Unknown or ambiguous colon command\7\n")
	}

	call sfree (sp)
end
