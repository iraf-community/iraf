include "../lib/radprof.h"

# AP_PROFCOLON -- Procedure to display and modify radprof parameters.

procedure ap_profcolon (ap, out, stid, cmdstr, newbuf, newfit)

pointer	ap		# pointer to apphot structure
pointer	out		# output file descriptor
int	stid		# output file number
char	cmdstr[ARB]	# command string
int	newbuf		# new aperture buffers
int	newfit		# compute new magnitudes

int	ival, ncmd
pointer	sp, cmd
real	rval
int	strdic(), nscan(), apstati()
real	apstatr()

begin
	# Get the command.
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	call sscan (cmdstr)
	call gargwrd (Memc[cmd], SZ_LINE)
	if (Memc[cmd] == EOS)
	    return

	# Process the colon command.
	ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, RPCMDS)
	switch (ncmd) {
	case RCMD_RADIUS:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_RPRADIUS)
		    call pargr (apstatr (ap, RPRADIUS))
		    call pargstr (UN_RPRADIUS)
	    } else {
		call apsetr (ap, RPRADIUS, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_RPRADIUS, rval, UN_RPRADIUS,
			"fitting radius")
		newbuf = YES; newfit = YES
	    }
	case RCMD_STEPSIZE:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_RPSTEP)
		    call pargr (apstatr (ap, RPSTEP))
		    call pargstr (UN_RPSTEP)
	    } else {
		call apsetr (ap, RPSTEP, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_RPSTEP, rval, UN_RPSTEP,
			"step size in radius")
		newfit = YES
	    }
	case RCMD_ORDER:
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("%s = %d\n")
		    call pargstr (KY_RPORDER)
		    call pargi (apstati (ap, RPORDER))
	    } else {
		call apseti (ap, RPORDER, ival)
		if (stid > 1)
		    call ap_iparam (out, KY_RPORDER, ival, UN_RPORDER,
			"maximum number of rejection cycels")
		newfit = YES
	    }
	case RCMD_KREJECT:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_RPKSIGMA)
		    call pargr (apstatr (ap, RPKSIGMA))
		    call pargstr (UN_RPKSIGMA)
	    } else {
		call apsetr (ap, RPKSIGMA, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_RPKSIGMA, rval, UN_RPKSIGMA,
			"k-sigma rejection criteron")
		newfit = YES
	    }
	case RCMD_NREJECT:
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("%s = %d\n")
		    call pargstr (KY_RPNREJECT)
		    call pargi (apstati (ap, RPNREJECT))
	    } else {
		call apseti (ap, RPNREJECT, ival)
		if (stid > 1)
		    call ap_iparam (out, KY_RPNREJECT, ival, UN_RPNREJECT,
			"maximum number of rejection cycles")
		newfit = YES
	    }
	default:
	    call printf ("Unknown or ambiguous colon command\7\n")
	}
	call sfree (sp)
end
