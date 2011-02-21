include "../lib/apphot.h"
include "../lib/display.h"
include "../lib/noise.h"
include "../lib/find.h"

# AP_FDCOLON -- Process colon commands from the daofind task.

procedure ap_fdcolon (ap, im, out, stid, cmdstr, newimage, newbuf, newfit)

pointer	ap		# pointer to the apphot structure
pointer	im		# pointer to the iraf image
int	out		# output file descriptor
int	stid		# output file sequence number
char	cmdstr		# command string
int	newimage	# new mage ?
int	newbuf		# new center buffer ?
int	newfit		# new center fit ?

int	cl, junk
pointer	sp, incmd, outcmd
int	strdic()

begin
	call smark (sp)
	call salloc (incmd, SZ_LINE, TY_CHAR)
	call salloc (outcmd, SZ_LINE, TY_CHAR)

	# Get the command.
	call sscan (cmdstr)
	call gargwrd (Memc[incmd], SZ_LINE)
	if (Memc[incmd] == EOS) {
	    call sfree (sp)
	    return
	}

	# Process the command making sure that the pointer to the
	# coords file is always NULL.
	if (strdic (Memc[incmd], Memc[outcmd], SZ_LINE, APCMDS) != 0) {
	    cl = NULL
	    call ap_apcolon (ap, im, cl, out, stid, junk, cmdstr, newimage,
	        junk, junk, junk, junk, newbuf, newfit)
	    if (cl != NULL) {
		call close (cl)
		cl = NULL
		call apsets (ap, CLNAME, "")
	    }
	} else if (strdic (Memc[incmd], Memc[outcmd], SZ_LINE, NCMDS) != 0) {
	    call ap_nscolon (ap, im, out, stid, cmdstr, junk, junk,
		junk, junk, newbuf, newfit)
	} else if (strdic (Memc[incmd], Memc[outcmd], SZ_LINE, FCMDS) != 0) {
	    call ap_fcolon (ap, out, stid, cmdstr, newbuf, newfit)
	} else {
	    call ap_fimcolon (ap, cmdstr)
	}

	call sfree (sp)
end


# AP_FCOLON -- Process colon commands for setting the find algorithm
# parameters.

procedure ap_fcolon (ap, out, stid, cmdstr, newbuf, newfit)

pointer	ap			# pointer to the apphot structure
int	out			# output file descriptor
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
	if (Memc[cmd] == EOS) {
	    call sfree (sp)
	    return
	}

	# Process the command.
	ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, FCMDS)
	switch (ncmd) {

	case FCMD_NSIGMA:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_NSIGMA)
		    call pargr (apstatr (ap, NSIGMA))
		    call pargstr (UN_FSIGMA)
	    } else {
		call apsetr (ap, NSIGMA, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_NSIGMA, rval, UN_FSIGMA,
			"size of kernel in sigma")
		newbuf = YES; newfit = YES
	    }

	case FCMD_RATIO:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_RATIO)
		    call pargr (apstatr (ap, RATIO))
		    call pargstr (UN_FNUMBER)
	    } else {
		call apsetr (ap, RATIO, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_RATIO, rval, UN_FNUMBER,
			"sigma y / x of Gaussian kernel")
		newbuf = YES; newfit = YES
	    }

	case FCMD_SHARPLO:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_SHARPLO)
		    call pargr (apstatr (ap, SHARPLO))
		    call pargstr (UN_FNUMBER)
	    } else {
		call apsetr (ap, SHARPLO, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_SHARPLO, rval, UN_FNUMBER,
			"lower sharpness bound")
		newfit = YES
	    }

	case FCMD_SHARPHI:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_SHARPHI)
		    call pargr (apstatr (ap, SHARPHI))
		    call pargstr (UN_FNUMBER)
	    } else {
		call apsetr (ap, SHARPHI, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_SHARPHI, rval, UN_FNUMBER,
			"upper sharpness bound")
		newfit = YES
	    }

	case FCMD_ROUNDLO:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_ROUNDLO)
		    call pargr (apstatr (ap, ROUNDLO))
		    call pargstr (UN_FNUMBER)
	    } else {
		call apsetr (ap, ROUNDLO, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_ROUNDLO, rval, UN_FNUMBER,
			"lower roundness bound")
		newfit = YES
	    }

	case FCMD_ROUNDHI:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_ROUNDHI)
		    call pargr (apstatr (ap, ROUNDHI))
		    call pargstr (UN_FNUMBER)
	    } else {
		call apsetr (ap, ROUNDHI, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_ROUNDHI, rval, UN_FNUMBER,
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
		    call pargstr (UN_FDEGREES)
	    } else {
		call apsetr (ap, THETA, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_THETA, rval, UN_FDEGREES,
			"position angle")
		newbuf = YES; newfit = YES
	    }

	case FCMD_THRESHOLD:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_THRESHOLD)
		    call pargr (apstatr (ap, THRESHOLD))
		    call pargstr (UN_FSIGMA)
	    } else {
		call apsetr (ap, THRESHOLD, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_THRESHOLD, rval, UN_FSIGMA,
			"detection threshold in sigma")
		newfit = YES
	    }

	default:
	    call printf ("Unknown or ambiguous colon command\7\n")
	}

	call sfree (sp)
end


# AP_FIMCOLON --  Process colon commands for the daofind task that do
# not affect the data dependent or find parameters.

procedure ap_fimcolon (ap, cmdstr)

pointer	ap			# pointer to the apphot structure
char	cmdstr[ARB]		# command string

int	ncmd
pointer	sp, cmd
int	strdic()

begin
	# Get the command.
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call sscan (cmdstr)
	call gargwrd (Memc[cmd], SZ_LINE)
	if (Memc[cmd] == EOS) {
	    call sfree (sp)
	    return
	}

	# Process the command.
	ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, MISC1)
	switch (ncmd) {
	case ACMD_SHOW:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, FSHOWARGS)
	    switch (ncmd) {
	    case FCMD_DATA:
		call printf ("\n")
		call ap_nshow (ap)
		call printf ("\n")
	    case FCMD_FIND:
		call printf ("\n")
	        call ap_fshow (ap)
		call printf ("\n")
	    default:
		call printf ("\n")
	        call ap_fdshow (ap)
		call printf ("\n")
	    }
	default:
	    call printf ("Unknown or ambiguous colon command\7\n")
	}

	call sfree (sp)
end
