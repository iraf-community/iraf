include "../lib/apphot.h"
include "../lib/display.h"
include "../lib/noise.h"
include "../lib/fitpsf.h"

# APSFCOLON -- Process the  fitpsf task colon commands.

procedure apsfcolon (ap, im, cl, out, stid, ltid, cmdstr, newimage, newbuf,
	newfit)

pointer	ap		# pointer to the apphot structure
pointer	im		# pointer to the iraf image
int	cl		# coordinate file descriptor
int	out		# output file descriptor
int	stid		# output file sequence number
int	ltid		# coord file sequence number
char	cmdstr		# command string
int	newimage	# new image ?
int	newbuf		# new psf buffer ?
int	newfit		# new psf fit ?

int	junk
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

	# Process the command.
	if (strdic (Memc[incmd], Memc[outcmd], SZ_LINE, PSFCMDS) != 0)
	    call ap_fitcolon (ap, out, stid, cmdstr, newbuf, newfit)
	else if (strdic (Memc[incmd], Memc[outcmd], SZ_LINE, APCMDS) != 0)
	    call ap_apcolon (ap, im, cl, out, stid, ltid, cmdstr, newimage,
	        junk, junk, junk, junk, newbuf, newfit)
	else if (strdic (Memc[incmd], Memc[outcmd], SZ_LINE, NCMDS) != 0)
	    call apnscolon (ap, im, out, stid, cmdstr, junk, junk,
	        junk, junk, newbuf, newfit)
	else
	    call ap_pfimcolon (ap, cmdstr)

	call sfree (sp)
end


# AP_FITCOLON -- Procedure to show/set the fitpsf parameters.

procedure ap_fitcolon (ap, out, stid, cmdstr, newbuf, newfit)

pointer	ap		# pointer to the apphot structure
int	out		# output file descriptor
int	stid		# output file sequence number
char	cmdstr		# command string
int	newbuf		# new psf buffer
int	newfit		# new psf fit

bool	bval
int	ncmd, ival, stat
pointer	sp, cmd, str
real	rval

bool	itob()
int	nscan(), strdic(), apstati(), btoi()
real	apstatr()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Get the command.
	call sscan (cmdstr)
	    call gargwrd (Memc[cmd], SZ_LINE)
	if (Memc[cmd] == EOS) {
	    call sfree (sp)
	    return
	}

	# Process the command.
	ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, PSFCMDS)
	switch (ncmd) {
	case PFCMD_FUNCTION:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (nscan() == 1) {
		call apstats (ap, PSFSTRING, Memc[str], SZ_FNAME)
		call printf ("%s = %s\n")
		    call pargstr (KY_PSFSTRING)
		    call pargstr (Memc[str])
	    } else {
		stat = strdic (Memc[cmd], Memc[cmd], SZ_LINE, PSFFUNCS)
		if (stat > 0) {
		    call apseti (ap, PSFUNCTION, stat)
		    call apsets (ap, PSFSTRING, Memc[cmd])
		    switch (stat) {
		    case AP_RADGAUSS:
			call apseti (ap, NPARS, 5)
		    case AP_ELLGAUSS:
		        call apseti (ap, NPARS, 7)
		    case AP_MOMENTS:
		        call apseti (ap, NPARS, 7)
		    }
		    if (stid > 1)
			call ap_sparam (out, "FUNCTION", Memc[cmd], "model",
			    "fitting function")
		    newfit = YES
		}
	    }
	case PFCMD_BOX:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_PSFAPERT)
		    call pargr (2.0 * apstatr (ap, PSFAPERT))
		    call pargstr (UN_PSFSCALEUNIT)
	    } else {
		call apsetr (ap, PSFAPERT, rval / 2.0)
		if (stid > 1)
		    call ap_rparam (out, KY_PSFAPERT, rval, UN_PSFSCALEUNIT,
			"fitting box width")
		newbuf = YES
		newfit = YES
	    }
	case PFCMD_KREJECT:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_PK2)
		    call pargr (apstatr (ap, PK2))
		    call pargstr (UN_PSFSIGMA)
	    } else {
		call apsetr (ap, PK2, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_PK2, rval, UN_PSFSIGMA,
			"k-sigma rejection criterion")
		newfit = YES
	    }
	case PFCMD_MAXITER:
	    call gargi (ival)
	    if (nscan () == 1) {
		call printf ("%s = %d\n")
		    call pargstr (KY_PMAXITER)
		    call pargi (apstati (ap, PMAXITER))
	    } else {
		call apseti (ap, PMAXITER, ival)
		if (stid > 1)
		    call ap_iparam (out, KY_PMAXITER, ival, UN_PSFNUMBER,
			"maximum number of iterations")
		newfit = YES
	    }
	case PFCMD_NREJECT:
	    call gargi (ival)
	    if (nscan () == 1) {
		call printf ("%s = %d\n")
		    call pargstr (KY_PNREJECT)
		    call pargi (apstati (ap, PNREJECT))
	    } else {
		call apseti (ap, PNREJECT, ival)
		if (stid > 1)
		    call ap_iparam (out, KY_PNREJECT, ival, UN_PSFNUMBER,
			"maximum number of rejection cycles")
		newfit = YES
	    }
	case PFCMD_MKBOX:
	    call gargb (bval)
	    if (nscan() == 1) {
		call printf ("%s = %b\n")
		    call pargstr (KY_MKPSFBOX)
		    call pargb (itob (apstati (ap, MKPSFBOX)))
	    } else
		call apseti (ap, MKPSFBOX, btoi (bval))
	default:
	    # do nothing gracefully
	    call printf ("Unknown or ambiguous colon command\n")
	}

	call sfree (sp)
end


# AP_PFIMCOLON -- Procedure to process fitpsf commands that are not fitpsf
# parameters.

procedure ap_pfimcolon (ap, cmdstr)

pointer	ap		# pointer to the apphot structure
char	cmdstr		# command string

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
	    ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, PFSHOWARGS)
	    switch (ncmd) {
	    case PFCMD_DATA:
		call printf ("\n")
		call ap_nshow (ap)
		call printf ("\n")
	    case PFCMD_FIT:
		call printf ("\n")
	        call ap_pfshow (ap)
		call printf ("\n")
	    default:
		call printf ("\n")
	        call ap_psfshow (ap)
		call printf ("\n")
	    }
	default:
	    call printf ("Unknown or ambigous colon command\7\n")
	}

	call sfree (sp)
end
