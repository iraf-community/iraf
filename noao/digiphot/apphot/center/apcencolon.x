include "../lib/apphot.h"
include "../lib/display.h"
include "../lib/noise.h"
include "../lib/center.h"

# APCENTERCOLON -- Process colon commands from the centering task.

procedure apcentercolon (ap, im, cl, out, stid, ltid, cmdstr, newimage,
	newbuf, newfit)

pointer	ap		# pointer to the apphot structure
pointer	im		# pointer to the iraf image
int	cl		# coordinate file descriptor
int	out		# output file descriptor
int	stid		# output file sequence number
int	ltid		# input list sequence number
char	cmdstr[ARB]	# command string
int	newimage	# new image ?
int	newbuf		# new center buffer ?
int	newfit		# new center fit ?

int	junk
pointer	sp, incmd, outcmd
int	strdic()

begin
	# Allocate working space.
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
	if (strdic (Memc[incmd], Memc[outcmd], SZ_LINE, CCMDS) != 0)
	    call ap_ccolon (ap, out, stid, cmdstr, newbuf, newfit)
	else if (strdic (Memc[incmd], Memc[outcmd], SZ_LINE, APCMDS) != 0)
	    call ap_apcolon (ap, im, cl, out, stid, ltid, cmdstr, newimage,
	        newbuf, newfit, junk, junk, junk, junk)
	else if (strdic (Memc[incmd], Memc[outcmd], SZ_LINE, NCMDS) != 0)
	    call ap_nscolon (ap, im, out, stid, cmdstr, newbuf, newfit,
		junk, junk, junk, junk)
	else
	    call ap_cimcolon (ap, cmdstr) 

	call sfree (sp)
end


# AP_CCOLON -- Process colon commands affecting the centering parameters.

procedure ap_ccolon (ap, out, stid, cmdstr, newbuf, newfit)

pointer	ap		# pointer to the apphot structure
int	out		# output file descriptor
int	stid		# output list id number
char	cmdstr		# command string
int	newbuf		# new sky buffer
int	newfit		# new sky fit

bool	bval
int	ival, ncmd, stat
pointer	sp, cmd, str
real	rval

bool	itob()
int	nscan(), strdic(), btoi(), apstati()
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
	ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, CCMDS)
	switch (ncmd) {

	case CCMD_CBOXWIDTH:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_CAPERT)
		    call pargr (2.0 * apstatr (ap, CAPERT))
		    call pargstr (UN_CSCALEUNIT)
	    } else {
		call apsetr (ap, CAPERT, (rval / 2.0))
		if (stid > 1)
		    call ap_rparam (out, KY_CAPERT, rval, UN_CSCALEUNIT,
			"centering subraster width")
		newbuf = YES
		newfit = YES
	    }

	case CCMD_CALGORITHM:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (Memc[cmd] == EOS) {
		call apstats (ap, CSTRING, Memc[str], SZ_FNAME)
	        call printf ("%s = %s %s\n")
		    call pargstr (KY_CSTRING)
		    call pargstr (Memc[str])
		    call pargstr (UN_CALGORITHM)
	    } else {
		stat = strdic (Memc[cmd], Memc[cmd], SZ_LINE, CFUNCS)
		if (stat > 0) {
		    call apseti (ap, CENTERFUNCTION, stat)
		    call apsets (ap, CSTRING, Memc[cmd])
		    if (stid > 1)
		        call ap_sparam (out, KY_CSTRING, Memc[cmd],
			    UN_CALGORITHM, "centering algorihtm")
		    newbuf = YES
		    newfit = YES
		}
	    }

	case CCMD_CTHRESHOLD:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_CTHRESHOLD)
		    call pargr (apstatr (ap, CTHRESHOLD))
		    call pargstr (UN_CSIGMA)
	    } else {
		call apsetr (ap, CTHRESHOLD, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_CTHRESHOLD, rval, UN_CSIGMA,
			"threshold for centering")
		newbuf = YES
		newfit = YES
	    }

	case CCMD_MAXSHIFT:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_MAXSHIFT)
		    call pargr (apstatr (ap, MAXSHIFT))
		    call pargstr (UN_CSCALEUNIT)
	    } else {
		call apsetr (ap, MAXSHIFT, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_MAXSHIFT, rval, UN_CSCALEUNIT,
			"maximum shift")
		newfit = YES
	    }

	case CCMD_MINSNRATIO:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g\n")
		    call pargstr (KY_MINSNRATIO)
		    call pargr (apstatr (ap, MINSNRATIO))
	    } else {
		call apsetr (ap, MINSNRATIO, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_MINSNRATIO, rval,
		        UN_CNUMBER, "minimum signal-to-noise ratio")
		newfit = YES
	    }

	case CCMD_CMAXITER:
	    call gargi (ival)
	    if (nscan () == 1) {
		call printf ("%s = %d\n")
		    call pargstr (KY_CMAXITER)
		    call pargi (apstati (ap, CMAXITER))
	    } else {
		call apseti (ap, CMAXITER, ival)
		if (stid > 1)
		    call ap_iparam (out, KY_CMAXITER, ival, UN_CNUMBER,
			"maximum number of iterations")
		newfit = YES
	    }

	case CCMD_CLEAN:
	    call gargb (bval)
	    if (nscan () == 1) {
		call printf ("%s = %b\n")
		    call pargstr (KY_CLEAN)
		    call pargb (itob (apstati (ap, CLEAN)))
	    } else {
		call apseti (ap, CLEAN, btoi (bval))
		if (stid > 1)
		    call ap_bparam (out, KY_CLEAN, bval, UN_CSCALEUNIT,
			"apply clean algorithm before centering")
		newbuf = YES
		newfit = YES
	    }

	case  CCMD_RCLEAN:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_RCLEAN)
		    call pargr (apstatr (ap, RCLEAN))
		    call pargstr (UN_CSCALEUNIT)
	    } else {
		call apsetr (ap, RCLEAN, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_RCLEAN, rval, UN_CSCALEUNIT,
			"cleaning radius")
		newbuf = YES
		newfit = YES
	    }

	case CCMD_RCLIP:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_RCLIP)
		    call pargr (apstatr (ap, RCLIP))
		    call pargstr (UN_CSCALEUNIT)
	    } else {
		call apsetr (ap, RCLIP, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_RCLIP, rval, UN_CSCALEUNIT,
			"clipping radius")
		newbuf = YES
		newfit = YES
	    }

	case CCMD_KCLEAN:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_SIGMACLEAN)
		    call pargr (apstatr (ap, SIGMACLEAN))
		    call pargstr (UN_CSIGMA)
	    } else {
		call apsetr (ap, SIGMACLEAN, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_SIGMACLEAN, rval,
		        UN_CSIGMA, "k-sigma clean rejection criterion")
		newbuf = YES
		newfit = YES
	    }

	case CCMD_MKCENTER:
	    call gargb (bval)
	    if (nscan() == 1) {
		call printf ("%s = %b\n")
		    call pargstr (KY_MKCENTER)
		    call pargb (itob (apstati (ap, MKCENTER)))
	    } else
		call apseti (ap, MKCENTER, btoi (bval))

	default:
	    call printf ("Unknown or ambiguous colon command\7\n")
	}

	call sfree (sp)
end


# AP_CIMCOLON --  Process colon commands for the center task that do
# not affect the centering parameters

procedure ap_cimcolon (ap, cmdstr)

pointer	ap			# pointer to the apphot structure
char	cmdstr[ARB]		# command string

bool	bval
int	ncmd
pointer	sp, cmd
bool	itob()
int	strdic(), nscan(), apstati(), btoi()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Get the command.
	call sscan (cmdstr)
	call gargwrd (Memc[cmd], SZ_LINE)
	if (Memc[cmd] == EOS) {
	    call sfree (sp)
	    return
	}

	# Process the command.
	ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, MISC)
	switch (ncmd) {
	case ACMD_SHOW:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, CSHOWARGS)
	    switch (ncmd) {
	    case CCMD_DATA:
		call printf ("\n")
		call ap_nshow (ap)
		call printf ("\n")
	    case CCMD_CENTER:
		call printf ("\n")
	        call ap_cpshow (ap)
		call printf ("\n")
	    default:
		call printf ("\n")
	        call ap_cshow (ap)
		call printf ("\n")
	    }
	case ACMD_RADPLOTS:
	    call gargb (bval)
	    if (nscan () == 1) {
		call printf ("%s = %b\n")
		    call pargstr (KY_RADPLOTS)
		    call pargb (itob (apstati (ap, RADPLOTS)))
	    } else {
		call apseti (ap, RADPLOTS, btoi (bval))
	    }
	default:
	    call printf ("Unknown or ambiguous colon command\7\n")
	}

	call sfree (sp)
end
