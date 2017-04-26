include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/fitsky.h"
include "../lib/display.h"


# APSKYCOLON -- Procedure to process the fitsky colon commands.

procedure apskycolon (ap, im, cl, out, stid, ltid, cmdstr, newimage,
	newskybuf, newsky)

pointer	ap				# pointer to the apphot structure
pointer	im				# pointer to the iraf image
int	cl				# coordinate file descriptor
int	out				# output file descriptor
int	stid				# output file sequence number
int	ltid				# coord list sequence number
char	cmdstr[ARB]			# command string
int	newimage			# new image ?
int	newskybuf			# new sky buffer ?
int	newsky				# new sky fit ?

int	junk
pointer	sp, incmd, outcmd
int	strdic()

begin
	# Get the command.
	call smark (sp)
	call salloc (incmd, SZ_LINE, TY_CHAR)
	call salloc (outcmd, SZ_LINE, TY_CHAR)
	call sscan (cmdstr)
	call gargwrd (Memc[incmd], SZ_LINE)
	if (Memc[incmd] == EOS) {
	    call sfree (sp)
	    return
	}

	# Process the command.
	if (strdic (Memc[incmd], Memc[outcmd], SZ_LINE, SCMDS) != 0)
	    call apscolon (ap, out, stid, cmdstr, newskybuf, newsky)
	else if (strdic (Memc[incmd], Memc[outcmd], SZ_LINE, APCMDS) != 0)
	    call ap_apcolon (ap, im, cl, out, stid, ltid, cmdstr,
	        newimage, junk, junk, newskybuf, newsky, junk, junk)
	else if (strdic (Memc[incmd], Memc[outcmd], SZ_LINE, NCMDS) != 0)
	    call apnscolon (ap, im, out, stid, cmdstr, junk, junk,
	        newskybuf, newsky, junk, junk)
	else
	    call ap_simcolon (ap, cmdstr)

	call sfree (sp)
end


# APSCOLON --  Procedure to examine and edit the sky fitting parameters.

procedure apscolon (ap, out, stid, cmdstr, newbuf, newfit)

pointer	ap		# pointer to the apphot structure
int	out		# output file descriptor
int	stid		# output file number
char	cmdstr		# command string
int	newbuf		# new sky buffer
int	newfit		# new sky fit

bool	bval
int	ncmd, ival, stat
pointer	sp, cmd
real	rval

bool	itob()
int	nscan(), strdic(), btoi(), apstati()
real	apstatr()


begin
	# Get the command
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call sscan (cmdstr)
	call gargwrd (Memc[cmd], SZ_LINE)
	if (Memc[cmd] == EOS) {
	    call sfree (sp)
	    return
	}

	# Process the command.
	ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, SCMDS)
	switch (ncmd) {
	case SCMD_ANNULUS:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_ANNULUS)
		    call pargr (apstatr (ap, ANNULUS))
		    call pargstr (UN_SSCALEUNIT)
	    } else {
		call apsetr (ap, ANNULUS, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_ANNULUS, rval, UN_SSCALEUNIT,
			"inner radius of sky annulus")
		newbuf = YES
		newfit = YES
	    }
	case SCMD_DANNULUS:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_DANNULUS)
		    call pargr (apstatr (ap, DANNULUS))
		    call pargstr (UN_SSCALEUNIT)
	    } else {
		call apsetr (ap, DANNULUS, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_DANNULUS, rval, UN_SSCALEUNIT,
			"width of the sky annulus")
		newbuf = YES
		newfit = YES
	    }
	case SCMD_SALGORITHM:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (Memc[cmd] == EOS) {
		call apstats (ap, SSTRING, Memc[cmd], SZ_FNAME)
	        call printf ("%s = %s\n")
		    call pargstr (KY_SSTRING)
		    call pargstr (Memc[cmd])
	    } else {
		stat = strdic (Memc[cmd], Memc[cmd], SZ_LINE, SFUNCS)
		if (stat > 0) {
		    call apseti (ap, SKYFUNCTION, stat)
		    call apsets (ap, SSTRING, Memc[cmd])
		    if (stid > 1)
		        call ap_sparam (out, KY_SSTRING, Memc[cmd],
			    UN_SALGORITHM, "sky fitting algorithm")
		    newfit = YES
		}
	    }
	case SCMD_KHIST:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_K1)
		    call pargr (apstatr (ap, K1))
		    call pargstr (UN_SSIGMA)
	    } else {
		call apsetr (ap, K1, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_K1, rval, UN_SSIGMA,
			"half width of sky histogram")
		newfit = YES
	    }
	case SCMD_SLOREJECT:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_SLOREJECT)
		    call pargr (apstatr (ap, SLOREJECT))
		    call pargstr (UN_SSIGMA)
	    } else {
		call apsetr (ap, SLOREJECT, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_SLOREJECT, rval, UN_SSIGMA,
			"lower k-sigma rejection criterion")
		newfit = YES
	    }
	case SCMD_SHIREJECT:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_SHIREJECT)
		    call pargr (apstatr (ap, SHIREJECT))
		    call pargstr (UN_SSIGMA)
	    } else {
		call apsetr (ap, SHIREJECT, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_SHIREJECT, rval, UN_SSIGMA,
			"upper k-sigma rejection criterion")
		newfit = YES
	    }
	case SCMD_SLOCLIP:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_SLOCLIP)
		    call pargr (apstatr (ap, SLOCLIP))
		    call pargstr (UN_SPERCENT)
	    } else {
		call apsetr (ap, SLOCLIP, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_SLOCLIP, rval, UN_SPERCENT,
			"lower k-sigma rejection criterion")
		newfit = YES
	    }
	case SCMD_SHICLIP:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_SHICLIP)
		    call pargr (apstatr (ap, SHICLIP))
		    call pargstr (UN_SPERCENT)
	    } else {
		call apsetr (ap, SHICLIP, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_SHICLIP, rval, UN_SPERCENT,
			"lower k-sigma rejection criterion")
		newfit = YES
	    }
	case SCMD_SMAXITER:
	    call gargi (ival)
	    if (nscan () == 1) {
		call printf ("%s = %d\n")
		    call pargstr (KY_SMAXITER)
		    call pargi (apstati (ap, SMAXITER))
	    } else {
		call apseti (ap, SMAXITER, ival)
		if (stid > 1)
		    call ap_iparam (out, KY_SMAXITER, ival, UN_SNUMBER,
			"maximum number of iterations")
		newfit = YES
	    }
	case SCMD_BINSIZE:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_BINSIZE)
		    call pargr (apstatr (ap, BINSIZE))
		    call pargstr (UN_SSIGMA)
	    } else {
		call apsetr (ap, BINSIZE, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_BINSIZE, rval, UN_SSIGMA,
			"width of the sky histogram bin")
		newfit = YES
	    }
	case SCMD_SMOOTH:
	    call gargb (bval)
	    if (nscan () == 1) {
		call printf ("%s = %b\n")
		    call pargstr (KY_SMOOTH)
		    call pargb (itob (apstati (ap, SMOOTH)))
	    } else {
		call apseti (ap, SMOOTH, btoi (bval))
		if (stid > 1)
		    call ap_bparam (out, KY_SMOOTH, bval, UN_SSWITCH,
			"Lucy smooth the histogram")
		newfit = YES
	    }
	case SCMD_RGROW:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_RGROW)
		    call pargr (apstatr (ap, RGROW))
		    call pargstr (UN_SSCALEUNIT)
	    } else {
		call apsetr (ap, RGROW, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_RGROW, rval, UN_SSCALEUNIT,
			"region growing radius")
		newfit = YES
	    }
	case SCMD_SNREJECT:
	    call gargi (ival)
	    if (nscan () == 1) {
		call printf ("%s = %d\n")
		    call pargstr (KY_SNREJECT)
		    call pargi (apstati (ap, SNREJECT))
	    } else {
		call apseti (ap, SNREJECT, ival)
		if (stid > 1)
		    call ap_iparam (out, KY_SNREJECT, ival, UN_SNUMBER,
			"maximum number of rejection cycles")
		newfit = YES
	    }
	case SCMD_SKYVALUE:
	    call gargr (rval)
	    if (nscan () == 1) {
		call printf ("%s = %g\n")
		    call pargstr (KY_SKY_BACKGROUND)
		    call pargr (apstatr (ap, SKY_BACKGROUND))
		    call pargstr (UN_SCOUNTS)
	    } else {
		call apsetr (ap, SKY_BACKGROUND, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_SKY_BACKGROUND, rval,
		        UN_SCOUNTS, "user supplied sky value")
		newfit = YES
	    }
	case SCMD_MKSKY:
	    call gargb (bval)
	    if (nscan () == 1) {
		call printf ("%s = %b\n")
		    call pargstr (KY_MKSKY)
		    call pargb (itob (apstati (ap, MKSKY)))
	    } else {
		call apseti (ap, MKSKY, btoi (bval))
	    }
	default:
	    # do nothing gracefully
	    call printf ("Unrecognized command\7\n")
	}

	call sfree (sp)
end


# AP_SIMCOLON -- Procedure to process fitsky commands which alter parameters
# other than the sky fitting parameters themselves.

procedure ap_simcolon (ap, cmdstr)

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

	# Process the commands.
	ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, MISC)
	switch (ncmd) {
	case ACMD_SHOW:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, SSHOWARGS)
	    switch (ncmd) {
	    case SCMD_DATA:
		call printf ("\n")
		call ap_nshow (ap)
		call printf ("\n")
	    case SCMD_SKY:
		call printf ("\n")
	        call ap_spshow (ap)
		call printf ("\n")
	    default:
		call printf ("\n")
	        call ap_sshow (ap)
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
