include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/display.h"
include "../lib/center.h"
include "../lib/fitsky.h"
include "../lib/phot.h"
include "../lib/radprof.h"

# AP_RPCOLON -- Show/set radprof parameters.

procedure ap_rpcolon (ap, im, cl, out, stid, ltid, cmdstr, newcenterbuf,
	newcenter, newskybuf, newsky, newbuf, newfit)

pointer	ap				# pointer to the apphot structure
pointer	im				# pointer to iraf image
int	cl				# coord file descriptor
int	out				# output file descriptor
int	stid				# output file sequence number
int	ltid				# coord list sequence number
char	cmdstr[ARB]			# command string
int	newcenterbuf, newcenter		# new sky fit
int	newskybuf, newsky		# new sky buffer
int	newbuf, newfit			# new aperture

pointer	sp, incmd, outcmd
int	strdic()

begin
	call smark (sp)
	call salloc (incmd, SZ_LINE, TY_CHAR)
	call salloc (outcmd, SZ_LINE, TY_CHAR)

	# Get the commands.
	call sscan (cmdstr)
	call gargwrd (Memc[incmd], SZ_LINE)
	if (Memc[incmd] == EOS) {
	    call sfree (sp)
	    return
	}

	# Process the command.
	if (strdic (Memc[incmd], Memc[outcmd], SZ_LINE, CCMDS) != 0)
	    call apccolon (ap, out, stid, cmdstr, newcenterbuf, newcenter)
	else if (strdic (Memc[incmd], Memc[outcmd], SZ_LINE, SCMDS) != 0)
	    call apscolon (ap, out, stid, cmdstr, newskybuf, newsky)
	else if (strdic (Memc[incmd], Memc[outcmd], SZ_LINE, RPCMDS) != 0)
	    call ap_profcolon (ap, out, stid, cmdstr, newbuf, newfit)
	else if (strdic (Memc[incmd], Memc[outcmd], SZ_LINE, PCMDS) != 0)
	    call apmagcolon (ap, out, stid, cmdstr, newbuf, newfit)
	else if (strdic (Memc[incmd], Memc[outcmd], SZ_LINE, APCMDS) != 0)
	    call ap_apcolon (ap, im, cl, out, stid, ltid, cmdstr, newcenterbuf,
	        newcenter, newskybuf, newsky, newbuf, newfit)
	else if (strdic (Memc[incmd], Memc[outcmd], SZ_LINE, NCMDS) != 0)
	    call ap_nscolon (ap, im, out, stid, cmdstr, newcenterbuf,
	        newcenter, newskybuf, newsky, newbuf, newfit)
	else
	    call ap_rpimcolon (ap, out, stid, cmdstr, newcenterbuf, newcenter,
	        newskybuf, newsky, newbuf, newfit)

	call sfree (sp)
end


# AP_RPIMCOLON -- Show/set quantities which are not radprof parameters.

procedure ap_rpimcolon (ap, out, stid, cmdstr, newcenterbuf, newcenter,
	newskybuf, newsky, newbuf, newfit)

pointer	ap			# pointer to the apphot structure
int	out			# output file descriptor
int	stid			# stellar id of output file
char	cmdstr[ARB]		# command string
int	newcenterbuf, newcenter	# centering parameters
int	newskybuf, newsky	# skyfitting parameters
int	newbuf, newfit		# magnitude parameters

bool	bval
int	ncmd
pointer	sp, cmd
bool	itob()
int	apstati(), strdic(), nscan(), btoi()

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
	    ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, RPSHOWARGS)
	    switch (ncmd) {
	    case RCMD_CENTER:
		call printf ("\n")
		call ap_cpshow (ap)
		call printf ("\n")
	    case RCMD_SKY:
		call printf ("\n")
		call ap_spshow (ap)
		call printf ("\n")
	    case RCMD_PHOT:
		call printf ("\n")
		call ap_mpshow (ap)
		call printf ("\n")
	    case RCMD_FIT:
		call printf ("\n")
		call ap_rppshow (ap)
		call printf ("\n")
	    case RCMD_DATA:
		call printf ("\n")
		call ap_nshow (ap)
		call printf ("\n")
	    default:
		call printf ("\n")
		call ap_rprofshow (ap)
		call printf ("\n")
	    }
	case ACMD_RADPLOTS:
	    call gargb (bval)
	    if (nscan() == 1) {
		call printf ("%s = %b\n")
		    call pargstr (KY_RADPLOTS)
		    call pargb (itob (apstati (ap, RADPLOTS)))
	    } else
		call apseti (ap, RADPLOTS, btoi (bval))
	default:
	    call printf ("Unknown or ambiguous colon command\7\n")
	}

	call sfree (sp)
end
