include <gset.h>
include "../lib/apphot.h"
include "../lib/fitsky.h"
include "../lib/center.h"
include "../lib/phot.h"
include "../lib/display.h"
include "../lib/noise.h"

# AP_WPCOLON -- Process wphot colon commands.

procedure ap_wpcolon (ap, im, cl, out, stid, ltid, cmdstr, newimage,
	newcenterbuf, newcenter, newskybuf, newsky, newmagbuf, newmag)

pointer	ap				# pointer to the apphot structure
pointer	im				# pointer to the iraf image
int	cl				# coord file descriptor
int	out				# output file descriptor
int	stid				# output file sequence number
int	ltid				# coord file  sequence number
char	cmdstr[ARB]			# command string
int	newimage			# new image ?
int	newcenterbuf, newcenter		# new center buffer ? new center fit ?
int	newskybuf, newsky		# new sky buffer ? new sky fit ?
int	newmagbuf, newmag		# new aperture buffer ? new fit ?

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
	if (strdic (Memc[incmd], Memc[outcmd], SZ_LINE, CCMDS) != 0)
	    call apccolon (ap, out, stid, cmdstr, newcenterbuf, newcenter)
	else if (strdic (Memc[incmd], Memc[outcmd], SZ_LINE, SCMDS) != 0)
	    call apscolon (ap, out, stid, cmdstr, newskybuf, newsky)
	else if (strdic (Memc[incmd], Memc[outcmd], SZ_LINE, PCMDS) != 0)
	    call ap_wmagcolon (ap, out, stid, cmdstr, newmagbuf, newmag)
	else if (strdic (Memc[incmd], Memc[outcmd], SZ_LINE, APCMDS) != 0)
	    call ap_apcolon (ap, im, cl, out, stid, ltid, cmdstr, newimage,
	        newcenterbuf, newcenter, newskybuf, newsky, newmagbuf, newmag)
	else if (strdic (Memc[incmd], Memc[outcmd], SZ_LINE, NCMDS) != 0)
	    call ap_nscolon (ap, im, out, stid, cmdstr, newcenterbuf,
	        newcenter, newskybuf, newsky, newmagbuf, newmag)
	else
	    call ap_himcolon (ap, cmdstr)

	call sfree (sp)
end


# AP_WMAGCOLON -- Procedure to display and edit the photometry parameters.

procedure ap_wmagcolon (ap, out, stid, cmdstr, newmagbuf, newmag)

pointer	ap		# pointer to apphot structure
int	out		# output file descriptor
int	stid		# output number
char	cmdstr[ARB]	# command string
int	newmagbuf	# new aperture buffers
int	newmag		# compute new magnitudes

bool	bval
int	ncmd, stat
pointer	sp, cmd
real	rval
bool	itob()
int	btoi(), strdic(), nscan(), apstati()
real	apstatr()

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

	# Process the command
	ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, PCMDS)
	switch (ncmd) {
	case PCMD_APERTURES:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (Memc[cmd] == EOS) {
	        call apstats (ap, APERTS, Memc[cmd], SZ_LINE)
	        call printf ("%s = %s %s\n")
		    call pargstr (KY_APERTS)
		    call pargstr (Memc[cmd])
		    call pargstr (UN_PSCALEUNIT)
	    } else {
		call apsets (ap, APERTS, Memc[cmd])
		if (stid > 1)
		    call ap_sparam (out, KY_APERTS, Memc[cmd], UN_PSCALEUNIT,
			"list of aperture radii")
		newmag = YES
		newmagbuf = YES
	    }
	case PCMD_ZMAG:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g\n")
		    call pargstr (KY_ZMAG)
		    call pargr (apstatr (ap, ZMAG))
	    } else {
		call apsetr (ap, ZMAG, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_ZMAG, rval, UN_PZMAG,
			"zero point of magnitude scale")
		newmag = YES
	    }
	case PCMD_MKAPERT:
	    call gargb (bval)
	    if (nscan () == 1) {
		call printf ("%s = %b\n")
		    call pargstr (KY_MKAPERT)
		    call pargb (itob (apstati (ap, MKAPERT)))
	    } else {
		call apseti (ap, MKAPERT, btoi (bval))
	    }
	case PCMD_WEIGHTING:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (Memc[cmd] == EOS) {
	        call apstats (ap, PWSTRING, Memc[cmd], SZ_LINE)
	        call printf ("%s = %s\n")
		    call pargstr (KY_PWSTRING)
		    call pargstr (Memc[cmd])
	    } else {
		stat = strdic (Memc[cmd], Memc[cmd], SZ_LINE, PWFUNCS)
		if (stat > 0) {
		    call apseti (ap, PWEIGHTS, stat)
		    call apsets (ap, PWSTRING, Memc[cmd])
	   	    if (stid > 1)
		        call ap_sparam (out, KY_PWSTRING, Memc[cmd],
			    UN_PMODEL, "photometric weighting model")
		    newmagbuf = YES
		    newmag = YES
		}
	    }
	default:
	    call printf ("Unknown or ambiguous colon command\7\n")
	}

	call sfree (sp)
end
