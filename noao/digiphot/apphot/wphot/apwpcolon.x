include <gset.h>
include "../lib/apphot.h"
include "../lib/fitsky.h"
include "../lib/center.h"
include "../lib/phot.h"
include "../lib/display.h"
include "../lib/noise.h"

# AP_WPCOLON -- Process wphot colon commands.

procedure ap_wpcolon (ap, im, cl, out, stid, ltid, cmdstr, newcenterbuf,
	newcenter, newskybuf, newsky, newmagbuf, newmag)

pointer	ap				# pointer to the apphot structure
pointer	im				# pointer to the iraf image
int	cl				# coord file descriptor
int	out				# output file descriptor
int	stid				# output file sequence number
int	ltid				# coord file  sequence number
char	cmdstr[ARB]			# command string
int	newcenterbuf, newcenter		# new sky fit
int	newskybuf, newsky		# new sky buffer
int	newmagbuf, newmag		# new aperture

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
	    call ap_apcolon (ap, im, cl, out, stid, ltid, cmdstr, newcenterbuf,
	        newcenter, newskybuf, newsky, newmagbuf, newmag)
	else if (strdic (Memc[incmd], Memc[outcmd], SZ_LINE, NCMDS) != 0)
	    call ap_nscolon (ap, im, out, stid, cmdstr, newcenterbuf,
	        newcenter, newskybuf, newsky, newmagbuf, newmag)
	else
	    call aphimcolon (ap, out, stid, cmdstr, newcenterbuf, newcenter,
	        newskybuf, newsky, newmagbuf, newmag)

	call sfree (sp)
end
