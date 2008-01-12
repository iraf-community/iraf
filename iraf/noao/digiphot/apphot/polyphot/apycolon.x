include <error.h>
include <gset.h>
include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/display.h"
include "../lib/center.h"
include "../lib/fitsky.h"
include "../lib/polyphot.h"

# AP_YCOLON -- Process polyphot task colon commands

procedure ap_ycolon (ap, im, pl, cl, out, stid, ptid, ltid, cmdstr, newimage,
        newcenterbuf, newcenter, newskybuf, newsky, newmagbuf, newmag)

pointer	ap				# pointer to the apphot structure
pointer	im				# pointer to the iraf image
int	pl				# polygon file descriptor
int     cl				# coord file descriptor
int	out				# output file descriptor	
int	stid				# output file number
int	ptid				# polygon file sequence number
int	ltid				# coord file sequence number
char	cmdstr[ARB]			# command string
int	newimage			# new image ?
int	newcenterbuf, newcenter		# new center buffer ?, new center fit ?
int	newskybuf, newsky		# new sky buffer ?, new sky fit ?
int	newmagbuf, newmag		# new aperture buffer ?, new fit ?

pointer	sp, incmd, outcmd
int	strdic()

begin
	# Fetch the command.
	call smark (sp)
	call salloc (incmd, SZ_LINE, TY_CHAR)
	call salloc (outcmd, SZ_LINE, TY_CHAR)

	call sscan (cmdstr)
	call gargwrd (Memc[incmd], SZ_LINE)
	if (Memc[incmd] == EOS) {
	    call sfree (sp)
	    return
	}

	# Set the command to the appropriate routine.
	if (strdic (Memc[incmd], Memc[outcmd], SZ_LINE, CCMDS) != 0)
	    call apccolon (ap, out, stid, cmdstr, newcenterbuf, newcenter)
	else if (strdic (Memc[incmd], Memc[outcmd], SZ_LINE, SCMDS) != 0)
	    call apscolon (ap, out, stid, cmdstr, newskybuf, newsky)
	else if (strdic (Memc[incmd], Memc[outcmd], SZ_LINE, APCMDS) != 0)
	    call ap_apcolon (ap, im, cl, out, stid, ltid, cmdstr, newimage,
	        newcenterbuf, newcenter, newskybuf, newsky, newmagbuf, newmag)
	else if (strdic (Memc[incmd], Memc[outcmd], SZ_LINE, NCMDS) != 0)
	    call ap_nscolon (ap, im, out, stid, cmdstr, newcenterbuf,
	        newcenter, newskybuf, newsky, newmagbuf, newmag)
	else if (strdic (Memc[incmd], Memc[outcmd], SZ_LINE, PYCMDS) != 0)
	    call ap_yycolon (ap, pl, out, stid, ptid, ltid, cmdstr, newmagbuf,
	        newmag)
	else
	    call ap_yimcolon (ap, cmdstr)

	call sfree (sp)
end


# AP_YIMCOLON -- Procedure to process remaining polyphot commands which 

procedure ap_yimcolon (ap, cmdstr)

pointer	ap			# pointer to the apphot structure
char	cmdstr[ARB]		# command string

int	ncmd
pointer	sp, cmd
int	strdic()

begin
	# Fetch the command.
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
	case 1:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, PYSHOWARGS)
	    switch (ncmd) {
	    case 1:
		call printf ("\n")
		call ap_cpshow (ap)
		call printf ("\n")
	    case 2:
		call printf ("\n")
		call ap_spshow (ap)
		call printf ("\n")
	    case 3:
		call printf ("\n")
		call ap_ypshow (ap)
		call printf ("\n")
	    case 4:
		call printf ("\n")
		call ap_nshow (ap)
		call printf ("\n")
	    default:
		call printf ("\n")
		call ap_yshow (ap)
		call printf ("\n")
	    }
	default:
	    call printf ("Unknown or ambiguous colon command\7\n")
	}

	call sfree (sp)
end


# AP_YYCOLON -- Procedure to process polyphot commands

procedure ap_yycolon (ap, pl, out, stid, ptid, ltid, cmdstr, newmagbuf, newmag)

pointer	ap		# pointer to apphot structure
int	pl		# polygon file descriptor
int	out		# output file descriptor
int	stid		# output file sequence number
int	ptid		# polygon file sequence number
int	ltid		# coords file sequence number
char	cmdstr[ARB]	# command string
int	newmagbuf	# new aperture buffers ?
int	newmag		# compute new magnitudes ?

bool	bval
int	ncmd
pointer	sp, cmd, str
real	rval
string	cmds PYCMDS

bool	itob(), streq()
int	apstati(), btoi(), strdic(), nscan(), open()
real	apstatr()
errchk	open, close

begin
	# Allocate the space.
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_FNAME, TY_CHAR)

	call sscan (cmdstr)
	call gargwrd (Memc[cmd], SZ_LINE)
	if (Memc[cmd] == EOS) {
	    call sfree (sp)
	    return
	}

	# Process the colon command.
	ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, cmds)
	switch (ncmd) {

	case PLCMD_ZMAG:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g\n")
		    call pargstr (KY_PYZMAG)
		    call pargr (apstatr (ap, PYZMAG))
	    } else {
		call apsetr (ap, PYZMAG, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_PYZMAG, rval, UN_PYZMAG,
			"zero point of magnitude scale")
		newmag = YES
	    }

	case PLCMD_MKPOLYGON:
	    call gargb (bval)
	    if (nscan() == 1) {
		call printf ("%s = %g\n")
		    call pargstr (KY_MKPOLYGON)
		    call pargb (itob (apstati (ap, MKPOLYGON)))
	    } else {
		call apseti (ap, MKPOLYGON, btoi (bval))
	    }

	case PLCMD_POLYGONS:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    call apstats (ap, PYNAME, Memc[str], SZ_FNAME)
	    if (Memc[cmd] == EOS || streq (Memc[cmd], Memc[str])) {
		call printf ("%s = %s\n")
		    call pargstr (KY_PYNAME)
		    call pargstr (Memc[str])
	    } else {
		if (pl != NULL) {
		    call close (pl)
		    pl = NULL
		}
		iferr {
		    pl = open (Memc[cmd], READ_ONLY, TEXT_FILE)
		} then {
		    pl = NULL
		    call erract (EA_WARN)
		    call apsets (ap, PYNAME, "")
		    call apsets (ap, PYROOT, "")
		    ptid = 0
		    ltid = 0
		} else {
		    call apsets (ap, PYNAME, Memc[cmd])
		    call apfroot (Memc[cmd], Memc[str], SZ_FNAME)
		    call apsets (ap, PYROOT, Memc[str])
		    ptid = 0
		    ltid = 0
		}
	    }

	default:
	    call printf ("Unknown or ambiguous colon command\n")
	}

	call sfree (sp)
end
