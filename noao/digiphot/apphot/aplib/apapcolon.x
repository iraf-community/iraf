include <error.h>
include "../lib/apphot.h"

# AP_APCOLON -- Procedure to process colon commands for setting
# noise fitting parameters.

procedure ap_apcolon (ap, im, cl, out, stid, ltid, cmdstr, newcenterbuf,
    newcenter, newskybuf, newsky, newbuf, newfit)

pointer	ap			# pointer to the apphot structure
pointer	im			# pointer to the iraf image
int	cl			# coordinate file descriptor
pointer	out			# output file descriptor
int	stid			# output file sequence number
int	ltid			# coord file sequence number
char	cmdstr[ARB]		# command string
int	newcenterbuf, newcenter	# change centering parameters
int	newskybuf, newsky	# change sky fitting parameters
int	newbuf, newfit		# change magnitude parameters

bool	bval
int	ncmd, ip, nchars
pointer	sp, cmd, str
real	rval

bool	streq(), itob()
int	strdic(), nscan(), btoi(), apstati(), ctowrd(), open()
pointer	immap()
real	apstatr()

errchk	immmap, open

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Get the command.
	ip = 1
	call sscan (cmdstr)
	call gargwrd (Memc[cmd], SZ_LINE)
	if (Memc[cmd] == EOS) {
	    call sfree (sp)
	    return
	}

	# Process the command.
	ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, APCMDS)
	switch (ncmd) {

	case APCMD_FWHMPSF:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_FWHMPSF)
		    call pargr (apstatr (ap, FWHMPSF))
		    call pargstr (UN_FWHMPSF)
	    } else {
		call apsetr (ap, FWHMPSF, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_FWHMPSF, rval, UN_FWHMPSF,
			"full width half max of psf")
		newcenterbuf = YES; newcenter = YES
		newskybuf = YES; newsky = YES
		newbuf = YES; newfit = YES
	    }

	case APCMD_SCALE:
	    call gargr (rval)
	    if (nscan () == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_SCALE)
		    call pargr (1.0 / apstatr (ap, SCALE))
		    call pargstr (UN_SCALE)
	    } else if (rval > 0.0) {
		call apsetr (ap, SCALE, (1.0 / rval))
		if (stid > 1)
		    call ap_rparam (out, KY_SCALE, (1.0 / rval), UN_SCALE,
			"scale in units / pixel")
		newcenterbuf = YES; newcenter = YES
		newskybuf = YES; newsky = YES
		newbuf = YES; newfit = YES
	    }

	case APCMD_EMISSION:
	    call gargb (bval)
	    if (nscan() == 1) {
		call printf ("%s = %b\n")
		    call pargstr (KY_POSITIVE)
		    call pargb (itob (apstati (ap, POSITIVE)))
	    } else {
		call apseti (ap, POSITIVE, btoi (bval))
		if (stid > 1)
		    call ap_bparam (out, KY_POSITIVE, bval, UN_POSITIVE,
			"emission feature")
		newcenterbuf = YES; newcenter = YES
		newskybuf = YES; newsky = YES
		newbuf = YES; newfit = YES
	    }

	case APCMD_FILTER:
	    call gargstr (Memc[cmd], SZ_LINE)
	    if (Memc[cmd] == EOS) {
		call apstats (ap, FILTER, Memc[str], SZ_LINE)
		call printf ("%s = %s\n")
		    call pargstr (KY_FILTER)
		    call pargstr (Memc[str])
	    } else {
	        nchars = ctowrd (Memc[cmd], ip, Memc[str], SZ_LINE)
		call apsets (ap, FILTER, Memc[str])
		if (im != NULL)
		    call ap_filter (im, ap)
		if (stid > 1)
		    call ap_sparam  (out, KY_FILTER, Memc[str], UN_FILTER,
			"filter keyword")
	    }

	case APCMD_FILTERID:
	    call gargstr (Memc[cmd], SZ_LINE)
	    if (Memc[cmd] == EOS) {
		call apstats (ap, FILTERID, Memc[str], SZ_LINE)
		call printf ("%s = %s\n")
		    call pargstr (KY_FILTERID)
		    call pargstr (Memc[str])
	    } else {
	        nchars = ctowrd (Memc[cmd], ip, Memc[str], SZ_LINE)
		call apsets (ap, FILTERID, Memc[str])
		#if (stid > 1)
		    #call ap_sparam  (out, KY_FILTERID, Memc[str], UN_FILTERID,
			#"filter")
	    }

	case APCMD_AIRMASS:
	    call gargstr (Memc[cmd], SZ_LINE)
	    if (Memc[cmd] == EOS) {
		call apstats (ap, AIRMASS, Memc[str], SZ_LINE)
		call printf ("%s = %s\n")
		    call pargstr (KY_AIRMASS)
		    call pargstr (Memc[str])
	    } else {
	        nchars = ctowrd (Memc[cmd], ip, Memc[str], SZ_LINE)
		call apsets (ap, AIRMASS, Memc[str])
		if (im != NULL)
		    call ap_airmass (im, ap)
		if (stid > 1)
		    call ap_sparam  (out, KY_AIRMASS, Memc[str], UN_AIRMASS,
			"airmass keyword")
	    }

	case APCMD_XAIRMASS:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_XAIRMASS)
		    call pargr (apstatr (ap, XAIRMASS))
		    call pargstr (UN_XAIRMASS)
	    } else {
		call apsetr (ap, XAIRMASS, rval)
		#if (stid > 1)
		    #call ap_rparam (out, KY_XAIRMASS, rval, UN_XAIRMASS,
			#"airmass")
	    }

	case APCMD_EXPOSURE:
	    call gargstr (Memc[cmd], SZ_LINE)
	    if (Memc[cmd] == EOS) {
		call apstats (ap, EXPOSURE, Memc[str], SZ_LINE)
		call printf ("%s = %s\n")
		    call pargstr (KY_EXPOSURE)
		    call pargstr (Memc[str])
	    } else {
	        nchars = ctowrd (Memc[cmd], ip, Memc[str], SZ_LINE)
		call apsets (ap, EXPOSURE, Memc[str])
		if (im != NULL)
		    call ap_itime (im, ap)
		if (stid > 1)
		    call ap_sparam  (out, KY_EXPOSURE, Memc[str], UN_EXPOSURE,
			"exposure time keyword")
		newfit = YES
	    }

	case APCMD_ITIME:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_ITIME)
		    call pargr (apstatr (ap, ITIME))
		    call pargstr (UN_ITIME)
	    } else {
		call apsetr (ap, ITIME, rval)
		#if (stid > 1)
		    #call ap_rparam (out, KY_ITIME, rval, UN_ITIME,
			#"exposure time")
		newfit = YES
	    }

	case APCMD_DATAMIN:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_DATAMIN)
		    call pargr (apstatr (ap, DATAMIN))
		    call pargstr (UN_DATAMIN)
	    } else {
		call apsetr (ap, DATAMIN, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_DATAMIN, rval, UN_DATAMIN,
			"minimim good data value")
		newcenterbuf = YES; newcenter = YES
		newskybuf = YES; newsky = YES
		newbuf = YES; newfit = YES
	    }

	case APCMD_DATAMAX:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_DATAMAX)
		    call pargr (apstatr (ap, DATAMAX))
		    call pargstr (UN_DATAMAX)
	    } else {
		call apsetr (ap, DATAMAX, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_DATAMAX, rval, UN_DATAMAX,
			"maximum good data value")
		newcenterbuf = YES; newcenter = YES
		newskybuf = YES; newsky = YES
		newbuf = YES; newfit = YES
	    }

	case APCMD_IMAGE:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    call apstats (ap, IMNAME, Memc[str], SZ_FNAME)
	    if (Memc[cmd] == EOS || streq (memc[cmd], Memc[str])) {
		call printf ("%s: %s\n")
		    call pargstr (KY_IMNAME)
		    call pargstr (Memc[str])
	    } else {
		if (im != NULL) {
		    call imunmap (im)
		    im = NULL
		}
		iferr {
		    im = immap (Memc[cmd], READ_ONLY, 0)
		} then {
		    call erract (EA_WARN)
		    im = immap (Memc[str], READ_ONLY, 0)
		} else {
		    call apsets (ap, IMNAME, Memc[cmd])
		    call ap_itime (im, ap)
		    call ap_padu (im, ap)
		    call ap_rdnoise (im, ap)
		    call ap_filter (im, ap)
		    call ap_airmass (im, ap)
		    newcenterbuf = YES; newcenter = YES
		    newskybuf = YES; newsky = YES
		    newbuf = YES; newfit = YES
		}
	    }

	case APCMD_COORDS:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    call apstats (ap, CLNAME, Memc[str], SZ_FNAME)
	    if (Memc[cmd] == EOS || streq (Memc[cmd], Memc[str])) {
		call printf ("%s:  %s\n")
		    call pargstr (KY_CLNAME)
		    call pargstr (Memc[str])
	    } else {
		if (cl != NULL) {
		    call close( cl)
		    cl = NULL
		}
		iferr {
		    cl = open (Memc[cmd], READ_ONLY, TEXT_FILE)
		} then {
		    cl = NULL
		    call erract (EA_WARN)
		    call apsets (ap, CLNAME, "")
		    call printf ("Coordinate file is undefined.\n")
		} else {
		    call apsets (ap, CLNAME, Memc[cmd])
		    ltid = 0
		}
	    }

	case APCMD_OUTPUT:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    call apstats (ap, OUTNAME, Memc[str], SZ_FNAME)
	    if (Memc[cmd] == EOS || streq (Memc[cmd], Memc[str])) {
		call printf ("%s:  %s\n")
		    call pargstr (KY_OUTNAME)
		    call pargstr (Memc[str])
	    } else {
		if (out != NULL) {
		    call close (out)
		    out = NULL
		    if (stid <= 1)
		        call delete (Memc[str])
		}
		iferr {
		    out = open (Memc[cmd], NEW_FILE, TEXT_FILE)
		} then {
		    call erract (EA_WARN)
		    call printf ("Reopening output file: %s\n")
			call pargstr (Memc[str])
		    if (Memc[str] != EOS)
		        out  = open (Memc[str], APPEND, TEXT_FILE)
		    else
			out = NULL
		} else {
		    call apsets (ap, OUTNAME, Memc[cmd])
		    stid = 1
		}
	    }
	default:
	    call printf ("Unknown or ambiguous colon command\7\n")
	}

	call sfree (sp)
end
