include <error.h>
include "../lib/apphot.h"
include "../lib/noise.h"

# AP_NSCOLON -- Procedure to process colon commands for setting
# noise fitting parameters.

procedure ap_nscolon (ap, im, cl, out, stid, ltid, cmdstr, newcenterbuf,
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
int	ncmd, stat, ip, nchars
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
	ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, NCMDS)
	switch (ncmd) {

	case NCMD_NOISE:
	    call gargwrd (Memc[cmd], SZ_LINE)
	    if (Memc[cmd] == EOS) {
		call apstats (ap, NSTRING, Memc[str], SZ_FNAME)
		call printf ("%s = %s %s\n")
		    call pargstr (KY_NSTRING)
		    call pargstr (Memc[str])
		    call pargstr (UN_NSTRING)
	    } else {
		stat = strdic (Memc[cmd], Memc[cmd], SZ_LINE, NFUNCS)
		if (stat > 0) {
		    call apseti (ap, NOISEFUNCTION, stat)
		    call apsets (ap, NSTRING, Memc[cmd])
		    if (stid > 1)
		        call ap_sparam (out, KY_NSTRING, Memc[cmd], UN_NSTRING,
		            "noise model")
		    newcenterbuf = YES; newcenter = YES
		    newsky = YES; newfit = YES
		}
	    }

	case NCMD_CTHRESHOLD:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_CTHRESHOLD)
		    call pargr (apstatr (ap, CTHRESHOLD))
		    call pargstr (UN_CTHRESHOLD)
	    } else {
		call apsetr (ap, CTHRESHOLD, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_CTHRESHOLD, rval, UN_CTHRESHOLD,
			"threshold for centering")
		newcenterbuf = YES; newcenter = YES
		newsky = YES; newfit = YES
	    }

	case NCMD_SIGMA:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_SKYSIGMA)
		    call pargr (apstatr (ap, SKYSIGMA))
		    call pargstr (UN_SKYSIGMA)
	    } else {
		call apsetr (ap, SKYSIGMA, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_SKYSIGMA, rval, UN_SKYSIGMA,
			"standard deviation of 1 pixel")
		newcenterbuf = YES; newcenter = YES
		newsky = YES
		newfit = YES
	    }

	case NCMD_EPADU:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_EPADU)
		    call pargr (apstatr (ap, EPADU))
		    call pargstr (UN_EPADU)
	    } else {
		call apsetr (ap, EPADU, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_EPADU, rval, UN_EPADU,
			"photons per adu")
		newcenterbuf = YES; newcenter = YES
		newsky = YES
		newfit = YES
	    }

	case NCMD_GAIN:
	    call gargstr (Memc[cmd], SZ_LINE)
	    if (Memc[cmd]  == EOS) {
		call apstats (ap, GAIN, Memc[str], SZ_LINE)
		call printf ("%s = %s\n")
		    call pargstr (KY_GAIN)
		    call pargstr (Memc[str])
	    } else {
	        nchars = ctowrd (Memc[cmd], ip, Memc[str], SZ_LINE)
		call apsets (ap, GAIN, Memc[str])
		if (im != NULL)
		    call ap_padu (im, ap)
		if (stid > 1)
		    call ap_sparam  (out, KY_GAIN, Memc[str], UN_GAIN,
			"gain keyword")
		newcenterbuf = YES; newcenter = YES
		newsky = YES
		newfit = YES
	    }

	case NCMD_CCDREAD:
	    call gargstr (Memc[cmd], SZ_LINE)
	    if (Memc[cmd] == EOS) {
		call apstats (ap, CCDREAD, Memc[str], SZ_LINE)
		call printf ("%s = %s\n")
		    call pargstr (KY_CCDREAD)
		    call pargstr (Memc[str])
	    } else {
	        nchars = ctowrd (Memc[cmd], ip, Memc[str], SZ_LINE)
		call apsets (ap, CCDREAD, Memc[str])
		if (im != NULL)
		    call ap_rdnoise (im, ap)
		if (stid > 1)
		    call ap_sparam  (out, KY_CCDREAD, Memc[str], UN_CCDREAD,
			"read noise keyword")
		newfit = YES
	    }

	case NCMD_READNOISE:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_READNOISE)
		    call pargr (apstatr (ap, READNOISE))
		    call pargstr (UN_READNOISE)
	    } else {
		call apsetr (ap, READNOISE, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_READNOISE, rval, UN_READNOISE,
			"readout noise")
		newfit = YES
	    }

	case NCMD_FWHMPSF:
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

	case NCMD_SCALE:
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

	case NCMD_EMISSION:
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

	case NCMD_EXPOSURE:
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

	case NCMD_ITIME:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_ITIME)
		    call pargr (apstatr (ap, ITIME))
		    call pargstr (UN_ITIME)
	    } else {
		call apsetr (ap, ITIME, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_ITIME, rval, UN_ITIME,
			"exposure time")
		newfit = YES
	    }

	case NCMD_DATAMIN:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_DATAMIN)
		    call pargr (apstatr (ap, DATAMIN))
		    call pargstr (UN_DATAMIN)
	    } else {
		call printf ("Datamin parameter not currently implemented.\n")
		#call apsetr (ap, DATAMIN, rval)
		#if (stid > 1)
		    #call ap_rparam (out, KY_DATAMIN, rval, UN_DATAMIN,
			#"minimim good data value")
		#newfit = YES
	    }

	case NCMD_DATAMAX:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_DATAMAX)
		    call pargr (apstatr (ap, DATAMAX))
		    call pargstr (UN_DATAMAX)
	    } else {
		call printf ("Datamax parameter not currently implemented.\n")
		#call apsetr (ap, DATAMAX, rval)
		#if (stid > 1)
		    #call ap_rparam (out, KY_DATAMAX, rval, UN_DATAMAX,
			#"maximum good data value")
		#newfit = YES
	    }

	case NCMD_THRESHOLD:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("%s = %g %s\n")
		    call pargstr (KY_THRESHOLD)
		    call pargr (apstatr (ap, THRESHOLD))
		    call pargstr (UN_THRESHOLD)
	    } else {
		call apsetr (ap, THRESHOLD, rval)
		if (stid > 1)
		    call ap_rparam (out, KY_THRESHOLD, rval, UN_THRESHOLD,
			"threshold for detection")
		newfit = YES
	    }

	case NCMD_IMAGE:
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
		    newcenterbuf = YES; newcenter = YES
		    newskybuf = YES; newsky = YES
		    newbuf = YES; newfit = YES
		}
	    }

	case NCMD_COORDS:
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

	case NCMD_OUTPUT:
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
