include <error.h>
include "../lib/noise.h"

# AP_NSCOLON -- Procedure to process colon commands for setting
# noise fitting parameters.

procedure ap_nscolon (ap, im, out, stid, cmdstr, newcenterbuf,
    newcenter, newskybuf, newsky, newbuf, newfit)

pointer	ap			# pointer to the apphot structure
pointer	im			# pointer to the iraf image
pointer	out			# output file descriptor
int	stid			# output file sequence number
char	cmdstr[ARB]		# command string
int	newcenterbuf, newcenter	# change centering parameters
int	newskybuf, newsky	# change sky fitting parameters
int	newbuf, newfit		# change magnitude parameters

int	ncmd, stat, ip, nchars
pointer	sp, cmd, str
real	rval

int	strdic(), nscan(), ctowrd()
real	apstatr()

errchk	immmap

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

	default:
	    call printf ("Unknown or ambiguous colon command\7\n")
	}

	call sfree (sp)
end
