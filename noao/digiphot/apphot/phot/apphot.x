include <ctype.h>
include <gset.h>
include "../lib/phot.h"
include "../lib/apphot.h"
include "../lib/display.h"
include "../lib/center.h"
include "../lib/fitsky.h"

define	HELPFILE	"apphot$phot/phot.key"

# APPHOT -- Procedure to compute magnitudes for a list of objects

int procedure apphot (ap, im, cl, sd, gd, mgd, id, out, stid, interactive)

pointer ap			# pointer to apphot structure
pointer	im			# pointer to IRAF image
int	cl			# starlist file descriptor
int	sd			# the sky file descriptor
pointer	gd			# pointer to graphcis descriptor
pointer	mgd			# pointer to the metacode file
pointer	id			# pointer to image display stream
int	out			# output file descriptor
int	stid			# output file sequence number
int	interactive		# interactive mode

int	wcs, key, ltid, newlist
int	newskybuf, newsky, newcenterbuf, newcenter, newmagbuf, newmag
int	colonkey, prev_num, req_num, ip, cier, sier, pier, oid
pointer	sp, cmd
real	wx, wy, xlist, ylist

bool	fp_equalr()
int	clgcur(), apfitsky(), aprefitsky(), apfitcenter(), aprefitcenter()
int	apmag(), apremag(), apgscur(), ctoi(), apstati(), apgqverify()
int	apgtverify(), apnew()
real	apstatr()

begin
	# Initialize.
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Initialize cursor command.
	key = ' '
	Memc[cmd] = EOS

	# Initialize the fitting parameters.
	newcenterbuf = YES
	newcenter = YES
	newskybuf = YES
	newsky = YES
	newmagbuf = YES
	newmag = YES
	cier = AP_OK
	sier = AP_OK
	pier = AP_OK

	# Intialize the sequencing.
	newlist = NO
	ltid = 0

	# Loop over the coordinate file.
	while (clgcur ("commands", wx, wy, wcs, key, Memc[cmd], SZ_LINE) !=
	    EOF) {

	    # Store the current cursor coordinates.
	    call apsetr (ap, CWX, wx)
	    call apsetr (ap, CWY, wy)

	    # Test to see if the cursor moved.
	    if (apnew (ap, wx, wy, xlist, ylist, newlist) == YES) {
		newcenterbuf = YES
		newcenter = YES
		newskybuf = YES
		newsky = YES
		newmagbuf = YES
		newmag = YES
	    }

	    # Store previous cursor coordinates.
	    call apsetr (ap, WX, apstatr (ap, CWX))
	    call apsetr (ap, WY, apstatr (ap, CWY))

	    # Loop over the colon keystroke commands.
	    switch (key) {

	    # Quit.
	    case 'q':
		if (interactive == YES) {
		    if (apgqverify ("phot", ap, key) == YES) {
			call sfree (sp)
			return (apgtverify (key))
		    }
		} else {
		    call sfree (sp)
		    return (NO)
		}

	    # Print out error messages.
	    case 'e':
		if (interactive == YES)
		    call ap_perrors (ap, cier, sier, pier)

	    # Print out the help page(s).
	    case '?':
		if (id != NULL)
		    call gpagefile (id, HELPFILE, "")
		else if (interactive == YES)
		    call pagefile (HELPFILE, "[space=morehelp,q=quit,?=help]")

	    # Plot a centered stellar radial profile.
	    case 'd':
		if (interactive == YES) {
		    call ap_qrad (ap, im, wx, wy, gd)
		    newmagbuf = YES; newmag = YES
		    newcenterbuf = YES; newcenter = YES
		    newskybuf = YES; newsky = YES
		}

	    # Rewind the list.
	    case 'r':
		if (cl != NULL) {
		    call seek (cl, BOFL)
		    ltid = 0
		} else if (interactive == YES)
		    call printf ("No coordinate list\7\n")

	    # Process the remainder of the list.
	    case 'l':
		if (cl != NULL) {
		    ltid = ltid + 1
		    oid = stid
		    call apbphot (ap, im, cl, sd, out, stid, ltid, gd, mgd, id,
		        YES)
		    ltid = ltid + stid - oid + 1
		} else if (interactive == YES)
		    call printf ("No coordinate list\7\n")

	    # Process apphot colon commands.
	    case ':':
		for (ip = 1; IS_WHITE(Memc[cmd+ip-1]); ip = ip + 1)
		    ;
		colonkey = Memc[cmd+ip-1]
		switch (colonkey) {
		case 'm':

		    # Show/set a phot parameters.
		    if (Memc[cmd+ip] != EOS && Memc[cmd+ip] != ' ') {
		        call apphotcolon (ap, im, cl, out, stid, ltid,
			    Memc[cmd], newcenterbuf, newcenter, newskybuf,
			    newsky, newmagbuf, newmag)

		    # Fetch the nth object from the list.
		    } else if (cl != NULL) {
			ip = ip + 1
			prev_num = ltid
			if (ctoi (Memc[cmd], ip, req_num) <= 0)
			    req_num = ltid + 1
		        if (apgscur (cl, id, xlist, ylist, prev_num,
			    req_num, ltid) != EOF) {
			    newcenterbuf = YES; newcenter = YES
			    newskybuf = YES; newsky = YES
			    newmagbuf = YES; newmag = YES
		    	    newlist = YES
			} else if (interactive == YES)
			    call printf (
			    "End of coordinate list, use r key to rewind\7\n")
		    } else if (interactive == YES)
		        call printf ("No coordinate list\7\n")

		case 'n':

		    # Show/set a phot parameter.
		    if (Memc[cmd+ip] != EOS && Memc[cmd+ip] != ' ') {
		        call apphotcolon (ap, im, cl, out, stid, ltid,
			    Memc[cmd], newcenterbuf, newcenter, newskybuf,
			    newsky, newmagbuf, newmag)

		    # Measure the nth object in the list.
		    } else if (cl != NULL) {
			ip = ip + 1
			prev_num = ltid
			if (ctoi (Memc[cmd], ip, req_num) <= 0)
			    req_num = ltid + 1

		        if (apgscur (cl, id, xlist, ylist, prev_num,
			    req_num, ltid) != EOF) {

		    	    newlist = YES
		    	    cier = apfitcenter (ap, im, xlist, ylist)
		    	    sier = apfitsky (ap, im, apstatr (ap, XCENTER),
			        apstatr (ap, YCENTER), sd, gd)
		    	    pier = apmag (ap, im, apstatr (ap, XCENTER),
			        apstatr (ap, YCENTER), apstati (ap, POSITIVE),
				apstatr (ap, SKY_MODE), apstatr (ap, SKY_SIGMA),
				apstati (ap, NSKY))

			    call apmark (ap, id, apstati (ap, MKCENTER),
				apstati (ap, MKSKY), apstati (ap, MKAPERT))
			    call appplot (ap, im, stid, cier, sier, pier, gd,
				apstati (ap, RADPLOTS))
			    if (interactive == YES)
		    	        call ap_qpmag (ap, cier, sier, pier)

		    	    if (stid == 1)
				call ap_param (ap, out, "phot")
		            call ap_pmag (ap, out, stid, ltid, cier, sier, pier)
			    call appplot (ap, im, stid, cier, sier, pier, mgd,
			        YES)
		    	    stid = stid + 1
		    	    newcenterbuf = NO
			    newcenter = NO
		    	    newskybuf = NO
			    newsky = NO
		    	    newmagbuf = NO
			    newmag = NO

			} else if (interactive == YES)
			    call printf (
			    "End of coordinate list, use r key to rewind\7\n")

		    } else if (interactive == YES)
		        call printf ("No coordinate list\7\n")

		# Show/set a phot parameter.
		default:
		    call apphotcolon (ap, im, cl, out, stid, ltid, Memc[cmd],
		        newcenterbuf, newcenter, newskybuf, newsky, newmagbuf,
			newmag)
		}

	    # Save the current parameters in the pset files.
	    case 'w':
		call ap_ppars (ap)

	    # Setup phot parameters interactively.
	    case 'i':
		if (interactive == YES) {
		    call ap_radsetup (ap, im, wx, wy, gd, out, stid)
		    newmagbuf = YES
		    newmag = YES
		    newcenterbuf = YES
		    newcenter = YES
		    newskybuf = YES
		    newsky = YES
		}

	    # Verify the critical PHOT parameters.
	    case 'v':
		call ap_pconfirm (ap, out, stid)

	    # Fit the center around the cursor position.
	    case 'c':
		if (newcenterbuf == YES)
		    cier = apfitcenter (ap, im, wx, wy)
		else if (newcenter == YES)
		    cier = aprefitcenter (ap, cier)
		call apmark (ap, id, apstati (ap, MKCENTER), NO, NO)
		call apcplot (ap, stid, cier, gd, apstati (ap, RADPLOTS))
		if (interactive == YES)
		    call ap_qcenter (ap, cier)
		newcenterbuf = NO
		newcenter = NO

	    # Fit the sky around the cursor position.
	    case 't':
	        if (newskybuf == YES || ! fp_equalr (wx,
		    apstatr (ap, SXCUR)) || ! fp_equalr (wy, apstatr (ap,
		    SYCUR)))
		    sier = apfitsky (ap, im, wx, wy, sd, gd)
	        else if (newsky == YES)
		    sier = aprefitsky (ap, gd)
		call apmark (ap, id, NO, apstati (ap, MKSKY), NO)
		call apsplot (ap, stid, sier, gd, apstati (ap, RADPLOTS))
		if (interactive == YES)
		    call ap_qspsky (ap, sier)
		newskybuf = NO
		newsky = NO

	     # Fit the sky around the current center position. 
	     case 's':
	        if (newskybuf == YES || ! fp_equalr (apstatr (ap, XCENTER),
		    apstatr (ap, SXCUR)) || ! fp_equalr (apstatr (ap, SYCUR),
		    apstatr (ap, YCENTER)))
		    sier = apfitsky (ap, im, apstatr (ap, XCENTER),
		        apstatr (ap, YCENTER), sd, gd)
		else if (newsky == YES)
		    sier = aprefitsky (ap, gd)
		call apmark (ap, id, NO, apstati (ap, MKSKY), NO)
		call apsplot (ap, stid, sier, gd, apstati (ap, RADPLOTS))
		if (interactive == YES)
		    call ap_qspsky (ap, sier)
		newskybuf = NO
		newsky = NO

	    # Compute magnitudes around the cursor position using the current
	    # sky.
	    case 'p':
	        if (newmagbuf == YES || ! fp_equalr (wx,
		    apstatr (ap, PXCUR)) || ! fp_equalr (apstatr (ap,
		    PYCUR), wy))
		    pier = apmag (ap, im, wx, wy, apstati (ap, POSITIVE),
		    apstatr (ap, SKY_MODE), apstatr (ap, SKY_SIGMA),
		    apstati (ap, NSKY))
		else
		    pier = apremag (ap, apstati (ap, POSITIVE), apstatr (ap,
		        SKY_MODE), apstatr (ap, SKY_SIGMA), apstati (ap, NSKY))
		call apmark (ap, id, NO, NO, apstati (ap, MKAPERT))
		if (interactive == YES)
		    call ap_qpmag (ap, cier, sier, pier)
		newmagbuf = NO
		newmag = NO

	    # Compute the center, sky, and magnitudes and save the results.
	    case 'f', ' ':
		if (newcenterbuf == YES)
		    cier = apfitcenter (ap, im, wx, wy)
		else if (newcenter == YES)
		    cier = aprefitcenter (ap, cier)
		if (newskybuf == YES || ! fp_equalr (apstatr (ap, XCENTER),
		    apstatr (ap, SXCUR)) || ! fp_equalr (apstatr (ap, YCENTER),
		    apstatr (ap, SYCUR)))
		    sier = apfitsky (ap, im, apstatr (ap, XCENTER), apstatr (ap,
		        YCENTER), sd, gd)
		else if (newsky == YES)
		    sier = aprefitsky (ap, gd)
		if (newmagbuf == YES || ! fp_equalr (apstatr (ap, XCENTER),
		    apstatr (ap, PXCUR)) || ! fp_equalr (apstatr (ap, YCENTER),
		    apstatr (ap, PYCUR)))
		    pier = apmag (ap, im, apstatr (ap, XCENTER),
		        apstatr (ap, YCENTER), apstati (ap, POSITIVE),
			apstatr (ap, SKY_MODE), apstatr (ap, SKY_SIGMA),
			apstati (ap, NSKY))
		else
		    pier = apremag (ap, apstati (ap, POSITIVE), apstatr (ap,
		        SKY_MODE), apstatr (ap, SKY_SIGMA), apstati (ap, NSKY))

		call apmark (ap, id, apstati (ap, MKCENTER), apstati (ap,
		    MKSKY), apstati (ap, MKAPERT))
		call appplot (ap, im, stid, cier, sier, pier, gd, apstati (ap,
		    RADPLOTS))
		if (interactive == YES)
		    call ap_qpmag (ap, cier, sier, pier)

		newcenterbuf = NO; newcenter = NO
		newskybuf = NO; newsky = NO
		newmagbuf = NO; newmag = NO

		if (key == ' ') {
		    if (stid == 1)
		        call ap_param (ap, out, "phot")
		    if (newlist == YES)
		        call ap_pmag (ap, out, stid, ltid, cier, sier, pier)
		    else
		        call ap_pmag (ap, out, stid, 0, cier, sier, pier)
		    call appplot (ap, im, stid, cier, sier, pier, mgd, YES)
		    stid = stid + 1
		}

	    default:
		call printf ("Unknown or ambiguous keystroke command\7\n")
	    }

	    # Setup for the next object.
	    key = ' '
	    Memc[cmd] = EOS
	}

	call sfree (sp)
end
