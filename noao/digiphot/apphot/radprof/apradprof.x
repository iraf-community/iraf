include <ctype.h>
include <gset.h>
include "../lib/apphot.h"
include "../lib/display.h"
include "../lib/center.h"
include "../lib/fitsky.h"
include "../lib/radprof.h"

define	HELPFILE	"apphot$radprof/radprof.key"

# AP_RADPROF -- Procedure to determine radial profiles for a list of objects
# in a list of images.

int procedure ap_radprof (ap, im, cl, gd, mgd, id, out, stid, interactive)

pointer ap			# pointer to apphot structure
pointer	im			# pointer to IRAF image
int	cl			# starlist file descriptor
pointer	gd			# pointer to graphcis descriptor
pointer	mgd			# pointer to plot metacode stream
pointer	id			# pointer to image display stream
int	out			# output file descriptor
int	stid			# output file sequence number
int	interactive		# interactive mode

int	wcs, key, oid, ltid, newlist, colonkey
int	newskybuf, newsky, newcenterbuf, newcenter, newbuf, newfit
int	ip, prev_num, req_num, cier, sier, pier, rier
pointer	sp, cmd
real	wx, wy, xlist, ylist

bool	fp_equalr()
int	clgcur(), apfitsky(), aprefitsky(), apfitcenter(), aprefitcenter()
int	apstati(), apgscur(), ap_frprof(), ctoi(), apgqverify(), apgtverify()
int	apnew()
real	apstatr()

begin
	# Initialize.
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Initialize the cursor command.
	key = ' '
	Memc[cmd] = EOS

	# Initialize the fit.
	newcenterbuf = YES
	newcenter = YES
	newskybuf = YES
	newsky = YES
	newbuf = YES
	newfit = YES
	cier = AP_OK
	sier = AP_OK
	pier = AP_OK
	rier = AP_OK

	# Initialize the sequencing.
	newlist = NO
	ltid = 0

	# Loop over the coordinate file.
	while (clgcur ("commands", wx, wy, wcs, key, Memc[cmd], SZ_LINE) !=
	    EOF) {

	    # Store the cursor coords.
	    call apsetr (ap, CWX, wx)
	    call apsetr (ap, CWY, wy)

	    # Test to see if the cursor has moved.
	    if (apnew (ap, wx, wy, xlist, ylist, newlist) == YES) {
		newcenterbuf = YES
		newcenter = YES
		newskybuf = YES
		newsky = YES
		newbuf = YES
		newfit = YES
	    }

	    # Switch on the keystroke commands.
	    switch (key) {

	    # Quit.
	    case 'q':
		if (interactive == YES) {
		    if (apgqverify ("radprof", ap, key) == YES) {
			call sfree (sp)
			return (apgtverify (key))
		    }
	        } else {
		    call sfree (sp)
		    return (NO)
	        }

	    # Print the errors.
	    case 'e':
		if (interactive == YES)
		    call ap_rferrors (ap, cier, sier, pier, rier)

	    # Print the help page.
	    case '?':
		if (id != NULL)
		    call gpagefile (id, HELPFILE, "")
		else if (interactive == YES)
		    call pagefile (HELPFILE, "[space=morehelp,q=quit,?=help]")

	    # Rewind the list.
	    case 'r':
		if (cl != NULL) {
		    call seek (cl, BOF)
		    ltid = 0
		} else if (interactive == YES)
		    call printf ("No coordinate list\7\n")

	    # Process the remainder of the list.
	    case 'l':
		if (cl != NULL) {
		    oid = stid
		    ltid = ltid + 1
		    call ap_bradprof (ap, im, cl, id, gd, mgd, out, stid, ltid,
		        YES)
		    ltid = ltid + stid - oid + 1
		} else if (interactive == YES)
		    call printf ("No coordinate list\7\n")

	    # Process radprof colon commands.
	    case ':':
		for (ip = 1; IS_WHITE(Memc[cmd+ip-1]); ip = ip + 1)
		    ;
		colonkey = Memc[cmd+ip-1]
		switch (colonkey) {
		case 'm':

		    # Show/set radprof parameters.
		    if (Memc[cmd+ip] != EOS && Memc[cmd+ip] != ' ') {
		        call ap_rpcolon (ap, im, cl, out, stid, ltid,
			    Memc[cmd], newcenterbuf, newcenter, newskybuf,
			    newsky, newbuf, newfit)

		    # Get next object out of the list.
		    } else if (cl != NULL) {
			ip = ip + 1
			prev_num = ltid
			if (ctoi (Memc[cmd], ip, req_num) <= 0)
			    req_num = ltid + 1
		        if (apgscur (cl, id, xlist, ylist, prev_num, req_num,
			    ltid) != EOF) {
			    newcenterbuf = YES
			    newcenter = YES
			    newskybuf = YES
			    newsky = YES
			    newbuf = YES
			    newfit = YES
		    	    newlist = YES
			} else
			    call printf (
			    "End of coordinate list, use r key to rewind\7\n")

		    } else if (interactive == YES)
		        call printf ("No coordinate list\7\n")

		case 'n':

		    # Show/set radprof parameters.
		    if (Memc[cmd+ip] != EOS && Memc[cmd+ip] != ' ') {
		        call ap_rpcolon (ap, im, cl, out, stid, ltid, Memc[cmd],
			    newcenterbuf, newcenter, newskybuf, newsky, newbuf,
			    newfit)

		    # Process the next object.
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
			        apstatr (ap, YCENTER), NULL, gd)
		    	    rier = ap_frprof (ap, im, apstatr (ap, XCENTER),
			        apstatr (ap, YCENTER), pier)

			    call aprmark (ap, id, apstati (ap, MKCENTER),
				apstati (ap, MKSKY), apstati (ap, MKAPERT))
			    call ap_rpplot (ap, stid, cier, sier, pier, rier,
			        gd, apstati (ap, RADPLOTS))
			    if (interactive == YES)
		    	        call ap_qprprof (ap, cier, sier, pier, rier)

		    	    if (stid == 1)
				call ap_param (ap, out, "radprof")
		            call ap_prprof (ap, out, stid, ltid, cier, sier,
			        pier, rier)
			    call ap_rpplot (ap, stid, cier, sier, pier, rier,
			        mgd, YES)
		    	    stid = stid + 1

		    	    newcenterbuf = NO
			    newcenter = NO
		    	    newskybuf = NO
			    newsky = NO
		    	    newbuf = NO
			    newfit = NO

			} else
			    call printf (
			    "End of coordinate list, use r key to rewind\7\n")

		    } else if (interactive == YES)
			call printf ("No coordinate list\7\n")

		default:
		    call ap_rpcolon (ap, im, cl, out, stid, ltid, Memc[cmd],
		        newcenterbuf, newcenter, newskybuf, newsky, newbuf,
			newfit)
		}

	    # Save the parameters.
	    case 'w':
		call ap_rpars (ap)

	    # Plot a simple centered radial profile.
	    case 'd':
		if (interactive == YES) {
		    call ap_qrad (ap, im, wx, wy, gd)
		    newcenterbuf = YES; newcenter = YES
		    newskybuf = YES; newsky = YES
		    newbuf = YES; newfit = YES
		}
		

	    # Setup the radial profile fitting parameters interactively.
	    case 'i':
		if (interactive == YES) {
		    call ap_profsetup (ap, im, wx, wy, gd, out, stid)
		    newcenterbuf = YES; newcenter = YES
		    newskybuf = YES; newsky = YES
		    newbuf = YES; newfit = YES
		}

	    # Verify the critical radprof parameters.
	    case 'v':
		call ap_rconfirm (ap, out, stid)

	     # Fit the center around the current cursor value.
	     case 'c':
		if (newcenterbuf == YES)
		    cier = apfitcenter (ap, im, wx, wy)
		else if (newcenter == YES)
		    cier = aprefitcenter (ap, cier)
		call aprmark (ap, id, apstati (ap, MKCENTER), NO, NO)
		call apcplot (ap, stid, cier, gd, apstati (ap, RADPLOTS))
		if (interactive == YES)
		    call ap_qcenter (ap, cier)
		newcenterbuf = NO
		newcenter = NO

	    # Fit the sky around the current cursor value.
	    case 't':
	        if (newskybuf == YES || ! fp_equalr (wx,
		    apstatr (ap, SXCUR)) || ! fp_equalr (wy, apstatr (ap,
		    SYCUR)))
		    sier = apfitsky (ap, im, wx, wy, NULL, gd)
	        else if (newsky == YES)
		    sier = aprefitsky (ap, gd)
		call aprmark (ap, id, NO, apstati (ap, MKSKY), NO)
		call apsplot (ap, stid, sier, gd, apstati (ap, RADPLOTS))
		if (interactive == YES)
		    call ap_qspsky (ap, sier)
		newskybuf = NO
		newsky = NO

	     # Fit the sky around derived center value. 
	     case 's':
	        if (newskybuf == YES || ! fp_equalr (apstatr (ap, XCENTER),
		    apstatr (ap, SXCUR)) || ! fp_equalr (apstatr (ap,
		    SYCUR), apstatr (ap, YCENTER)))
		    sier = apfitsky (ap, im, apstatr (ap, XCENTER),
		        apstatr (ap, YCENTER), NULL, gd)
		else if (newsky == YES)
		    sier = aprefitsky (ap, gd)
		call aprmark (ap, id, NO, apstati (ap, MKSKY), NO)
		call apsplot (ap, stid, sier, gd, apstati (ap, RADPLOTS))
		if (interactive == YES)
		    call ap_qspsky (ap, sier)
		newskybuf = NO
		newsky = NO

	    # Compute magnitudes around the current cursor position using
	    # the current sky.
	    case 'p':
		call aprmark (ap, id, NO, NO, apstati (ap, MKAPERT))
	        if (newfit == YES || newbuf == YES || ! fp_equalr (wx,
		    apstatr (ap, RPXCUR)) || ! fp_equalr (apstatr (ap, RPYCUR),
		    wy))
		    rier = ap_frprof (ap, im, wx, wy, pier)
		if (interactive == YES)
		    call ap_qprprof (ap, cier, sier, pier, rier)
		newbuf = NO
		newfit = NO

	    # Center, fit the sky, and compute magnitudes.
	    # Compute the centers, fit the sky, compute the magnitudes
	    # and save the results.
	    case 'f', ' ':
		if (newcenterbuf == YES)
		    cier = apfitcenter (ap, im, wx, wy)
		else if (newcenter == YES)
		    cier = aprefitcenter (ap, cier)
		if (newskybuf == YES || ! fp_equalr (apstatr (ap, XCENTER),
		    apstatr (ap, SXCUR)) || ! fp_equalr (apstatr (ap, YCENTER),
		    apstatr (ap, SYCUR)))
		    sier = apfitsky (ap, im, apstatr (ap, XCENTER),
		        apstatr (ap, YCENTER), NULL, gd)
		else if (newsky == YES)
		    sier = aprefitsky (ap, gd)

		if (newfit == YES || newbuf == YES || ! fp_equalr (apstatr (ap,
		    XCENTER), apstatr (ap, RPXCUR)) || ! fp_equalr (apstatr (ap,
		    YCENTER), apstatr (ap, RPYCUR)))
		    rier = ap_frprof (ap, im, apstatr (ap, XCENTER),
		        apstatr (ap, YCENTER), pier)
		call aprmark (ap, id, apstati (ap, MKCENTER), apstati (ap,
		    MKSKY), apstati (ap, MKAPERT))
		call ap_rpplot (ap, stid, cier, sier, pier, rier, gd,
		    apstati (ap, RADPLOTS))
		if (interactive == YES)
		    call ap_qprprof (ap, cier, sier, pier, rier)

		newcenterbuf = NO; newcenter = NO
		newskybuf = NO; newsky = NO
		newbuf = NO; newfit = NO

		if (key == ' ') {
		    if (stid == 1)
		        call ap_param (ap, out, "radprof")
		    if (newlist == YES)
		        call ap_prprof (ap, out, stid, ltid, cier, sier, pier,
			    rier)
		    else
		        call ap_prprof (ap, out, stid, 0, cier, sier, pier,
			    rier)
		    call ap_rpplot (ap, stid, cier, sier, pier, rier, mgd, YES)
		    stid = stid + 1
		}

	    default:
		# do nothing
		call printf ("Print unknown or ambiguous colon command\7\n")
	    }

	    # Prepare for the next object.
	    key = ' '
	    Memc[cmd] = EOS
	    call apsetr (ap, WX, apstatr (ap, CWX))
	    call apsetr (ap, WY, apstatr (ap, CWY))

	}

	call sfree (sp)
end
