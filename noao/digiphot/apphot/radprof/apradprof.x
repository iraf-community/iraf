include <ctype.h>
include <gset.h>
include <imhdr.h>
include "../lib/apphot.h"
include "../lib/display.h"
include "../lib/center.h"
include "../lib/fitsky.h"
include "../lib/radprof.h"

define	HELPFILE	"apphot$radprof/radprof.key"

# AP_RADPROF -- Procedure to determine radial profiles for a list of objects
# in a list of images.

int procedure ap_radprof (ap, im, cl, gd, mgd, id, out, stid, interactive,
	cache)

pointer ap			# pointer to apphot structure
pointer	im			# pointer to IRAF image
int	cl			# starlist file descriptor
pointer	gd			# pointer to graphcis descriptor
pointer	mgd			# pointer to plot metacode stream
pointer	id			# pointer to image display stream
int	out			# output file descriptor
int	stid			# output file sequence number
int	interactive		# interactive mode
int	cache			# cache the input image pixels

real	wx, wy, xlist, ylist
pointer	sp, cmd
int	wcs, key, oid, ltid, newlist, colonkey, req_size, buf_size, old_size
int	newimage, newskybuf, newsky, newcenterbuf, newcenter, newbuf, newfit
int	ip, prev_num, req_num, cier, sier, pier, rier, memstat

real	apstatr()
int	clgcur(), apfitsky(), aprefitsky(), apfitcenter(), aprefitcenter()
int	apstati(), apgscur(), ap_frprof(), ctoi(), apgqverify(), apgtverify()
int	apnew(), ap_avsky(), sizeof(), ap_memstat()
bool	fp_equalr()

define  endswitch_ 99

begin
	# Initialize.
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Initialize the cursor command.
	key = ' '
	Memc[cmd] = EOS

	# Initialize the fit.
	newimage = NO
	newcenterbuf = YES; newcenter = YES
	newskybuf = YES; newsky = YES
	newbuf = YES; newfit = YES
	cier = AP_OK; sier = AP_OK; pier = AP_OK; rier = AP_OK

	# Initialize the sequencing.
	newlist = NO
	ltid = 0

	# Loop over the coordinate file.
	while (clgcur ("icommands", wx, wy, wcs, key, Memc[cmd], SZ_LINE) !=
	    EOF) {

	    # Store the cursor coords.
	    call ap_vtol (im, wx, wy, wx, wy, 1)
	    call apsetr (ap, CWX, wx)
	    call apsetr (ap, CWY, wy)

	    # Test to see if the cursor has moved.
	    if (apnew (ap, wx, wy, xlist, ylist, newlist) == YES) {
		newcenterbuf = YES; newcenter = YES
		newskybuf = YES; newsky = YES
		newbuf = YES; newfit = YES
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
		if ((id != NULL) && (id == gd))
		    call gpagefile (id, HELPFILE, "")
		else if (interactive == YES)
		    call pagefile (HELPFILE, "[space=morehelp,q=quit,?=help]")

	    # Rewind the list.
	    case 'r':
		if (cl != NULL) {
		    call seek (cl, BOF)
		    ltid = 0
		} else if (interactive == YES)
		    call printf ("No coordinate list\n")

	    # Move, measure next object in the coordinate list.
	    case 'm', 'n':

		# No coordinate file.
		if (cl == NULL) {
                    if (interactive == YES)
                        call printf ("No coordinate list\n")
                    goto endswitch_
		}

		# Need to rewind the coordinate file.
		prev_num = ltid
		req_num = ltid + 1
		if (apgscur (cl, id, xlist, ylist, prev_num, req_num,
		    ltid) == EOF) {
                    if (interactive == YES)
                        call printf (
                            "End of coordinate list, use r key to rewind\n")
                    goto endswitch_
		}

		# Convert coordinates if necessary.
                switch (apstati (ap, WCSIN)) {
                case WCS_PHYSICAL, WCS_WORLD:
                    call ap_itol (ap,  xlist, ylist, xlist, ylist, 1)
                case WCS_TV:
                    call ap_vtol (im, xlist, ylist, xlist, ylist, 1)
                default:
                    ;
                }

		# Move to the next object.
		newlist = YES
		if (key == 'm') {
		    newcenterbuf = YES; newcenter = YES
		    newskybuf = YES; newsky = YES
		    newbuf = YES; newfit = YES
                    goto endswitch_
		}

		# Measure next object.
		cier = apfitcenter (ap, im, xlist, ylist)
		sier = apfitsky (ap, im, apstatr (ap, XCENTER), apstatr (ap,
		    YCENTER), NULL, gd)
		rier = ap_frprof (ap, im, apstatr (ap, XCENTER), apstatr (ap,
		    YCENTER), pier)
		call aprmark (ap, id, apstati (ap, MKCENTER), apstati (ap,
		    MKSKY), apstati (ap, MKAPERT))
	        if (id != NULL) {
		    if (id == gd)
		    	call gflush (id)
		    else
		        call gframe (id)
		}
	        call ap_rpplot (ap, stid, gd, apstati (ap, RADPLOTS))
		if (interactive == YES)
		    call ap_qprprof (ap, cier, sier, pier, rier)
		if (stid == 1)
		    call ap_param (ap, out, "radprof")
		call ap_prprof (ap, out, stid, ltid, cier, sier, pier, rier)
		call ap_rpplot (ap, stid, mgd, YES)
		stid = stid + 1
		newcenterbuf = NO; newcenter = NO
		newskybuf = NO; newsky = NO
		newbuf = NO; newfit = NO

	    # Process the remainder of the list.
	    case 'l':
		if (cl != NULL) {
		    oid = stid
		    ltid = ltid + 1
		    call ap_bradprof (ap, im, cl, id, gd, mgd, out, stid, ltid,
		        YES)
		    ltid = ltid + stid - oid + 1
		    if (id != NULL) {
			if (id == gd)
			    call gflush (id)
			else
			    call gframe (id)
		    }
		} else if (interactive == YES)
		    call printf ("No coordinate list\n")

	    # Process radprof colon commands.
	    case ':':
		for (ip = 1; IS_WHITE(Memc[cmd+ip-1]); ip = ip + 1)
		    ;
		colonkey = Memc[cmd+ip-1]
		switch (colonkey) {
		case 'm', 'n':

		    # Show/set radprof parameters.
		    if (Memc[cmd+ip] != EOS && Memc[cmd+ip] != ' ') {
		        call ap_rpcolon (ap, im, cl, out, stid, ltid, Memc[cmd],
			    newimage, newcenterbuf, newcenter, newskybuf,
			    newsky, newbuf, newfit)
			goto endswitch_
		    }

		    # Process the next object.
		    if (cl == NULL) {
                        if (interactive == YES)
                            call printf ("No coordinate list\n")
                        goto endswitch_
		    }

		    # Get next object from the list.
		    ip = ip + 1
		    prev_num = ltid
		    if (ctoi (Memc[cmd], ip, req_num) <= 0)
		        req_num = ltid + 1

		    # Fetch next object from the list.
		    if (apgscur (cl, id, xlist, ylist, prev_num, req_num,
			ltid) == EOF) {
                        if (interactive == YES)
                            call printf (
                            "End of coordinate list, use r key to rewind\n")
                        goto endswitch_

		    }

                    # Convert the coordinates.
                    switch (apstati (ap, WCSIN)) {
                    case WCS_PHYSICAL, WCS_WORLD:
                        call ap_itol (ap,  xlist, ylist, xlist, ylist, 1)
                    case WCS_TV:
                        call ap_vtol (im, xlist, ylist, xlist, ylist, 1)
                    default:
                        ;
                    }

		    # Move to next object.
		    newlist = YES
		    if (colonkey == 'm') {
		        newcenterbuf = YES; newcenter = YES
		        newskybuf = YES; newsky = YES
		        newbuf = YES; newfit = YES
		    }

		    # Measure the next object.
		    cier = apfitcenter (ap, im, xlist, ylist)
		    sier = apfitsky (ap, im, apstatr (ap, XCENTER),
			apstatr (ap, YCENTER), NULL, gd)
		    rier = ap_frprof (ap, im, apstatr (ap, XCENTER),
			apstatr (ap, YCENTER), pier)
		    call aprmark (ap, id, apstati (ap, MKCENTER), apstati (ap,
			MKSKY), apstati (ap, MKAPERT))
		    if (id != NULL) {
		        if (id == gd)
		    	    call gflush (id)
			else
			    call gframe (id)
		    }
		    call ap_rpplot (ap, stid, gd, apstati (ap, RADPLOTS))
		    if (interactive == YES)
		        call ap_qprprof (ap, cier, sier, pier, rier)

		    if (stid == 1)
		        call ap_param (ap, out, "radprof")
		    call ap_prprof (ap, out, stid, ltid, cier, sier, pier, rier)
		    call ap_rpplot (ap, stid, mgd, YES)
		    stid = stid + 1
		     newcenterbuf = NO; newcenter = NO
		     newskybuf = NO; newsky = NO
		     newbuf = NO; newfit = NO

		default:
		    call ap_rpcolon (ap, im, cl, out, stid, ltid, Memc[cmd],
		        newimage, newcenterbuf, newcenter, newskybuf, newsky,
			newbuf, newfit)
		}

		# Reestablish the image viewport and window.
		if (newimage == YES) {
		    if ((id != NULL) && (id != gd))
		        call ap_gswv (id, Memc[cmd], im, 4)
                    req_size = MEMFUDGE * IM_LEN(im,1) * IM_LEN(im,2) *
                        sizeof (IM_PIXTYPE(im))
                    memstat = ap_memstat (cache, req_size, old_size)
                    if (memstat == YES)
                        call ap_pcache (im, INDEFI, buf_size)
		}

		newimage = NO

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
		newcenterbuf = YES; newcenter = YES
		newskybuf = YES; newsky = YES
		newbuf = YES; newfit = YES

	     # Fit the center around the current cursor value.
	     case 'c':
		if (newcenterbuf == YES)
		    cier = apfitcenter (ap, im, wx, wy)
		else if (newcenter == YES)
		    cier = aprefitcenter (ap, im, cier)
		call aprmark (ap, id, apstati (ap, MKCENTER), NO, NO)
		if (id != NULL) {
		    if (id == gd)
			call gflush (id)
		    else
			call gframe (id)
		}
		call ap_cplot (ap, stid, gd, apstati (ap, RADPLOTS))
		if (interactive == YES)
		    call ap_qcenter (ap, cier)
		newcenterbuf = NO; newcenter = NO

	    # Fit the sky around the current cursor value.
	    case 't':
	        if (newskybuf == YES || ! fp_equalr (wx,
		    apstatr (ap, SXCUR)) || ! fp_equalr (wy, apstatr (ap,
		    SYCUR)))
		    sier = apfitsky (ap, im, wx, wy, NULL, gd)
	        else if (newsky == YES)
		    sier = aprefitsky (ap, im, gd)
		call aprmark (ap, id, NO, apstati (ap, MKSKY), NO)
		if (id != NULL) {
		    if (id == gd)
			call gflush (id)
		    else
			call gframe (id)
		}
		call ap_splot (ap, stid, gd, apstati (ap, RADPLOTS))
		if (interactive == YES)
		    call ap_qspsky (ap, sier)
		newskybuf = NO; newsky = NO

	    # Compute the average of several sky measurements around
            # different cursor postions.
            case 'a':
                sier = ap_avsky (ap, im, stid, NULL, id, gd, interactive)
                if (interactive == YES)
                    call ap_qaspsky (ap, sier)
                newskybuf = NO; newsky = NO

	     # Fit the sky around derived center value. 
	     case 's':
	        if (newskybuf == YES || ! fp_equalr (apstatr (ap, XCENTER),
		    apstatr (ap, SXCUR)) || ! fp_equalr (apstatr (ap,
		    SYCUR), apstatr (ap, YCENTER)))
		    sier = apfitsky (ap, im, apstatr (ap, XCENTER),
		        apstatr (ap, YCENTER), NULL, gd)
		else if (newsky == YES)
		    sier = aprefitsky (ap, im, gd)
		call aprmark (ap, id, NO, apstati (ap, MKSKY), NO)
		if (id != NULL) {
		    if (id == gd)
			call gflush (id)
		    else
			call gframe (id)
		}
		call ap_splot (ap, stid, gd, apstati (ap, RADPLOTS))
		if (interactive == YES)
		    call ap_qspsky (ap, sier)
		newskybuf = NO; newsky = NO

	    # Compute magnitudes around the current cursor position using
	    # the current sky.
	    case 'p', 'o':
		if (newcenterbuf == YES)
		    cier = apfitcenter (ap, im, wx, wy)
		else if (newcenter == YES)
		    cier = aprefitcenter (ap, im, cier)
		call aprmark (ap, id, apstati (ap, MKCENTER), NO,
		    apstati (ap, MKAPERT))
		if (id != NULL) {
		    if (id == gd)
			call gflush (id)
		    else
			call gframe (id)
		}
	        if (newfit == YES || newbuf == YES || ! fp_equalr (apstatr (ap,
		    XCENTER), apstatr (ap, RPXCUR)) ||
		    ! fp_equalr (apstatr (ap, RPYCUR), apstatr (ap, YCENTER)))
		    rier = ap_frprof (ap, im, apstatr (ap, XCENTER),
		        apstatr (ap, YCENTER), pier)
		if (interactive == YES)
		    call ap_qprprof (ap, cier, sier, pier, rier)
		newcenterbuf = NO; newcenter = NO
		newbuf = NO; newfit = NO

		if (key == 'o') {
		    if (stid == 1)
		        call ap_param (ap, out, "radprof")
		    if (newlist == YES)
		        call ap_prprof (ap, out, stid, ltid, cier, sier, pier,
			    rier)
		    else
		        call ap_prprof (ap, out, stid, 0, cier, sier, pier,
			    rier)
		    call ap_rpplot (ap, stid, mgd, YES)
		    stid = stid + 1
		}

	    # Center, fit the sky, and compute magnitudes.
	    # Compute the centers, fit the sky, compute the magnitudes
	    # and save the results.
	    case 'f', ' ':
		if (newcenterbuf == YES)
		    cier = apfitcenter (ap, im, wx, wy)
		else if (newcenter == YES)
		    cier = aprefitcenter (ap, im, cier)
		if (newskybuf == YES || ! fp_equalr (apstatr (ap, XCENTER),
		    apstatr (ap, SXCUR)) || ! fp_equalr (apstatr (ap, YCENTER),
		    apstatr (ap, SYCUR)))
		    sier = apfitsky (ap, im, apstatr (ap, XCENTER),
		        apstatr (ap, YCENTER), NULL, gd)
		else if (newsky == YES)
		    sier = aprefitsky (ap, im, gd)

		if (newfit == YES || newbuf == YES || ! fp_equalr (apstatr (ap,
		    XCENTER), apstatr (ap, RPXCUR)) || ! fp_equalr (apstatr (ap,
		    YCENTER), apstatr (ap, RPYCUR)))
		    rier = ap_frprof (ap, im, apstatr (ap, XCENTER),
		        apstatr (ap, YCENTER), pier)
		call aprmark (ap, id, apstati (ap, MKCENTER), apstati (ap,
		    MKSKY), apstati (ap, MKAPERT))
		if (id != NULL) {
		    if (id == gd)
			call gflush (id)
		    else
			call gframe (id)
		}
		call ap_rpplot (ap, stid, gd, apstati (ap, RADPLOTS))
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
		    call ap_rpplot (ap, stid, mgd, YES)
		    stid = stid + 1
		}

	    default:
		# do nothing
		call printf ("Print unknown or ambiguous colon command\n")
	    }

endswitch_
	    # Prepare for the next object.
	    key = ' '
	    Memc[cmd] = EOS
	    call apsetr (ap, WX, apstatr (ap, CWX))
	    call apsetr (ap, WY, apstatr (ap, CWY))

	}

	call sfree (sp)
end
