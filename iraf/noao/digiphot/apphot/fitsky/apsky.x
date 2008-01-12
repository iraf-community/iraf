include <ctype.h>
include <gset.h>
include <imhdr.h>
include "../lib/apphot.h"
include "../lib/display.h"
include "../lib/fitsky.h"

define	HELPFILE	"apphot$fitsky/fitsky.key"

# APSKY -- Procedure to interactively determine sky values in an annular
# region around a list of objects.

int procedure apsky (ap, im, cl, sd, gd, mgd, id, out, stid, interactive, cache)

pointer ap			# pointer to apphot structure
pointer	im			# pointer to IRAF image
int	cl			# starlist file descriptor
int	sd			# the sky file descriptor
pointer	gd			# pointer to graphcis descriptor
pointer	mgd			# pointer to graphics metacode file
pointer	id			# pointer to image display stream
int	out			# output file descriptor
int	stid			# output file sequence number
int	interactive		# interactive mode
int	cache			# cache the input image pixels

real	wx, wy, xlist, ylist
pointer sp, cmd
int	wcs, key, colonkey, newimage, newobject, newsky, newlist, ier
int	ip, ltid, oid, prev_num, req_num, buf_size, req_size, old_size
int	memstat

real	apstatr()
int	clgcur(), apfitsky(), aprefitsky(), apgscur(), ctoi(), apstati()
int	apgqverify(), apgtverify(), apnew(), ap_memstat(), sizeof()

define  endswitch_ 99

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Initialize cursor command.
	key = ' '
	Memc[cmd] = EOS

	# Initialize fitting parameters.
	newimage = NO
	newobject = YES
	newsky = YES
	ier = AP_OK

	# Initialize sequencing.
	newlist = NO
	ltid = 0

	# Loop over the cursor commands.
	while (clgcur ("icommands", wx, wy, wcs, key, Memc[cmd], SZ_LINE) !=
	    EOF) {

	    # Store the current cursor coordinates.
	    call ap_vtol (im, wx, wy, wx, wy, 1)
	    call apsetr (ap, CWX, wx)
	    call apsetr (ap, CWY, wy)

	    # Test to see if the cursor has moved.
	    if (apnew (ap, wx, wy, xlist, ylist, newlist) == YES) {
		newobject = YES
		newsky = YES
	    }

	    # Loop over the colon commands.
	    switch (key) {

	    # Quit.
	    case 'q':
		if (interactive == YES) {
		    if (apgqverify ("fitsky", ap, key) == YES) {
			call sfree (sp)
			return (apgtverify (key))
		    }
		} else {
		    call sfree (sp)
		    return (NO)
		}

	    # Print the error messages.
	    case 'e':
		if (interactive == YES)
		    call ap_serrors (ap, ier)

	    # Print the help page.
	    case '?':
		if ((id != NULL) && (id == gd))
		    call gpagefile (id, HELPFILE, "")
		else if (interactive == YES)
		    call pagefile (HELPFILE, "[space=morehelp,q=quit,?=help]")

	    # Rewind the coordinate file.
	    case 'r':
		if (cl != NULL) {
		    call seek (cl, BOFL)
		    ltid = 0
		} else if (interactive == YES)
		    call printf ("No coordinate list\n")

	    # Verify the critical parameters.
	    case 'v':
		call ap_sconfirm (ap, out, stid)
		newobject = YES
		newsky = YES

	    # Save the sky fitting parameters.
	    case 'w':
		call ap_pspars (ap)

	    # Draw a centered radial profile plot.
	    case 'd':
	        if (interactive == YES) {
		    call ap_qrad (ap, im, wx, wy, gd)
		    newobject = YES
		    newsky = YES
	        }

	    # Interactively set up sky fitting parameters.
	    case 'i':
		if (interactive == YES) {
		    call ap_sradsetup (ap, im, wx, wy, gd, out, stid)
		    newobject = YES
		    newsky = YES
		}

	    # Process fitsky colon commands.
	    case ':':
		for (ip = 1; IS_WHITE(Memc[cmd+ip-1]); ip = ip + 1)
		    ;
		colonkey = Memc[cmd+ip-1]
		switch (colonkey) {
		case 'm', 'n':

		    # Show/set fitsky commands.
		    if (Memc[cmd+ip] != EOS && Memc[cmd+ip] != ' ') {
		        call apskycolon (ap, im, cl, out, stid, ltid,
			    Memc[cmd], newimage, newobject, newsky)
			goto endswitch_
		    }

                    # No coordinate list.
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

		    # Fetch the next object from the list.
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

		    # Move to the next object.
		    newlist = YES
		    if (colonkey == 'm') {
		        newobject = YES
			newsky = YES
                        goto endswitch_
		    }

		    # Measure the next object.	
		    ier = apfitsky (ap, im, xlist, ylist, sd, gd)
		    if (id != NULL) {
		        call apmark (ap, id, NO, apstati (ap, MKSKY), NO)
			if (id == gd)
			    call gflush (id)
			else
			    call gframe (id)
		    }
		    call ap_splot (ap, stid, gd, apstati (ap, RADPLOTS))
		    if (interactive == YES)
		        call ap_qspsky (ap, ier)
		    if (stid == 1)
		        call ap_param (ap, out, "fitsky")
		    call ap_splot (ap, stid, mgd, YES)
		    call ap_pssky (ap, out, stid, ltid, ier)
		    stid = stid + 1
		    newobject = NO; newsky = NO

		default:
		    call apskycolon (ap, im, cl, out, stid, ltid, Memc[cmd],
		        newimage, newobject, newsky)
		}

		if (newimage == YES) {
		    if ((id != NULL) && (gd != id))
		        call ap_gswv (id, Memc[cmd], im, 4)
                    req_size = MEMFUDGE * IM_LEN(im,1) * IM_LEN(im,2) *
                        sizeof (IM_PIXTYPE(im))
                    memstat = ap_memstat (cache, req_size, old_size)
                    if (memstat == YES)
                        call ap_pcache (im, INDEFI, buf_size)
		}

		newimage = NO

	    # Fit the sky and store the results.
	    case 'f', ' ':
		if (newobject == YES)
		    ier = apfitsky (ap, im, wx, wy, sd, gd)
		else if (newsky == YES)
		    ier = aprefitsky (ap, im, gd)
		if (id != NULL) {
		    call apmark (ap, id, NO, apstati (ap, MKSKY), NO)
		    if (id == gd)
			call gflush (id)
		    else
		        call gframe (id)
		}
		call ap_splot (ap, stid, gd, apstati (ap, RADPLOTS))
		if (interactive == YES)
		    call ap_qspsky (ap, ier)
		newobject = NO; newsky = NO

		if (key == ' ') {
		    if (stid == 1)
		        call ap_param (ap, out, "fitsky")
		    if (newlist == YES)
		        call ap_pssky (ap, out, stid, ltid, ier)
		    else
		        call ap_pssky (ap, out, stid, 0, ier)
		    call ap_splot (ap, stid, mgd, YES)
		    stid = stid + 1
		}

	    # Get, fit the next object in the list.
	    case 'm', 'n':

                # No coordinate file.
                if (cl == NULL) {
                    if (interactive == YES)
                        call printf ("No coordinate list\n")
                    goto endswitch_
                }

		# Need to rewind coordinate file.
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
		    newobject = YES
		    newsky = YES
                    goto endswitch_
		}

		# Measure the next object.
		ier = apfitsky (ap, im, xlist, ylist, sd, gd)
		if (id != NULL) {
		    call apmark (ap, id, NO, apstati (ap, MKSKY), NO)
		    if (id == gd)
			call gflush (id)
		    else
			call gframe (id)
		}
		call ap_splot (ap, stid, gd, apstati (ap, RADPLOTS))
		if (interactive == YES)
		    call ap_qspsky (ap, ier)
		if (stid == 1)
		    call ap_param (ap, out, "fitsky")
		call ap_pssky (ap, out, stid, ltid, ier)
		call ap_splot (ap, stid, mgd, YES)
		stid = stid + 1
		newobject = NO
		newsky = NO

	    # Process the remainder of the list.
	    case 'l':
		if (cl != NULL) {
		    ltid = ltid + 1
		    oid = stid
		    call apbsky (ap, im, cl, sd, out, stid, ltid, gd, mgd, id,
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

	    default:
		# do nothing
		call printf ("Unknown or ambiguous keystroke command\n")
	    }

endswitch_

	    # Setup for the next object.
	    key = ' '
	    Memc[cmd] = EOS
	    call apsetr (ap, WX, apstatr (ap, CWX))
	    call apsetr (ap, WY, apstatr (ap, CWY))

	}

	call sfree (sp)
end
