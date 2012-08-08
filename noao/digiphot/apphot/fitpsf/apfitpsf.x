include <ctype.h>
include <gset.h>
include <imhdr.h>
include "../lib/apphot.h"
include "../lib/display.h"
include "../lib/fitpsf.h"

define	HELPFILE	"apphot$fitpsf/fitpsf.key"

# APFITPSF -- Procedure to fit a functional form to the PSF for a list of
# objects in interactive mode.

int procedure apfitpsf (ap, im, cl, gd, id, out, stid, interactive, cache)

pointer ap			# pointer to apphot structure
pointer	im			# pointer to IRAF image
int	cl			# starlist file descriptor
pointer	gd			# graphics pointer
pointer	id			# display pointer
int	out			# output file descriptor
int	stid			# output file sequence number
int	interactive		# interactive mode
int	cache			# cache the input image pixels

real	wx, wy, xlist, ylist
pointer	sp, cmd
int	wcs, key, newimage, newbuf, newfit, newlist, ltid, ier
int	ip, oid, colonkey, prev_num, req_num, buf_size, req_size, old_size
int	memstat

real	apstatr()
int	clgcur(), apgscur(), apsffit(), apsfrefit(), apstati()
int	apgqverify(), apgtverify(), ctoi(), apnew(), ap_memstat(), sizeof()

define  endswitch_ 99

begin
	# Initialize.
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Initialize the cursor commands.
	key = ' '
	Memc[cmd] = EOS

	# Initialize the fitting commands.
	newimage = NO
	newbuf = YES
	newfit = YES
	ier = AP_OK

	# Initialize the sequencing.
	newlist = NO
	ltid = 0

	# Loop over the cursor commands.
	while (clgcur ("icommands", wx, wy, wcs, key, Memc[cmd], SZ_LINE) !=
	    EOF) {

	    # Store the cursor coordinates
	    call ap_vtol (im, wx, wy, wx, wy, 1)
	    call apsetr (ap, CWX, wx)
	    call apsetr (ap, CWY, wy)

	    # Test to see if the cursor moved.
	    if (apnew (ap, wx, wy, xlist, ylist, newlist) == YES) {
		newbuf = YES
		newfit = YES
	    }

	    # Loop over the cursor commands.
	    switch (key) {

	    # Quit.
	    case 'q':
		if (interactive == YES) {
		    if (apgqverify ("fitpsf", ap, key) == YES) {
			call sfree (sp)
			return (apgtverify (key))
		    }
		} else {
		    call sfree (sp)
		    return (NO)
		}
	    

	    # Print error messages.
	    case 'e':
		if (interactive == YES)
		    call ap_pferrors (ap, ier)

	    # Print the help pages.
	    case '?':
		if ((id != NULL) && (id == gd))
		    call gpagefile (id, HELPFILE, "")
		else if (interactive == YES)
		    call pagefile (HELPFILE, "[space=morehelp,q=quit,?=help]")

	    # Rewind the list.
	    case 'r':
		if (cl != NULL) {
		    call seek (cl, BOFL)
		    ltid = 0
		} else if (interactive == YES)
		    call printf ("No coordinate list\n")

	    # Process the fitpsf colon commands.
	    case ':':
		for (ip = 1; IS_WHITE(Memc[cmd+ip-1]); ip = ip + 1)
		    ;
		colonkey = Memc[cmd+ip-1]

		switch (colonkey) {
		case 'm', 'n':

		    # Show/set fitpsf parameters.
		    if (Memc[cmd+ip] != EOS && Memc[cmd+ip] != ' ') {
		        call apsfcolon (ap, im, cl, out, stid, ltid, Memc[cmd],
			    newimage, newbuf, newfit)
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
			newbuf = YES
			newfit = YES
                        goto endswitch_
		    }

		    # Measure the next object.
	            ier = apsffit (ap, im, xlist, ylist)
		    if (id != NULL) {
		        call appfmark (ap, id, apstati (ap, MKPSFBOX))
		    	if (id == gd)
			    call gflush (id)
		    	else
			    call gframe (id)
		    }
		    if (interactive == YES)
	                call ap_qppsf (ap, ier)
		    if (stid == 1)
		        call ap_param (ap, out, "fitpsf")
		    call ap_ppsf (ap, out, stid, ltid, ier)
		    stid = stid + 1
		    newbuf = NO
		    newfit = NO

		default:
		    call apsfcolon (ap, im, cl, out, stid, ltid, Memc[cmd],
		        newimage, newbuf, newfit)
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

	    # Plot a centered stellar radial profile.
	    case 'd':
		if (interactive == YES) {
		    call ap_qrad (ap, im, wx, wy, gd)
		    newbuf = YES
		    newfit = YES
		}

	    # Verify the parameters interactively.
	    case 'v':
		call ap_pfconfirm (ap, out, stid)
		newbuf = YES
		newfit = YES

	    # Interactively set up fitpsf parameters.
	    case 'i':
		if (interactive == YES) {
		    call ap_pfradsetup (ap, im, wx, wy, gd, out, stid)
		    newbuf = YES
		    newfit = YES
		}

	    # Save the parameters.
	    case 'w':
		call ap_ppfpars (ap)

	    # Fit the PSF and save the results.
	    case 'f',  ' ':
	        if (newbuf == YES)
	            ier = apsffit (ap, im, wx, wy)
	        else if (newfit == YES)
	            ier = apsfrefit (ap, im)
		if (id != NULL) {
		    call appfmark (ap, id, apstati (ap, MKPSFBOX))
		    if (id == gd)
			call gflush (id)
		    else
			call gframe (id)
		}
		if (interactive == YES)
		    call ap_qppsf (ap, ier)
		newbuf = NO
		newfit = NO

		if (key == ' ') {
		    if (stid == 1)
		        call ap_param (ap, out, "fitpsf")
		    if (newlist == YES)
		        call ap_ppsf (ap, out, stid, ltid, ier)
		    else
		        call ap_ppsf (ap, out, stid, 0, ier)
		    stid = stid + 1
		}


	    # Get, measure the next star in the coordinate list.
	    case 'm', 'n':

                # No coordinate file.
                if (cl == NULL) {
                    if (interactive == YES)
                        call printf ("No coordinate list\n")
                    goto endswitch_
                }

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

		# Move to next object.
		newlist = YES
		if (key == 'm') {
		    newbuf = YES
		    newfit = YES
                    goto endswitch_
		}

		# Measure next object.
	        ier = apsffit (ap, im, xlist, ylist)
		if (id != NULL) {
		    call appfmark (ap, id, apstati (ap, MKPSFBOX))
		    if (id == gd)
			call gflush (id)
		    else
			call gframe (id)
		}
		if (interactive == YES)
	            call ap_qppsf (ap, ier)
		if (stid == 1)
		    call ap_param (ap, out, "fitpsf")
		call ap_ppsf (ap, out, stid, ltid, ier)
		stid = stid + 1
		newbuf = NO
		newfit = NO

	    # Process the remainder of the coordinate list.
	    case 'l':
		if (cl != NULL) {
		    oid = stid
		    ltid = ltid + 1
		    call apbfitpsf (ap, im, cl, out, id, stid, ltid, YES)
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
		call printf ("Unknown keystroke command\n")
	    }

endswitch_
            # Setup for the next object by setting the default keystroke
            # command and storing the old cursor coordinates in the
            # centering structure.

	    key = ' '
	    Memc[cmd] = EOS
	    call apsetr (ap, WX, apstatr (ap, CWX))
	    call apsetr (ap, WY, apstatr (ap, CWY))

	}

	call sfree (sp)
end
