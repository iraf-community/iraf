include <ctype.h>
include <gset.h>
include "../lib/apphot.h"
include "../lib/display.h"
include "../lib/fitpsf.h"

define	HELPFILE	"apphot$fitpsf/fitpsf.key"

# APFITPSF -- Procedure to fit a functional form to the PSF for a list of
# objects in interactive mode.

int procedure apfitpsf (ap, im, cl, gd, id, out, stid, interactive)

pointer ap			# pointer to apphot structure
pointer	im			# pointer to IRAF image
int	cl			# starlist file descriptor
pointer	gd			# graphics pointer
pointer	id			# display pointer
int	out			# output file descriptor
int	stid			# output file sequence number
int	interactive		# interactive mode

int	wcs, key, newimage, newbuf, newfit, newlist, ltid, ier
int	ip, oid, colonkey, prev_num, req_num
pointer	sp, cmd
real	wx, wy, xlist, ylist

int	clgcur(), apgscur(), apsffit(), apsfrefit(), apstati()
int	apgqverify(), apgtverify(), ctoi(), apnew()
real	apstatr()

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
		    call printf ("No coordinate list\7\n")

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

		    # Get next object from the list.
		    } else if (cl != NULL) {
		        ip = ip + 1
		        prev_num = ltid
		        if (ctoi (Memc[cmd], ip, req_num) <= 0)
			    req_num = ltid + 1
		        if (apgscur (cl, id, xlist, ylist, prev_num,
			    req_num, ltid) != EOF) {
		            newlist = YES
			    if (colonkey == 'm') {
			        newbuf = YES
			        newfit = YES
			    } else {
	            	        ier = apsffit (ap, im, xlist, ylist)
			        if (interactive == YES)
	            	            call ap_qppsf (ap, ier)
			        call appfmark (ap, id, apstati (ap, MKPSFBOX))
			        if (id != NULL) {
		    		    if (id == gd)
				        call gflush (id)
		    		    else
				        call gframe (id)
			        }
		    	        if (stid == 1)
				    call ap_param (ap, out, "fitpsf")
		                call ap_ppsf (ap, out, stid, ltid, ier)
		                stid = stid + 1
		                newbuf = NO
			        newfit = NO
			    }
			} else if (interactive == YES)
			    call printf (
			    "End of coordinate list, use r key to rewind\7\n")

		    } else if (interactive == YES)
		        call printf ("No coordinate list\7\n")

		default:
		    call apsfcolon (ap, im, cl, out, stid, ltid, Memc[cmd],
		        newimage, newbuf, newfit)
		}

		# Reestablish the image viewport and window.
		if ((newimage == YES) && (id != NULL) && (id != gd)) {
		    call apstats (ap, IMNAME, Memc[cmd], SZ_LINE)
		    call ap_gswv (id, Memc[cmd], im, 4)
		    newimage = NO
		}

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
	            ier = apsfrefit (ap)
		if (interactive == YES)
		    call ap_qppsf (ap, ier)
		call appfmark (ap, id, apstati (ap, MKPSFBOX))
		if (id != NULL) {
		    if (id == gd)
			call gflush (id)
		    else
			call gframe (id)
		}
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
		if (cl != NULL) {
		    prev_num = ltid
		    req_num = ltid + 1
		    if (apgscur (cl, id, xlist, ylist, prev_num, req_num,
		        ltid) != EOF) {
		        newlist = YES
			if (key == 'm') {
			    newbuf = YES
			    newfit = YES
			} else {
	            	    ier = apsffit (ap, im, xlist, ylist)
			    if (interactive == YES)
	            	        call ap_qppsf (ap, ier)
			    call appfmark (ap, id, apstati (ap, MKPSFBOX))
			    if (id != NULL) {
		    		if (id == gd)
				    call gflush (id)
		    		else
				    call gframe (id)
			    }
		    	    if (stid == 1)
			        call ap_param (ap, out, "fitpsf")
		            call ap_ppsf (ap, out, stid, ltid, ier)
		            stid = stid + 1
		            newbuf = NO
			    newfit = NO
			}
		    } else if (interactive == YES)
			call printf (
			    "End of coordinate list, use r key to rewind\7\n")

		} else if (interactive == YES)
		    call printf ("No coordinate list\7\n")

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
		    call printf ("No coordinate list\7\n")

	    default:
		# do nothing
		call printf ("Unknown keystroke command\7\n")
	    }

	    key = ' '
	    Memc[cmd] = EOS
	    call apsetr (ap, WX, apstatr (ap, CWX))
	    call apsetr (ap, WY, apstatr (ap, CWY))

	}

	call sfree (sp)
end
