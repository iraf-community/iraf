include <ctype.h>
include <gset.h>
include "../lib/apphot.h"
include "../lib/display.h"
include "../lib/fitsky.h"

define	HELPFILE	"apphot$fitsky/fitsky.key"

# APSKY -- Procedure to interactively determine sky values in an annular
# region around a list of objects.

int procedure apsky (ap, im, cl, sd, gd, mgd, id, out, stid, interactive)

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

int	wcs, key, colonkey, newobject, newsky, newlist, ier
int	ip, ltid, oid, prev_num, req_num
pointer sp, cmd
real	wx, wy, xlist, ylist

int	clgcur(), apfitsky(), aprefitsky(), apgscur(), ctoi(), apstati()
int	apgqverify(), apgtverify(), apnew()
real	apstatr()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Initialize cursor command.
	key = ' '
	Memc[cmd] = EOS

	# Initialize fitting parameters.
	newobject = YES
	newsky = YES
	ier = AP_OK

	# Initialize sequencing.
	newlist = NO
	ltid = 0

	# Loop over the cursor commands.
	while (clgcur ("commands", wx, wy, wcs, key, Memc[cmd], SZ_LINE) !=
	    EOF) {

	    # Store the current cursor coordinates.
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
		if (id != NULL)
		    call gpagefile (id, HELPFILE, "")
		else if (interactive == YES)
		    call pagefile (HELPFILE, "[space=morehelp,q=quit,?=help]")

	    # Rewind the coordinate file.
	    case 'r':
		if (cl != NULL) {
		    call seek (cl, BOFL)
		    ltid = 0
		} else if (interactive == YES)
		    call printf ("No coordinate list\7\n")

	    # Verify the critical parameters.
	    case 'v':
		call ap_sconfirm (ap, out, stid)

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
		case 'm':

		    # Show/set fitsky parameters
		    if (Memc[cmd+ip] != EOS && Memc[cmd+ip] != ' ') {
		        call apskycolon (ap, im, cl, out, stid, ltid,
			    Memc[cmd], newobject, newsky)

		    # Get next object from the list.
		    } else if (cl != NULL) {
		        ip = ip + 1
			prev_num = ltid
			if (ctoi (Memc[cmd], ip, req_num) <= 0)
			    req_num = ltid + 1
		        if (apgscur (cl, id, xlist, ylist, prev_num, req_num,
			    ltid) != EOF) {
			    newobject = YES
			    newsky = YES
		   	    newlist = YES
			} else if (interactive == YES)
			    call printf (
			    "End of coordinate list, use r key to rewind\7\n")
		    } else if (interactive == YES)
		        call printf ("No coordinate list\7\n")

		case 'n':

		    # Show/set fitsky commands.
		    if (Memc[cmd+ip] != EOS && Memc[cmd+ip] != ' ') {
		        call apskycolon (ap, im, cl, out, stid, ltid,
			    Memc[cmd], newobject, newsky)

		    # Measure the sky for the next object in the list.
		    } else if (cl != NULL) {
		        ip = ip + 1
			prev_num = ltid
			if (ctoi (Memc[cmd], ip, req_num) <= 0)
			    req_num = ltid + 1

		        if (apgscur (cl, id, xlist, ylist, prev_num,
			    req_num, ltid) != EOF) {

		   	    newlist = YES
		            ier = apfitsky (ap, im, xlist, ylist, sd, gd)

			    call apmark (ap, id, NO, apstati (ap, MKSKY), NO)
			    call apsplot (ap, stid, ier, gd, apstati (ap,
			        RADPLOTS))
			    if (interactive == YES)
		                call ap_qspsky (ap, ier)

		            if (stid == 1)
				call ap_param (ap, out, "fitsky")
		            call ap_pssky (ap, out, stid, ltid, ier)
			    call apsplot (ap, stid, ier, mgd, YES)
		            stid = stid + 1
		            newobject = NO; newsky = NO
		        } else if (interactive == YES)
			    call printf (
			    "End of coordinate list, use r key to rewind\7\n")

		    } else if (interactive == YES)
		        call printf ("No coordinate list\7\n")

		default:
		    call apskycolon (ap, im, cl, out, stid, ltid, Memc[cmd],
		        newobject, newsky)
		}

	    # Fit the sky and store the results.
	    case 'f', ' ':
		if (newobject == YES)
		    ier = apfitsky (ap, im, wx, wy, sd, gd)
		else if (newsky == YES)
		    ier = aprefitsky (ap, gd)
		call apmark (ap, id, NO, apstati (ap, MKSKY), NO)
		call apsplot (ap, stid, ier, gd, apstati (ap, RADPLOTS))
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
		    call apsplot (ap, stid, ier, mgd, YES)
		    stid = stid + 1
		}

	    # Process the remainder of the list.
	    case 'l':
		if (cl != NULL) {
		    ltid = ltid + 1
		    oid = stid
		    call apbsky (ap, im, cl, sd, out, stid, ltid, gd, mgd, id,
		        YES)
		    ltid = ltid + stid - oid + 1
		} else if (interactive == YES)
		    call printf ("No coordinate list\7\n")

	    default:
		# do nothing
		call printf ("Unknown or ambiguous keystroke command\7\n")
	    }

	    # Setup for the next object.
	    key = ' '
	    Memc[cmd] = EOS
	    call apsetr (ap, WX, apstatr (ap, CWX))
	    call apsetr (ap, WY, apstatr (ap, CWY))

	}

	call sfree (sp)
end
