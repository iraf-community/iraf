include <ctype.h>
include <gset.h>
include "../lib/apphot.h"
include "../lib/display.h"
include "../lib/center.h"

define	HELPFILE	"apphot$center/center.key"

# APCENTER -- Procedure to determine accurate centers for a list of objects
# interactively.

int procedure apcenter (ap, im, cl, gd, mgd, id, out, stid, interactive)

pointer ap			# pointer to the apphot structure
pointer	im			# pointer to the IRAF image
int	cl			# coordinate list file descriptor
pointer	gd			# pointer to the graphics stream
pointer	mgd			# pointer to the plot metacode stream
pointer	id			# pointer to the image display stream
int	out			# output file descriptor
int	stid			# output file sequence number
int	interactive		# interactive mode

int	wcs, key, colonkey, newobject, newcenter, newlist, ier, oid
int	ip, prev_num, req_num, ltid
pointer	sp, cmd
real	wx, wy, xlist, ylist

int	ctoi(), clgcur(), apgscur(), apfitcenter(), aprefitcenter(), apstati()
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
	newcenter = YES
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
		newcenter = YES
	    }

	    # Process the colon commands.
	    switch (key) {

	    # Quit.
	    case 'q':
		if (interactive == YES) {
		    if (apgqverify ("center", ap, key) == YES) {
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
		    call ap_cerrors (ap, ier)

	    # Get information on keystroke commands.
	    case '?':
		if (id != NULL)
		    call gpagefile (id, HELPFILE, "")
		else if (interactive == YES)
		    call pagefile (HELPFILE, "[space=cmhelp,q=quit,?=help]")

	    # Rewind the coord list.
	    case 'r':
		if (cl != NULL) {
		    call seek (cl, BOF)
		    ltid = 0
		} else if (interactive == YES)
		    call printf ("No coordinate list\7\n")

	    # Draw a centered stellar radial profile plot.
	    case 'd':
		if (interactive == YES) {
		    call ap_qrad (ap, im, wx, wy, gd)
		    newobject = YES
		    newcenter = YES
		}

	    # Interactively set the centering parameters.
	    case 'i':
		if (interactive == YES) {
		    call ap_cradsetup (ap, im, wx, wy, gd, out, stid)
		    newobject = YES
		    newcenter = YES
		}

	    # Verify critical parameters.
	    case 'v':
		call ap_cconfirm (ap, out, stid)

	    # Save centering parameters in the pset files.
	    case 'w':
		call ap_pcpars (ap)

	    # Process apphot : commands.
	    case ':':
		for (ip = 1; IS_WHITE(Memc[cmd+ip-1]); ip = ip + 1)
		    ;
		colonkey = Memc[cmd+ip-1]

		switch (colonkey) {
		# Process the :m command
		case 'm':

		    # Show/set apphot parameters.
		    if (Memc[cmd+ip] != EOS && Memc[cmd+ip] != ' ') {
		        call apcentercolon (ap, im, cl, out, stid, ltid,
			    Memc[cmd], newobject, newcenter)

		    # Get the nth object from the list.
		    } else if (cl != NULL) {
			ip = ip + 1
			prev_num = ltid
			if (ctoi (Memc[cmd], ip, req_num) <= 0)
			    req_num = ltid + 1
		        if (apgscur (cl, id, xlist, ylist, prev_num, req_num,
			    ltid) != EOF) {
		            newobject = YES
			    newcenter = YES
			    newlist = YES
			} else if (interactive == YES)
			    call printf (
			    "End of coordinate list, use r key to rewind\7\n")
		    } else if (interactive == YES)
		        call printf ("No coordinate list\7\n")

		# Process the apphot :n command.
		case 'n':
		    # Show/set apphot commands
		    if (Memc[cmd+ip] != EOS && Memc[cmd+ip] != ' ') {
		        call apcentercolon (ap, im, cl, out, stid, ltid,
			    Memc[cmd], newobject, newcenter)

		    # Measure the nth object in the list.
		    } else if (cl != NULL) {

			# Get the center coordinates
			ip = ip + 1
			prev_num = ltid
			if (ctoi (Memc[cmd], ip, req_num) <= 0)
			    req_num = ltid + 1

			# Fit the center, save results, setup for next object.
		        if (apgscur (cl, id, xlist, ylist, prev_num, req_num,
			    ltid) != EOF) {

		    	    newlist = YES
		    	    ier = apfitcenter (ap, im, xlist, ylist)

			    call apmark (ap, id, apstati (ap, MKCENTER), NO, NO)
			    call apcplot (ap, stid, ier, gd, apstati (ap,
			        RADPLOTS))
			    if (interactive == YES)
		    	        call ap_qcenter (ap, ier)

		    	    if (stid == 1)
				call ap_param (ap, out, "center")
			    call apcplot (ap, stid, ier, mgd, YES)
			    call ap_pcenter (ap, out, stid, ltid, ier)
		    	    stid = stid + 1
		    	    newobject = NO
			    newcenter = NO

			} else if (interactive == YES)
			    call printf (
			    "End of coordinate list, use r key to rewind\7\n")

		    } else if (interactive == YES)
		        call printf ("No coordinate list\7\n")

		# Show/set apphot parameters.
		default:
		    call apcentercolon (ap, im, cl, out, stid, ltid, Memc[cmd],
		        newobject, newcenter)
		}

	    # Fit the center, and save the results.
	    case 'f',  ' ':

		# Compute the center.
		if (newobject == YES)
		    ier = apfitcenter (ap, im, wx, wy)
		else if (newcenter == YES)
		    ier = aprefitcenter (ap, ier)
		call apmark (ap, id, apstati (ap, MKCENTER), NO, NO)
		call apcplot (ap, stid, ier, gd, apstati (ap, RADPLOTS))
		if (interactive == YES)
		    call ap_qcenter (ap, ier)
		newobject = NO; newcenter = NO

		# Write the results to the output file(s).
		if (key == ' ') {
		    if (stid == 1)
		        call ap_param (ap, out, "center")
		    if (newlist == YES)
		        call ap_pcenter (ap, out, stid, ltid, ier)
		    else
		        call ap_pcenter (ap, out, stid, 0, ier)
		    call apcplot (ap, stid, ier, mgd, YES)
		    stid = stid + 1
		}

	    # Fit centers for the rest of the list.
	    case 'l':

		if (cl != NULL) {
		    ltid = ltid + 1
		    oid = stid
		    call apbcenter (ap, im, cl, out, stid, ltid, mgd, id, YES)
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
