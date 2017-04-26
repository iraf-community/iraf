include <ctype.h>
include <gset.h>
include <imhdr.h>
include "../lib/apphot.h"
include "../lib/display.h"
include "../lib/center.h"

define	HELPFILE	"apphot$center/center.key"

# APCENTER -- Procedure to determine accurate centers for a list of objects
# interactively.

int procedure apcenter (ap, im, cl, gd, mgd, id, out, stid, interactive, cache)

pointer ap			# pointer to the apphot structure
pointer	im			# pointer to the IRAF image
int	cl			# coordinate list file descriptor
pointer	gd			# pointer to the graphics stream
pointer	mgd			# pointer to the plot metacode stream
pointer	id			# pointer to the image display stream
int	out			# output file descriptor
int	stid			# output file sequence number
int	interactive		# interactive mode
int	cache			# cache the input pixels

real	wx, wy, xlist, ylist
pointer	sp, cmd
int	wcs, key, colonkey, newimage, newobject, newcenter, newlist, ier, oid
int	ip, prev_num, req_num, ltid, buf_size, old_size, req_size, memstat

real	apstatr()
int	ctoi(), clgcur(), apgscur(), apfitcenter(), aprefitcenter(), apstati()
int	apgqverify(), apgtverify(), apnew(), ap_memstat(), sizeof()

define	endswitch_ 99

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Initialize cursor command.
	key = ' '
	Memc[cmd] = EOS

	# Initialize fitting parameters.
	newimage = NO
	newobject = YES
	newcenter = YES
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
		if ((id != NULL) && (id == gd))
		    call gpagefile (id, HELPFILE, "")
		else if (interactive == YES)
		    call pagefile (HELPFILE, "[space=cmhelp,q=quit,?=help]")

	    # Rewind the coord list.
	    case 'r':
		if (cl != NULL) {
		    call seek (cl, BOF)
		    ltid = 0
		} else if (interactive == YES)
		    call printf ("No coordinate list\n")

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
		newobject = YES
		newcenter = YES

	    # Measure the next object.
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
		if (apgscur (cl, id, xlist, ylist, prev_num, req_num, ltid) ==
		    EOF) {
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
		    newobject = YES
		    newcenter = YES
		    goto endswitch_
		} 

		# Measure next object.
		ier = apfitcenter (ap, im, xlist, ylist)
		if (id != NULL) {
		    call apmark (ap, id, apstati (ap, MKCENTER), NO, NO)
		    if (id == gd)
		        call gflush (id)
		    else
		        call gframe (id)
		}
		call ap_cplot (ap, stid, gd, apstati (ap, RADPLOTS))
		if (interactive == YES)
		    call ap_qcenter (ap, ier)
		if (stid == 1)
		     call ap_param (ap, out, "center")
		call ap_cplot (ap, stid, mgd, YES)
		call ap_pcenter (ap, out, stid, ltid, ier)
		stid = stid + 1
		newobject = NO
		newcenter = NO

	    # Save centering parameters in the pset files.
	    case 'w':
		call ap_pcpars (ap)

	    # Process apphot : commands.
	    case ':':
		for (ip = 1; IS_WHITE(Memc[cmd+ip-1]); ip = ip + 1)
		    ;
		colonkey = Memc[cmd+ip-1]

		# Process the apphot :n command.
		switch (colonkey) {
		case 'm', 'n':

		    # Show/set apphot commands
		    if (Memc[cmd+ip] != EOS && Memc[cmd+ip] != ' ') {
		        call apcentercolon (ap, im, cl, out, stid, ltid,
			    Memc[cmd], newimage, newobject, newcenter)
			goto endswitch_
		    }

		    # No coordinate list.
		    if (cl == NULL) {
		        if (interactive == YES)
		            call printf ("No coordinate list\n")
			goto endswitch_
		    }

		    # Get next object from list.
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
			newcenter = YES
			goto endswitch_
		    }

		    # Measure next object.
		    ier = apfitcenter (ap, im, xlist, ylist)
		    if (id != NULL) {
		        call apmark (ap, id, apstati (ap, MKCENTER), NO, NO)
		    	if (id == gd)
			   call gflush (id)
		    	else
		           call gframe (id)
		    }
		    call ap_cplot (ap, stid, gd, apstati (ap, RADPLOTS))
		    if (interactive == YES)
		        call ap_qcenter (ap, ier)
		    if (stid == 1)
		        call ap_param (ap, out, "center")
		    call ap_cplot (ap, stid, mgd, YES)
		    call ap_pcenter (ap, out, stid, ltid, ier)
		    stid = stid + 1
		    newobject = NO
		    newcenter = NO

		# Show/set apphot parameters.
		default:
		    call apcentercolon (ap, im, cl, out, stid, ltid, Memc[cmd],
		        newimage, newobject, newcenter)
		}

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

	    # Fit the center, and save the results.
	    case 'f',  ' ':

		# Compute the center.
		if (newobject == YES)
		    ier = apfitcenter (ap, im, wx, wy)
		else if (newcenter == YES)
		    ier = aprefitcenter (ap, im, ier)
		if (id != NULL) {
		    call apmark (ap, id, apstati (ap, MKCENTER), NO, NO)
		    if (id == gd)
			call gflush (id)
		    else
		        call gframe (id)
		}
		call ap_cplot (ap, stid, gd, apstati (ap, RADPLOTS))
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
		    call ap_cplot (ap, stid, mgd, YES)
		    stid = stid + 1
		}

	    # Fit centers for the rest of the list.
	    case 'l':

		# No coordinate list.
		if (cl == NULL) {
		    if (interactive == YES)
		        call printf ("No coordinate list\n")
		    goto endswitch_
		}

		# Center rest of coordinate list.
		ltid = ltid + 1
		oid = stid
		call apbcenter (ap, im, cl, out, stid, ltid, mgd, id, YES)
		ltid = ltid + stid - oid + 1
		if (id != NULL) {
		    if (id == gd)
		        call gflush (id)
		    else
		        call gframe (id)
		}

	    # Unknown command.
	    default:
		call printf ("Unknown or ambiguous keystroke command\n")
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
