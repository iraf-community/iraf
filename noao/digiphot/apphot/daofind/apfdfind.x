include "../lib/apphot.h"
include "../lib/display.h"
include "../lib/find.h"

define	HELPFILE	"apphot$daofind/daofind.key"
define	SHELPFILE	"apphot$daofind/sdaofind.key"

# AP_FDFIND -- Procedure to find objects in an image interactively.

int procedure ap_fdfind (cnvname, ap, im, gd, mgd, id, out, boundary, constant,
        save, interactive)

char	cnvname[ARB]		# name of convolved image
pointer ap			# pointer to the apphot structure
pointer	im			# pointer to the IRAF image
pointer	gd			# pointer to the graphics stream
pointer	mgd			# pointer to the plot metacode stream
pointer	id			# pointer to the image display stream
int	out			# output file descriptor
int	boundary		# type of boundary extension
real	constant		# constatn for constant boundary extension
int	save			# save convolved image
int	interactive		# interactive mode

int	wcs, key, newconv, newfit, stid, stidp
pointer	sp, cmd, cnv
real	wx, wy

int	clgcur(), apgqverify(), apgtverify()
pointer	ap_cnvmap()
real	apstatr()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Initialize cursor command.
	key = ' '
	Memc[cmd] = EOS

	# Initialize fitting parameters.
	cnv = NULL
	newconv = YES
	newfit = YES
	stid = 1

	# Print a short help page.
	if (id != NULL)
	    call gpagefile (id, SHELPFILE, "")
	else if (interactive == YES)
	    call pagefile (SHELPFILE, "")

	# Loop over the cursor commands.
	while (clgcur ("commands", wx, wy, wcs, key, Memc[cmd], SZ_LINE) !=
	    EOF) {

	    # Store the current cursor coordinates.
	    call apsetr (ap, CWX, wx)
	    call apsetr (ap, CWY, wy)

	    # Process the colon commands.
	    switch (key) {

	    # Quit.
	    case 'q':
		if (interactive == YES && apgqverify ("daofind",
		    ap, key) == YES) {
		    call sfree (sp)
		    if (cnv != NULL) {
	    	        call imunmap (cnv)
		        if (save == NO)
	    	            call imdelete (cnvname)
		    }
		    return (apgtverify (key))
		} else {
		    call sfree (sp)
		    return (YES)
		}

	    # Get information on keystroke commands.
	    case '?':
		if (id != NULL)
		    call gpagefile (id, HELPFILE, "")
		else if (interactive == YES)
		    call pagefile (HELPFILE, "[space=morehelp,q=quit,?=help]")

	    # Plot a centered stellar radial profile
	    case 'd':
		call ap_qrad (ap, im, wx, wy, gd)
		newconv = YES
		newfit = YES

	    # Interactively set the daofind parameters.
	    case 'i':
		call ap_fdradsetup (ap, im, wx, wy, gd, out, stid)
		newconv = YES
		newfit = YES

	    # Verify the critical daofind parameters.
	    case 'v':
		call ap_fdconfirm (ap)
		newconv = YES
		newfit = YES

	    # Save daofind parameters in the pset files.
	    case 'w':
		call ap_fdpars (ap)

	    # Process apphot : commands.
	    case ':':
		call ap_fdcolon (ap, im, out, stid, Memc[cmd], newconv, newfit)

	    # Find the stars.
	    case 'f':
		stidp = 1
		if (newconv == YES) {
		    cnv = ap_cnvmap (cnvname, im, ap, save)
		    #if (cnv != NULL) {
			#call imunmap (cnv)
		        #call imdelete (cnvname)
		    #}
		    #cnv = immap (cnvname, NEW_COPY, im)
		    call ap_fdstars (im, ap, cnv, NULL, id, boundary, constant,
		        NO, stidp)
		} else if (newfit == YES) {
		    call ap_fdstars (im, ap, cnv, NULL, id, boundary, constant,
		        YES, stidp)
		} else {
		    call printf ("Detection parameters have not changed\7\n")
		}
		newconv = NO; newfit = NO

	    # Find the stars and store the results.
	    case ' ':
		if (newconv == YES) {
		    cnv = ap_cnvmap (cnvname, im, ap, save)
		    #if (cnv != NULL) {
			#call imunmap (cnv)
			#call imdelete (cnvname)
		    #}
		    #cnv = immap (cnvname, NEW_COPY, im)
		    call ap_fdstars (im, ap, cnv, out, id, boundary, constant,
		        NO, stid)
		} else if (newfit == YES) {
		    call ap_fdstars (im, ap, cnv, out, id, boundary, constant,
		        YES, stid)
		} else {
		    call printf ("Detection parameters have not changed\7\n\n")
		    #call ap_fdstars (im, ap, cnv, out, id, boundary, constant,
		        #YES, stid)
		}
		newconv = NO; newfit = NO

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

end
