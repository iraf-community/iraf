include <imhdr.h>
include "../lib/apphot.h"
include "../lib/display.h"
include "../lib/find.h"

define	HELPFILE	"apphot$daofind/daofind.key"

# AP_FDFIND -- Find objects in an image interactively.

int procedure ap_fdfind (denname, skyname, ap, im, gd, id, out, boundary,
	constant, save, skysave, interactive, cache)

char	denname[ARB]		# name of density enhancement image
char	skyname[ARB]		# name of the fitted sky image
pointer ap			# pointer to the apphot structure
pointer	im			# pointer to the IRAF image
pointer	gd			# pointer to the graphics stream
pointer	id			# pointer to the image display stream
int	out			# output file descriptor
int	boundary		# type of boundary extension
real	constant		# constatn for constant boundary extension
int	save			# save convolved image
int	skysave			# save the sky image
int	interactive		# interactive mode
int	cache			# cache the input image pixels

real	wx, wy
pointer	sp, cmd, root, den, sky
int	wcs, key, newimage, newden, newfit, stid, memstat, req_size, old_size
int	buf_size

real	apstatr()
pointer	ap_immap()
int	clgcur(), apgqverify(), apgtverify(), sizeof(), ap_memstat()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call salloc (root, SZ_FNAME, TY_CHAR)

	# Initialize cursor command.
	key = ' '
	Memc[cmd] = EOS
	call strcpy (" ", Memc[root], SZ_FNAME)

	# Initialize fitting parameters.
	den = NULL
	sky = NULL
	newimage = NO
	newden = YES
	newfit = YES
	memstat = NO

	# Loop over the cursor commands.
	stid = 1
	while (clgcur ("icommands", wx, wy, wcs, key, Memc[cmd], SZ_LINE) !=
	    EOF) {

	    # Store the current cursor coordinates.
	    call ap_vtol (im, wx, wy, wx, wy, 1)
	    call apsetr (ap, CWX, wx)
	    call apsetr (ap, CWY, wy)

	    # Process the colon commands.
	    switch (key) {

	    # Quit.
	    case 'q':
		if (interactive == YES && apgqverify ("daofind",
		    ap, key) == YES) {
		    call sfree (sp)
		    if (den != NULL) {
	    	        call imunmap (den)
		        if (save == NO)
	    	            call imdelete (denname)
		    }
		    if (sky != NULL)
			call imunmap (sky)
		    return (apgtverify (key))
		} else {
		    if (den != NULL) {
	    	        call imunmap (den)
		        if (save == NO)
	    	            call imdelete (denname)
		    }
		    if (sky != NULL)
			call imunmap (sky)
		    call sfree (sp)
		    return (YES)
		}

	    # Get information on keystroke commands.
	    case '?':
		if ((id != NULL) && (gd == id))
		    call gpagefile (id, HELPFILE, "")
		else if (interactive == YES)
		    call pagefile (HELPFILE, "[space=morehelp,q=quit,?=help]")

	    # Plot a centered stellar radial profile
	    case 'd':
		if (interactive == YES)
		    call ap_qrad (ap, im, wx, wy, gd)

	    # Interactively set the daofind parameters.
	    case 'i':
		if (interactive == YES) {
		    call ap_fdradsetup (ap, im, wx, wy, gd, out, stid)
		    newden = YES
		    newfit = YES
		}

	    # Verify the critical daofind parameters.
	    case 'v':
		call ap_fdconfirm (ap)
		newden = YES
		newfit = YES

	    # Save daofind parameters in the pset files.
	    case 'w':
		call ap_fdpars (ap)

	    # Process apphot : commands.
	    case ':':
		call ap_fdcolon (ap, im, out, stid, Memc[cmd], newimage,
		    newden, newfit)

		# Determine the viewport and data window of image display.
		if (newimage == YES) {
		    if ((id != NULL) && (id != gd))
		        call ap_gswv (id, Memc[cmd], im, 4)
            	    req_size = MEMFUDGE * (IM_LEN(im,1) * IM_LEN(im,2) *
                	sizeof (IM_PIXTYPE(im)) + 2 * IM_LEN(im,1) *
			IM_LEN(im,2) * sizeof (TY_REAL))
                    memstat = ap_memstat (cache, req_size, old_size)
                    if (memstat == YES)
                        call ap_pcache (im, INDEFI, buf_size)
		}
		newimage = NO

	    # Find the stars.
	    case 'f', ' ':

		if (newden == YES) {

		    if (den != NULL) {
			call imunmap (den)
		        call imdelete (denname)
		    }
		    den = ap_immap (denname, im, ap, save)
                    if (memstat == YES)
                        call ap_pcache (den, INDEFI, buf_size)

		    if (sky != NULL)
			call imunmap (sky)
		    if (skysave == YES) {
		        sky = ap_immap (skyname, im, ap, YES)
                        if (memstat == YES)
                            call ap_pcache (den, INDEFI, buf_size)
		    } else
			sky = NULL
		    newden = NO

		    if (key == 'f') {
		        call ap_fdstars (im, ap, den, sky, NULL, id,
			    boundary, constant, NO, stid)
		    } else {
		        call ap_outmap (ap, out, Memc[root])
		        call ap_fdstars (im, ap, den, sky, out, id,
			    boundary, constant, NO, stid)
			newfit = NO
		    }

		} else if (newfit == YES) {

		    if (key == 'f') {
		        call ap_fdstars (im, ap, den, sky, NULL, id,
			    boundary, constant, YES, stid)
		    } else {
		        call ap_outmap (ap, out, Memc[root])
		        call ap_fdstars (im, ap, den, sky, out, id,
			    boundary, constant, YES, stid)
			newfit = NO
		    }

		} else {
		    call printf ("Detection parameters have not changed\n")
		}

		if (id != NULL) {
		    if (id == gd)
			call gflush (id)
		    else
			call gframe (id) 
		}

		stid = 1
		#newden = NO
		#newfit = NO

	    default:
		# do nothing
		call printf ("Unknown or ambiguous keystroke command\n")
	    }

	    # Setup for the next object.
	    key = ' '
	    Memc[cmd] = EOS
	    call apsetr (ap, WX, apstatr (ap, CWX))
	    call apsetr (ap, WY, apstatr (ap, CWY))

	}

end
