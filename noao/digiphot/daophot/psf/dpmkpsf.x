include <gset.h>
include <ctype.h>
include	<imhdr.h>
include "../lib/daophotdef.h"
include "../lib/apseldef.h"
include "../lib/psfdef.h"

define	HELPFILE	"daophot$psf/mkpsf.key"

# DP_MKPSF -- Construct a stellar PSF from one or more stars in an image frame.

procedure dp_mkpsf (dao, im, psfim, opst, psfgr, gd, mgd, id, mkstars,
	interactive, showplots)

pointer	dao			# pointer to the main daophot structure
pointer im			# pointer to the input image
pointer	psfim			# pointer to the output psfimage
int	opst			# the psf star list file descriptor
int	psfgr			# the psf group file descriptor
pointer gd			# pointer to graphics descriptor
pointer mgd			# pointer to the metacode file
pointer id			# pointer to image display stream
bool	mkstars			# mark the added and deleted psf stars
bool	interactive		# interactive mode
bool	showplots		# show the plots

real	wx, wy
pointer apsel, sp, cmd, str
int	i, key, wcs, idnum, istar, ip, npsf, verbose
bool	psf_new, psf_computed, psf_written

real	dp_pstatr()
int	clgcur(), dp_qverify(), dp_locstar(), dp_idstar(), dp_stati()
int	dp_pstati(), ctoi(), dp_addstar(), dp_delstar(), dp_subpsf()
int	dp_fitpsf()
bool	dp_updatepsf()

begin
	# Get some pointers
	apsel = DP_APSEL(dao)

	# Allocate some working space.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Initialize some variables.
	key = 'a'
	if (dp_pstati (dao, PNUM) > 0)
	    psf_new = false
	else
	    psf_new = true
	psf_computed = false
	psf_written = false

	# Begin to build the PSF.
	while (clgcur ("icommands", wx, wy, wcs, key, Memc[cmd], SZ_LINE) !=
	    EOF) {

	    # Convert coordinates if necessary.
	    call dp_vtol (im, wx, wy, wx, wy, 1)

	    switch (key) {

	    # Quit the interactive cursor loop.
	    case 'q':

		if (interactive) {
		    if (dp_qverify (dao, im, psfim, opst, psfgr, psf_new,
		        psf_computed, psf_written) == NO) 
			next
		} else if (dp_updatepsf (dao, im, psfim, opst, psfgr, psf_new,
		    psf_computed, psf_written)) {
		    psf_computed = true
		    psf_written = true
		}

		# Delete any empty psf and group files lying around.
		if (! psf_written)
		    call dp_rmpsf (dao, psfim, opst, psfgr)

		# Delete the PSF stars. 
		call dp_pseti (dao, PNUM, 0)

		break

	    # Print the help page.
	    case '?':
		if (id == NULL)
		    call pagefile (HELPFILE, "")
		else
		    call gpagefile (id, HELPFILE, "")

	    # Add the star nearest the cursor position to the PSF.
	    case 'a':
		if (dp_addstar (dao, im, wx, wy, INDEFR, 0, gd, mgd,
		    showplots) == ERR) 
		    next
		psf_new = false
		psf_computed = false
		psf_written = false
                if (mkstars && id != NULL) {
                    call gmark (id, dp_pstatr (dao, CUR_PSFX), dp_pstatr (dao,
                        CUR_PSFY), GM_PLUS, -5.0, -5.0)
                    if (gd == id)
                        call gflush (id)
                    else
                        call gframe (id)
		}

	    # Subtract the star nearest the cursor position from the PSF
	    # using the current best fit.
	    case 's':
		if (psf_new) {
		    call printf ("The PSF star list is empty\n")
		    next
		}
		if (! psf_computed) {
		    call printf ("The PSF is not uptodate\n")
		    next
		}
		if (dp_subpsf (dao, im, wx, wy, 0, gd, mgd, showplots) == ERR)
		    next
		if (dp_pstati (dao, PNUM) <= 0)
		    psf_new = true
		psf_computed = false
		psf_written = false

	    # Delete the star nearest the cursor position from the PSF.
	    case 'd':
		if (dp_delstar (dao, im, wx, wy, 0, gd, showplots) == ERR)
		    next
		if (dp_pstati (dao, PNUM) <= 0)
		    psf_new = true
		psf_computed = false
		psf_written = false
                if (mkstars && id != NULL) {
                    call gmark (id, dp_pstatr (dao, CUR_PSFX), dp_pstatr (dao,
                        CUR_PSFY), GM_CROSS, -5.0, -5.0)
                    if (gd == id)
                        call gflush (id)
                    else
                        call gframe (id)
                }

	    # List all the current psf stars.
	    case 'l':
		if (interactive)
		    call dp_listpsf (dao, im)

	    # Fit the PSF.
	    case 'f':

		# Reread the stars is necessary.
		if (dp_pstati (dao, PNUM) > 0 && (psf_new || psf_computed)) {
		    npsf = dp_pstati (dao, PNUM)
		    call dp_pseti (dao, PNUM, 0)
		    call dp_reinit (dao)
		    verbose = dp_stati (dao, VERBOSE)
		    call dp_seti (dao, VERBOSE, NO)
		    do i = 1, npsf
		        if (dp_addstar (dao, im, wx, wy, INDEFR,
		            Memi[DP_APID(apsel)+i-1], gd, mgd,
			    false) == OK)
			    psf_new = false
		    call dp_seti (dao, VERBOSE, verbose)
		}

		# Fit the psf.
		if (psf_new) {
		    call printf ("The PSF star list is empty\n")
		} else if (dp_fitpsf (dao, im, Memc[str], SZ_LINE) == OK) {
		    psf_computed = true
		} else {
		    call printf ("%s\n")
			call pargstr (Memc[str])
		}

	    # Review the fit. 
	    case 'r':
		if (psf_new) {
		    call printf ("The PSF star list is empty\n")
		} else if (! psf_computed) {
		    call printf ("The PSF is not uptodate\n")
		} else {
		    i = 1
		    repeat {
		        if (dp_subpsf (dao, im, wx, wy,
			    Memi[DP_APID(apsel)+i-1], gd, mgd,
			    showplots) == OK) {
		            if (dp_pstati (dao, PNUM) <= 0)
		                psf_new = true
		            psf_computed = false
		            psf_written = false
			} else
			    i = i + 1
		    } until (i > dp_pstati(dao, PNUM))
		}

	    # Rebuild the PSF from scratch.
	    case 'z':

		# Print message.
		if (interactive) {
		    call dp_stats (dao, OUTPHOTFILE, Memc[str], SZ_FNAME)
		    call printf (
		        "PSF star list, image, and output files deleted\n")
		}

		# Delete the previous psf an dgroup files if any.
		call dp_rmpsf (dao, psfim, opst, psfgr)

		# Delete the PSF stars. 
		call dp_pseti (dao, PNUM, 0)

		# Reopen the psf image and group files.
		call dp_oppsf (dao, psfim, opst, psfgr)

		# Reset the reduction flags.
		psf_new = true
		psf_computed = false
		psf_written = false

	    # Write out the PSF.
	    case 'w':
		if (dp_updatepsf (dao, im, psfim, opst, psfgr, psf_new,
		    psf_computed, psf_written)) {
		    psf_computed = true
		    psf_written = true
		}

	    # Locate the star in the aperture photometry file and print out
	    # the photometry.
	    case 'p':
		if (interactive)
		    istar = dp_locstar (dao, im, wx, wy)
		else
		    next
		if (istar > 0)
		    call dp_pshow (dao, im, istar)
		else if (istar == 0)
		    call printf ("Star not found in the photometry file.\n")
		else
		    call printf (
		        "Star off or too near the edge of the image.\n")

	    # Command mode
	    case ':':
		for (ip = 1; IS_WHITE(Memc[cmd+ip-1]); ip = ip + 1)
		    ;
		switch (Memc[cmd+ip-1]) {

		case 'p':
		    if (Memc[cmd+ip] != EOS && Memc[cmd+ip] != ' ') {
			call dp_pcolon (dao, psfim, opst, psfgr, Memc[cmd],
			    psf_new, psf_computed, psf_written)
			next
		    }
		    if (! interactive)
			next
		    ip = ip + 1
		    if (ctoi (Memc[cmd], ip, idnum) <= 0)
		        istar = dp_locstar (dao, im, wx, wy)
		    else
		        istar = dp_idstar (dao, im, idnum)
		    if (istar > 0)
		        call dp_pshow (dao, im, istar)
		    else if (istar == 0)
		        call printf (
		            "Star not found in the photometry file\n")
		    else
		        call printf (
		            "Star is off or too near the edge of the image.\n")

		case 'a':
		    if (Memc[cmd+ip] != EOS && Memc[cmd+ip] != ' ') {
			call dp_pcolon (dao, psfim, opst, psfgr, Memc[cmd],
			    psf_new, psf_computed, psf_written)
			next
		    }
		    ip = ip + 1
		    if (ctoi (Memc[cmd], ip, idnum) <= 0)
		        idnum = 0
		    if (dp_addstar (dao, im, wx, wy, INDEFR, idnum, gd, mgd,
		        showplots) == ERR) 
			next
		    psf_new = false
		    psf_computed = false
		    psf_written = false
                    if (mkstars && id != NULL) {
                        call gmark (id, dp_pstatr (dao, CUR_PSFX),
                            dp_pstatr (dao, CUR_PSFY), GM_PLUS, -5.0, -5.0)
                        if (gd == id)
                            call gflush (id)
                        else
                            call gframe (id)
                    }

		case 's':
		    if (Memc[cmd+ip] != EOS && Memc[cmd+ip] != ' ') {
			call dp_pcolon (dao, psfim, opst, psfgr, Memc[cmd],
			    psf_new, psf_computed, psf_written)
			next
		    }
		    ip = ip + 1
		    if (ctoi (Memc[cmd], ip, idnum) <= 0)
			idnum = 0
		    if (! psf_computed) {
		        call printf ("The PSF has not been fit\n")
			next
		    }
		    if (! psf_computed) {
		        call printf ("Warning: The PSF is not uptodate\n")
			next
		    }
		    if (dp_subpsf (dao, im, wx, wy, idnum, gd, mgd,
		        showplots) == ERR)
			next
		    if (dp_pstati (dao, PNUM) <= 0)
		        psf_new = true
		    psf_computed = false
		    psf_written = false

		case 'd':
		    if (Memc[cmd+ip] != EOS && Memc[cmd+ip] != ' ') {
			call dp_pcolon (dao, psfim, opst, psfgr, Memc[cmd],
			    psf_new, psf_computed, psf_written)
			next
		    }
		    ip = ip + 1
		    if (ctoi (Memc[cmd], ip, idnum) <= 0)
		        idnum = 0
		    if (dp_delstar (dao, im, wx, wy, idnum, gd,
		        showplots) == ERR) 
			next
		    if (dp_pstati (dao, PNUM) <= 0)
		        psf_new = true
		    psf_computed = false
		    psf_written = false
                    if (mkstars && id != NULL) {
                        call gmark (id, dp_pstatr (dao, CUR_PSFX),
                            dp_pstatr (dao, CUR_PSFY), GM_CROSS, -5.0, -5.0)
                        if (gd == id)
                            call gflush (id)
                        else
                            call gframe (id)
                    }

		default:
		    call dp_pcolon (dao, psfim, opst, psfgr, Memc[cmd],
		        psf_new, psf_computed, psf_written)
		}

	    default:
		call printf ("Unknown keystroke command\n")
	    }
	}

	# Free up memory. 
	call sfree (sp)
end
