include <fset.h>
include <ctype.h>
include <gset.h>
include	<imhdr.h>
include <math.h>
include "../lib/daophotdef.h"
include "../lib/daophot.h"
include "../lib/psfdef.h"
include	"../lib/apsel.h"

define	HELPFILE	"daophot$psf/mkpsf.key"

# DP_MKPSF -- Construct a stellar PSF from one or more stars
# in an image frame.

procedure dp_mkpsf (dao, im, psfim, psfgr, gd, mgd, id, interactive, showplots)

pointer	dao			# pointer to DAOPHOT structure
pointer im			# pointer to IRAF image
pointer	psfim			# pointer to the psfimage
int	psfgr			# pointer to the psf group file
pointer gd			# pointer to graphics descriptor
pointer mgd			# pointer to the metacode file
pointer id			# pointer to image display stream
bool	interactive		# interactive mode
bool	showplots		# show the plots

bool	newpsf, psf_written
int	ip, wcs, key, idnum, istar
real	wx, wy
pointer sp, cmd, str, apsel

int	clgcur(), ctoi(), open()
int	dp_addpsf(), dp_newpsf(), dp_locstar(), dp_writepsf(), dpgqverify()
int	dp_idstar()
pointer	immap(), tbtopn()

begin
	# Allocate some working space.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Define some pointers.
	apsel = DP_APSEL(dao)

	# Initialize some variables.
	key = 'a'
	newpsf = true
	psf_written = false

	# Print the help page.
	if (interactive) {
	    call printf (
	       "The number of stars in the aperture photometry file is %d.\n\n")
		call pargi (DP_APNUM (apsel))
	}


	# Begin to build the PSF.
	while (clgcur ("commands", wx, wy, wcs, key, Memc[cmd], SZ_LINE) !=
	    EOF) {

	    switch (key) {

	    # Quit the interactive cursor loop.
	    case 'q':
		if (interactive) {
		    if (dpgqverify (dao, im, psfim, psfgr, psf_written,
		        key) == YES) {
			if (! psf_written)
			    call dp_rmpsf (dao, psfim, psfgr)
			break
		    }
		} else {
		    if (! newpsf) {
			if (! psf_written)
			    if (dp_writepsf (dao, im, psfim) == ERR)
			        call dp_rmpsf (dao, psfim, psfgr)
		    } else 
			call dp_rmpsf (dao, psfim, psfgr)
		    break
		}

	    # Print the help page.
	    case '?':
		if (id == NULL)
		    call pagefile (HELPFILE, "")
		else
		    call gpagefile (id, HELPFILE, "")

	    # Add the star nearest the cursor position to the PSF.
	    case 'a':
		if (newpsf) {
		    if (dp_newpsf (dao, im, psfim, psfgr, wx, wy, 0, gd,
		        mgd, showplots) == OK) {
		        newpsf = false 
			psf_written = false
		    }

		} else {
		    if (dp_addpsf (dao, im, psfim, psfgr, wx, wy, 0, gd,
		        mgd, showplots) != OK) {
			next
		    } else
			psf_written = false
		}

	    # Rebuild the PSF from scratch.
	    case 'z':

		newpsf = true

		# Delete the previous GROUP table and reopen a new one of
		# the same name.
		if (DP_TEXT(dao) == YES) {
		    call fstats (psfgr, F_FILENAME, Memc[str], SZ_FNAME)
		    call close (psfgr)
		    call delete (Memc[str])
		    psfgr = open (Memc[str], NEW_FILE, TEXT_FILE)
		} else {
		    call tbtnam (psfgr, Memc[str], SZ_FNAME)
		    call tbtclo (psfgr)
		    call delete (Memc[str])
		    psfgr = tbtopn (Memc[str], NEW_FILE, 0)
		}

		# Delete the previous PSF image and reopen a new PSF
		# image of the same name.
		call strcpy (IM_HDRFILE(psfim), Memc[str], SZ_FNAME)
		call imunmap (psfim)
		call imdelete (Memc[str])
		psfim = immap (Memc[str], NEW_IMAGE, 0)

	    # Write out the PSF.
	    case 'w':
		if (! newpsf && dp_writepsf (dao, im, psfim) != ERR) {
		    psf_written = true
		    if (interactive) {
		        call printf ("Writing PSF %s for image %s\n")
			    call pargstr (IM_HDRFILE (psfim))
			    call pargstr (IM_HDRFILE (im))
		    }
		} else
		    call printf ("The PSF is undefined.\n")

	    # Locate the star in the aperture photometry file and print out
	    # the photometry.
	    case 'p':
		istar = dp_locstar (dao, im, wx, wy)
		if (istar > 0) {
		    call printf (
		    "Star: %4d  Mag: %7.2f  Coords: %7.2f %7.2f Sky: %10.1f\n")
		        call pargi (Memi[DP_APID(apsel)+istar-1])
		        call pargr (Memr[DP_APMAG(apsel)+istar-1])
		        call pargr (Memr[DP_APXCEN(apsel)+istar-1])
		        call pargr (Memr[DP_APYCEN(apsel)+istar-1])
		        call pargr (Memr[DP_APMSKY(apsel)+istar-1])
		} else if (istar == 0) {
		    call printf (
		        "Star not found in the photometry file.\n")
		} else {
		    call printf (
		    "Star is off or too near the edge of the image.\n")
		}

	    # Command mode
	    case ':':
		for (ip = 1; IS_WHITE(Memc[cmd+ip-1]); ip = ip + 1)
		    ;
		switch (Memc[cmd+ip-1]) {
		case 'p':
		    ip = ip + 1
		    if (ctoi (Memc[cmd], ip, idnum) <= 0)
			istar = dp_locstar (dao, im, wx, wy)
		    else
			istar = dp_idstar (dao, im, idnum)
		    if (istar > 0) {
		        call printf (
		    "Star: %4d  Mag: %7.2f  Coords: %7.2f %7.2f Sky: %10.1f\n")
		            call pargi (Memi[DP_APID(apsel)+istar-1])
		            call pargr (Memr[DP_APMAG(apsel)+istar-1])
		            call pargr (Memr[DP_APXCEN(apsel)+istar-1])
		            call pargr (Memr[DP_APYCEN(apsel)+istar-1])
		            call pargr (Memr[DP_APMSKY(apsel)+istar-1])
		    } else if (istar == 0) {
		        call printf (
		            "Star not found in the photometry file\n")
		    } else {
		        call printf (
		       "Star is off or too near the edge of the image.\n")
		    }

		case 'a':
		    ip = ip + 1
		    if (ctoi (Memc[cmd], ip, idnum) <= 0)
			idnum = 0
		    if (newpsf) {
		        if (dp_newpsf (dao, im, psfim, psfgr, wx, wy, idnum,
		    	    gd, mgd, showplots) == OK)
		            newpsf = false 
		    } else {
		        if (dp_addpsf (dao, im, psfim, psfgr, wx, wy, idnum,
			    gd, mgd, showplots) != OK)
			    next
			else
			    psf_written = false
		    }

		}

	    default:
		call printf ("Unknown keystroke command\007\n")
	    }
	}

	# Free up memory. 
	call sfree (sp)
end
