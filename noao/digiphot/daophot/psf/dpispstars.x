include <fset.h>
include <ctype.h>
include <gset.h>
include "../lib/daophotdef.h"
include "../lib/apseldef.h"
include "../lib/psfdef.h"

define	HELPFILE	"daophot$psf/mkpsflist.key"

# DP_IPFSTARS -- Construct a stellar PSF from one or more stars in an image
# frame.

int procedure dp_ipfstars (dao, im, maxnpsf, lolimit, radius, gd, mgd, id,
	mkstars, interactive, showplots)

pointer	dao			# pointer to DAOPHOT structure
pointer im			# pointer to IRAF image
int	maxnpsf			# maximum number of psf stars
real	lolimit			# lower magnitude limit for stars
real	radius			# the confusion radius for good psf stars
pointer gd			# pointer to graphics descriptor
pointer mgd			# pointer to the metacode file
pointer id			# pointer to image display stream
bool	mkstars			# marked the selected and deleted psf stars
bool	interactive		# interactive mode
bool	showplots		# show the plots

int	key, nxstar, wcs, idnum, istar, ip
pointer apsel, sp, cmd, str
real	wx, wy

real	dp_pstatr()
int	clgcur(), dp_pqverify(), dp_locstar(), dp_idstar(), dp_nxstar()
int	dp_pstati(), ctoi(), dp_addstar(), dp_delstar()

begin
	# Get some pointers
	apsel = DP_APSEL(dao)

	# Allocate some working space.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Initialize some variables.
	key = 'a'
	nxstar = 0
	call dp_pseti (dao, PNUM, 0)

	# Begin to build the PSF.
	while (clgcur ("icommands", wx, wy, wcs, key, Memc[cmd], SZ_LINE) !=
	    EOF) {

	    # Correct for tv coordinates if necessary.
	    call dp_vtol (im, wx, wy, wx, wy, 1)

	    switch (key) {

	    # Quit the interactive cursor loop.
	    case 'q':
		if (interactive) {
		    if (dp_pqverify() == YES)
			break
		} else
		    break

	    # Print the help page.
	    case '?':
		if (id == NULL)
		    call pagefile (HELPFILE, "")
		else
		    call gpagefile (id, HELPFILE, "")

	    # Add next candidate star to the PSF.
	    case 'n':
		if (dp_pstati (dao, PNUM) >= maxnpsf) {
		    call printf ("Number of psf stars is >= to maxnpsf\n")
		    next
		}
		idnum = dp_nxstar (Memi[DP_APID(apsel)], Memr[DP_APXCEN(apsel)],
		    Memr[DP_APYCEN(apsel)], Memr[DP_APMSKY(apsel)],
		    Memr[DP_APMAG(apsel)], DP_APNUM(apsel), nxstar, lolimit,
		    radius)
		if (idnum <= 0) {
		    call printf ("No more good psf stars in photometry list\n")
		    next
		}
		if (dp_addstar (dao, im, wx, wy, INDEFR, idnum, gd, mgd,
		    showplots) == ERR)
		    next
		if (mkstars && id != NULL) {
		    call gmark (id, dp_pstatr (dao, CUR_PSFX), dp_pstatr (dao,
		        CUR_PSFY), GM_PLUS, -5.0, -5.0)
		    if (gd == id)
		        call gflush (id)
		    else
		        call gframe (id)
		}

	    # Add the star nearest the cursor position to the PSF.
	    case 'a':
		if (dp_pstati (dao, PNUM) >= maxnpsf) {
		    call printf ("Number of psf stars is >= to maxnpsf\n")
		    next
		}
		if (dp_addstar (dao, im, wx, wy, INDEFR, 0, gd, mgd,
		    showplots) == ERR)
		    next
		if (mkstars && id != NULL) {
		    call gmark (id, dp_pstatr (dao, CUR_PSFX), dp_pstatr (dao,
		        CUR_PSFY), GM_PLUS, -5.0, -5.0)
		    if (gd == id)
		        call gflush (id)
		    else
		        call gframe (id)
		}

	    # Delete the star nearest the cursor position from the PSF.
	    case 'd':
		if (dp_delstar (dao, im, wx, wy, 0, gd, showplots) == ERR)
		    next
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
		    call printf ("Star not found in the photometry file\n")
		else
		    call printf (
		        "Star off or too near the edge of the image\n")

	    # Colon command mode.
	    case ':':
		for (ip = 1; IS_WHITE(Memc[cmd+ip-1]); ip = ip + 1)
		    ;
		switch (Memc[cmd+ip-1]) {

		case 'p':
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
		    if (dp_pstati (dao, PNUM) >= maxnpsf) {
		        call printf (
			    "Number of psf stars is >= to maxnpsf\n")
			next
		    }
		    ip = ip + 1
		    if (ctoi (Memc[cmd], ip, idnum) <= 0)
		        idnum = 0
		    if (dp_addstar (dao, im, wx, wy, INDEFR, idnum, gd, mgd,
		        showplots) == ERR) 
			next
		    if (mkstars && id != NULL) {
		        call gmark (id, dp_pstatr (dao, CUR_PSFX),
			    dp_pstatr (dao, CUR_PSFY), GM_PLUS, -5.0, -5.0)
			if (gd == id)
			    call gflush (id)
			else
			    call gframe (id)
		    }

		case 'd':
		    ip = ip + 1
		    if (ctoi (Memc[cmd], ip, idnum) <= 0)
			idnum = 0
		    if (dp_delstar (dao, im, wx, wy, idnum, gd, showplots) ==
		        ERR)
			next
		    if (mkstars && id != NULL) {
			call gmark (id, dp_pstatr (dao, CUR_PSFX),
			    dp_pstatr (dao, CUR_PSFY), GM_CROSS, -5.0, -5.0)
			if (gd == id)
			    call gflush (id)
			else
			    call gframe (id)
		    }

		default:
		    call printf ("Unknown keystroke command\n")
		}

	    default:
		call printf ("Unknown keystroke command\n")
	    }
	}

	# Free up memory. 
	call sfree (sp)

	return (dp_pstati (dao, PNUM))
end


define	QUERY "[Hit return to continue, q to quit]"

# DP_PQVERIFY -- Print a message in the status line asking the user if they
# really want to quit, returning YES if they really want to quit, NO otherwise.

int procedure dp_pqverify ()

int	ch
pointer	tty
int	getci()
pointer	ttyodes()

begin
	# Open terminal and print query.
	tty = ttyodes ("terminal")
	call ttyclearln (STDOUT, tty)
	call ttyso (STDOUT, tty, YES)
	call printf (QUERY)
	call flush (STDOUT)

	# Get character.
	call fseti (STDIN, F_RAW, YES)
	if (getci (STDIN, ch) == EOF)
	    ;

	# Reset and close terminal.
	call fseti (STDIN, F_RAW, NO)
	call ttyso (STDOUT, tty, NO)
	call ttyclearln (STDOUT, tty)
	call printf ("\n")
	call flush (STDOUT)
	call ttycdes (tty)

	# Return YES for the quit command, otherwise NO.
	if (ch == 'q') {
	    return (YES)
	} else {
	    return (NO)
	}
end



# DP_NXSTAR -- Select the next psf star form the photometry list.

int procedure dp_nxstar (ids, xcen, ycen, sky, mag, nstar, nxstar, lolimit,
	radius) 

int	ids[ARB]		# array of star ids
real	xcen[ARB]		# array of x coordinates
real	ycen[ARB]		# array of y coordinates
real	sky[ARB]		# array of sky values
real	mag[ARB]		# array of magnitudes
int	nstar			# the number of stars
int	nxstar			# the current star
real	lolimit			# lower data limit
real	radius			# minimum separation

bool	omit
int	istar, jstar
real	radsq, dy2, dr2

begin
	radsq = radius * radius

	# Check the first star.
	if ((mag[1] > lolimit) && (nxstar == 0)) {
	    nxstar = 1
	    return (ids[1])
	}

	# Loop over the candidate psf stars.
	do istar = nxstar + 1, nstar {

	    # Test that the candidate psf stars are not saturated and
	    # are sufficiently far from the edge of the frame.

	    if (mag[istar] <= lolimit)
		next

	    # Text that there are no brighter stars with a distance squared
	    # of radsq.
	    omit = false
	    do jstar = 1, istar - 1 {
		dy2 = abs (ycen[jstar] - ycen[istar])
		if (dy2 >= radius)
		    next
		dr2 = (xcen[jstar] - xcen[istar]) ** 2 + dy2 ** 2
		if (dr2 >= radsq)
		    next
		omit = true
		break
	    }

	    if (omit)
		next

	    nxstar = istar
	    return (ids[istar])
	}

	return (0)
end
