include	<error.h>
include	<imhdr.h>
include	<gset.h>
include	<smw.h>
include	"ecidentify.h"

define	HELP		"noao$onedspec/ecidentify/ecidentify.key"
define	PROMPT		"ecidentify options"

define	PAN		1	# Pan graph
define	ZOOM		2	# Zoom graph

# EC_IDENTIFY -- Identify echelle features in an image.
# This is the basic interactive loop.

procedure ec_identify (ec)

pointer	ec			# EC pointer

real	wx, wy
int	wcs, key
char	cmd[SZ_LINE]

char	newimage[SZ_FNAME]
int	i, j, last, all, prfeature, nfeatures1, npeaks
bool	answer
double	pix, fit, user, shift, pix_shift, z_shift
pointer	peaks

bool	clgetb()
int	clgcur(), scan(), nscan(), find_peaks(), ec_next(), ec_previous()
int	ec_line()
double	ec_center(), ec_fittopix(), ec_fitpt(), ec_shift(), ec_rms()
double	ecf_getd()
errchk	ec_gdata(), ec_graph(), ec_dbread(), xt_mk1d(), ec_line()

define	newim_		10
define	newkey_		20
define	beep_		99

begin
newim_	# Start here for each new image.

	# Get the image data.  Return if there is an error.
	iferr (call ec_gdata (ec)) {
	    call erract (EA_WARN)
	    return
	}

	# Look for a database entry for the image.
	iferr {
	    call ec_dbread (ec, Memc[EC_IMAGE(ec)], NO)
	    EC_NEWDBENTRY(ec) = NO
	} then
	    if ((EC_NFEATURES(ec) > 0) || (EC_ECF(ec) != NULL))
	        EC_NEWDBENTRY(ec) = YES

	# Set the coordinate array and the feature data.
	iferr (call ec_fitdata (ec))
	    call erract (EA_WARN)
	call ec_fitfeatures (ec)

	# Begin with the first image line.
	EC_LINE(ec) = 1
	EC_AP(ec) = APS(ec,EC_LINE(ec))
	EC_ORDER(ec) = ORDERS(ec,EC_LINE(ec))
	call ec_gline (ec, EC_LINE(ec))

	# Initialize.
	EC_GTYPE(ec) = PAN
	EC_REFIT(ec) = NO
	EC_NEWFEATURES(ec) = NO
	EC_NEWECF(ec) = NO
	EC_CURRENT(ec) = 0
	i = ec_next (ec, EC_CURRENT(ec))
	last = EC_CURRENT(ec)
	all = 0
	newimage[1] = EOS
	key = 'r'

	repeat {
	    prfeature = YES
	    if (all != 0)
		all = mod (all + 1, 3)

	    switch (key) {
	    case '?':	# Page help
		call gpagefile (EC_GP(ec), HELP, PROMPT)
	    case ':':	# Execute colon commands
		if (cmd[1] == '/')
		    call gt_colon (cmd, EC_GP(ec), EC_GT(ec), EC_NEWGRAPH(ec))
		else
		    call ec_colon (ec, cmd, newimage, prfeature)
	    case ' ':	# Go to the current feature
	    case '.':	# Go to the nearest feature
		if (EC_NFEATURES(ec) == 0)
		    goto beep_
		call ec_nearest (ec, double (wx))
	    case '-':	# Go to the previous feature
		if (ec_previous (ec, EC_CURRENT(ec)) == EOF)
		    goto beep_
	    case '+', 'n':	# Go to the next feature
		if (ec_next (ec, EC_CURRENT(ec)) == EOF)
		    goto beep_
	    case 'a':	# Set the all flag for the next key
		all = 1
	    case 'c':	# Center features on data
		if (all != 0) {
		    call eprintf ("Recentering features ...\n")
		    for (i = 1; i <= EC_NFEATURES(ec); i = i + 1) {
			call ec_gline (ec, LINE(ec,i))
		        call gseti (EC_GP(ec), G_PLTYPE, 0)
		        call ec_mark (ec, i)
		        call gseti (EC_GP(ec), G_PLTYPE, 1)
			FWIDTH(ec,i) = EC_FWIDTH(ec)
		        PIX(ec,i) = ec_center (ec, PIX(ec,i), FWIDTH(ec,i),
			    FTYPE(ec,i))
		        if (!IS_INDEFD (PIX(ec,i))) {
			    FIT(ec,i) = ec_fitpt (ec, APN(ec,i), PIX(ec,i))
		            call ec_mark (ec, i)
			} else {
			    call ec_delete (ec, i)
			    i = i - 1
		        }
		    }
		    call ec_gline (ec, EC_LINE(ec))
		    EC_NEWFEATURES(ec) = YES
		} else {
		    if (EC_NFEATURES(ec) == 0)
		       goto beep_

		    call ec_nearest (ec, double (wx))
		    pix = PIX(ec,EC_CURRENT(ec))
		    pix = ec_center (ec, pix, EC_FWIDTH(ec),
			FTYPE(ec,EC_CURRENT(ec)))
		    if (!IS_INDEFD (pix)) {
		        call gseti (EC_GP(ec), G_PLTYPE, 0)
		        call ec_mark (ec, EC_CURRENT(ec))
			PIX(ec,EC_CURRENT(ec)) = pix
		        FWIDTH(ec,EC_CURRENT(ec)) = EC_FWIDTH(ec)
			FIT(ec,EC_CURRENT(ec)) =
			    ec_fitpt (ec, APN(ec,EC_CURRENT(ec)), pix)
		        call gseti (EC_GP(ec), G_PLTYPE, 1)
			call ec_mark (ec, EC_CURRENT(ec))
		        EC_NEWFEATURES(ec) = YES
		    } else {
			call eprintf ("Centering failed\n")
			prfeature = NO
		    }
		}
	    case 'd':	# Delete features
		if (all != 0) {
		    EC_NFEATURES(ec) = 0
		    EC_CURRENT(ec) = 0
		    EC_NEWFEATURES(ec) = YES
		    EC_NEWGRAPH(ec) = YES
		} else {
		    if (EC_NFEATURES(ec) == 0)
			goto beep_

		    call ec_nearest (ec, double (wx))
		    call gseti (EC_GP(ec), G_PLTYPE, 0)
		    call ec_mark (ec, EC_CURRENT(ec))
		    call gseti (EC_GP(ec), G_PLTYPE, 1)
		    call ec_delete (ec, EC_CURRENT(ec))
		    call ec_nearest (ec, double (wx))
		    last = 0
		}
	    case 'f':	# Fit dispersion function
		iferr (call ec_dofit (ec, YES, NO)) {
		    call erract (EA_WARN)
		    prfeature = NO
		    goto beep_
		}
	    case 'g':	# Fit shift
		call ec_doshift (ec, YES)
		prfeature = NO
	    case 'i':	# Initialize
	        call dgsfree (EC_ECF(ec))
		call ecf_setd ("shift", 0.D0)
	        EC_NEWECF(ec) = YES
		EC_NFEATURES(ec) = 0
		EC_CURRENT(ec) = 0
		EC_NEWFEATURES(ec) = YES
		EC_NEWGRAPH(ec) = YES
	    case 'j':	# Go to the previous order
		EC_LINE(ec) =
		    mod (EC_LINE(ec)+EC_NLINES(ec)-2, EC_NLINES(ec)) + 1
		EC_AP(ec) = APS(ec,EC_LINE(ec))
		EC_ORDER(ec) = ORDERS(ec,EC_LINE(ec))
		call ec_gline (ec, EC_LINE(ec))
		EC_NEWGRAPH(ec) = YES
		EC_CURRENT(ec) = 0
		i = ec_next (ec, EC_CURRENT(ec))
	    case 'k':	# Go to the next order
		EC_LINE(ec) = mod (EC_LINE(ec), EC_NLINES(ec)) + 1
		EC_AP(ec) = APS(ec,EC_LINE(ec))
		EC_ORDER(ec) = ORDERS(ec,EC_LINE(ec))
		call ec_gline (ec, EC_LINE(ec))
		EC_NEWGRAPH(ec) = YES
		EC_CURRENT(ec) = 0
		i = ec_next (ec, EC_CURRENT(ec))
	    case 'l':	# Find features using a line list
		if (EC_ECF(ec) == NULL) {
		    call eprintf ("Doing initial fit ...\n")
		    iferr (call ec_dofit (ec, NO, NO)) {
		        call erract (EA_WARN)
		        prfeature = NO
			goto beep_
		    }
		    if (EC_NEWECF(ec) == YES) {
		        iferr (call ec_fitdata (ec)) {
			    call erract (EA_WARN)
			    prfeature = NO
		        }
		        call ec_fitfeatures (ec)
		        EC_NEWECF(ec) = NO
		    }
		}

		call eprintf ("Searching coordinate list ...\n")
		call ec_linelist (ec)
		EC_CURRENT(ec) = 0
		i = ec_next (ec, EC_CURRENT(ec))
		if (EC_NEWFEATURES(ec) == YES)
		    EC_NEWGRAPH(ec) = YES
	    case 'm':	# Mark a new feature
		fit = wx
		pix = ec_fittopix (ec, fit)
		pix = ec_center (ec, pix, EC_FWIDTH(ec), EC_FTYPE(ec))
		if (IS_INDEFD (pix))
		    goto beep_
		fit = ec_fitpt (ec, EC_AP(ec), pix)
		user = fit
		call ec_newfeature (ec, EC_AP(ec), pix, fit, user,
		    EC_FWIDTH(ec), EC_FTYPE(ec))
		USER(ec,EC_CURRENT(ec)) = INDEFD
		call ec_match (ec, FIT(ec,EC_CURRENT(ec)),
		    USER(ec,EC_CURRENT(ec)))
		call ec_mark (ec, EC_CURRENT(ec))
		call printf ("%3d %10.2f %10.8g (%10.8g): ")
		    call pargi (APN(ec,EC_CURRENT(ec)))
		    call pargd (PIX(ec,EC_CURRENT(ec)))
		    call pargd (FIT(ec,EC_CURRENT(ec)))
		    call pargd (USER(ec,EC_CURRENT(ec)))
		call flush (STDOUT)
		if (scan() != EOF) {
		    call gargd (user)
		    if (nscan() == 1) {
			USER(ec,EC_CURRENT(ec)) = user
			call ec_match (ec, user, USER(ec,EC_CURRENT(ec)))
		    }
		}
	    case 'o':	# Go to a specified order
		call printf ("Aperture (%d): ")
		    call pargi (EC_AP(ec))
		    call flush (STDOUT)
		if (scan() != EOF) {
		    call gargi (j)
		    if (nscan() == 1) {
			if (j != EC_AP(ec)) {
			    iferr {
				i = ec_line (ec, j)
				call ec_gline (ec, i)
				EC_LINE(ec) = i
				EC_AP(ec) = j
				EC_ORDER(ec) = ORDERS(ec,i)
				EC_NEWGRAPH(ec) = YES
				EC_CURRENT(ec) = 0
				i = ec_next (ec, EC_CURRENT(ec))
			    } then
				goto beep_
			}
		    }
		}
	    case 'p':	# Go to pan graph mode
		if (EC_GTYPE(ec) == PAN)
		    goto beep_

		EC_GTYPE(ec) = PAN
		EC_NEWGRAPH(ec) = YES
	    case 'q':	# Quit
		break
	    case 'r':	# Redraw the current graph
		EC_NEWGRAPH(ec) = YES
	    case 's', 'x':	# Shift or cross correlate features
		# Get coordinate shift.
		switch (key) {
		case 's':
		    call printf ("User coordinate (%10.8g): ")
		        call pargr (wx)
		        call flush (STDOUT)
		    if (scan() != EOF) {
		        call gargd (user)
		        if (nscan() == 1)
			    shift = (wx - user) * EC_ORDER(ec)
		    } else
		        shift = 0.
		case 'x':
		    if (EC_NFEATURES(ec) > 5) {
			call eprintf (
			    "Cross correlating features with peaks ...\n")
			shift = ec_shift (ec)
		    } else
			goto beep_
		}

		EC_NEWFEATURES(ec) = YES
		EC_NEWECF(ec) = YES
		EC_NEWGRAPH(ec) = YES
		prfeature = NO

		if (EC_NFEATURES(ec) < 1) {
		    call printf ("User coordinate shift=%5f")
			call pargd (shift / EC_ORDER(ec))
		    call ecf_setd ("shift", ecf_getd ("shift") - shift)
		    goto newkey_
		}

		# Recenter features.
		call eprintf ("Recentering features ...\n")
		pix_shift = 0.
		z_shift = 0.
		nfeatures1 = EC_NFEATURES(ec)

		j = 0.
		do i = 1, EC_NFEATURES(ec) {
		    call ec_gline (ec, LINE(ec,i))
		    pix = ec_fittopix (ec, FIT(ec,i) + shift/ORDER(ec,i))
		    pix = ec_center (ec, pix, FWIDTH(ec,i), FTYPE(ec,i))
		    if (IS_INDEFD (pix)) {
			if (EC_CURRENT(ec) == i)
			     EC_CURRENT(ec) = 0
			next
		    }
		    fit = ec_fitpt (ec, APN(ec,i), pix)

		    pix_shift = pix_shift + pix - PIX(ec,i)
		    if (FIT(ec,i) != 0.)
		        z_shift = z_shift + (fit - FIT(ec,i)) / FIT(ec,i)

		    j = j + 1
		    APN(ec,j) = APN(ec,i)
		    LINE(ec,j) = LINE(ec,i)
		    ORDER(ec,j) = ORDER(ec,i)
		    PIX(ec,j) = pix
		    FIT(ec,j) = FIT(ec,i)
		    USER(ec,j) = USER(ec,i)
		    FWIDTH(ec,j) = FWIDTH(ec,i)
		    FTYPE(ec,j) = FTYPE(ec,i)
		    if (EC_CURRENT(ec) == i)
			EC_CURRENT(ec) = j
		}
		call ec_gline (ec, EC_LINE(ec))
		EC_NFEATURES(ec) = j
		if (EC_CURRENT(ec) == 0)
		    i = ec_next (ec, EC_CURRENT(ec))

		if (EC_NFEATURES(ec) < 1) {
		    call printf ("User coordinate shift=%5f")
			call pargd (shift / EC_ORDER(ec))
		    call printf (", No features found during recentering")
		    call ecf_setd ("shift", ecf_getd ("shift") - shift)
		    goto newkey_
		}

		# Adjust shift.
		pix = ecf_getd ("shift")
		call ec_doshift (ec, NO)
		call ec_fitfeatures (ec)

		# Print results.
		call printf ("Recentered=%d/%d")
		    call pargi (EC_NFEATURES(ec))
		    call pargi (nfeatures1)
		call printf (
		    ", pixel shift=%.2f, user shift=%5f, z=%7.3g, rms=%5g")
		    call pargd (pix_shift / EC_NFEATURES(ec))
		    call pargd ((pix - ecf_getd ("shift")) / EC_ORDER(ec))
		    call pargd (z_shift / EC_NFEATURES(ec))
		    call pargd (ec_rms(ec))
	    case 't':	# Move current feature
		if (EC_CURRENT(ec) == 0)
		    goto beep_

		call gseti (EC_GP(ec), G_PLTYPE, 0)
		call ec_mark (ec, EC_CURRENT(ec))
		pix = ec_fittopix (ec, double (wx))
		PIX(ec,EC_CURRENT(ec)) = pix
		FIT(ec,EC_CURRENT(ec)) =
		    ec_fitpt (ec, APN(ec,EC_CURRENT(ec)), pix)
		call gseti (EC_GP(ec), G_PLTYPE, 1)
		call ec_mark (ec, EC_CURRENT(ec))
		EC_NEWFEATURES(ec) = YES
	    case 'u':	# Set uesr coordinate value
		if (EC_NFEATURES(ec) == 0)
		    goto beep_

		call printf ("%3d %10.2f %10.8g (%10.8g): ")
		    call pargi (APN(ec,EC_CURRENT(ec)))
		    call pargd (PIX(ec,EC_CURRENT(ec)))
		    call pargd (FIT(ec,EC_CURRENT(ec)))
		    call pargd (USER(ec,EC_CURRENT(ec)))
		    call flush (STDOUT)
		if (scan() != EOF) {
		    call gargd (user)
		    if (nscan() == 1) {
			USER(ec,EC_CURRENT(ec)) = user
			EC_NEWFEATURES(ec) = YES
		    }
		}
	    case 'w':	# Window graph
		call gt_window (EC_GT(ec), EC_GP(ec), "cursor", EC_NEWGRAPH(ec))
	    case 'y':	# Find peaks in order
		call malloc (peaks, EC_NPTS(ec), TY_REAL)
		npeaks = find_peaks (IMDATA(ec,1), Memr[peaks],
		    EC_NPTS(ec), 0., int (EC_MINSEP(ec)), 0, EC_MAXFEATURES(ec),
		    0., false)
		for (j = 1; j <= EC_NFEATURES(ec); j = j + 1) {
		    for (i = 1; i <= npeaks; i = i + 1) {
			if (!IS_INDEF(pix)) {
			    pix = Memr[peaks + i - 1]
			    if (abs (pix - PIX(ec,j)) < EC_MINSEP(ec))
			        Memr[peaks + i - 1] = INDEF
			}
		    }
		}
		for (i = 1; i <= npeaks; i = i + 1) {
		    pix = Memr[peaks+i-1]
		    pix = ec_center (ec, pix, EC_FWIDTH(ec), EC_FTYPE(ec))
		    if (IS_INDEFD (pix))
			next
		    fit = ec_fitpt (ec, EC_AP(ec), pix)
		    user = INDEFD
		    call ec_match (ec, fit, user)
		    call ec_newfeature (ec, EC_AP(ec), pix, fit, user,
			EC_FWIDTH(ec), EC_FTYPE(ec))
		    call ec_mark (ec, EC_CURRENT(ec))
		}
		call mfree (peaks, TY_REAL)
	    case 'z':	# Go to zoom mode
		if (EC_CURRENT(ec) == 0)
		    goto beep_

		if (EC_GTYPE(ec) == PAN)
		    EC_NEWGRAPH(ec) = YES
		EC_GTYPE(ec) = ZOOM
		call ec_nearest (ec, double (wx))
	    case 'I':	# Interrupt
		call fatal (0, "Interrupt")
	    default:	# Beep
beep_		 call printf ("\007\n")
	    }

newkey_
	    # Set database update flag if there has been a change.
	    if ((EC_NEWFEATURES(ec) == YES) || (EC_NEWECF(ec) == YES))
		EC_NEWDBENTRY(ec) = YES

	    # Exit loop and then start new image.
	    if (newimage[1] != EOS)
		break

	    # Refit the dispersion function if needed.
	    if (EC_REFIT(ec) == YES) {
		iferr (call ec_dofit (ec, NO, NO)) {
		    call erract (EA_WARN)
		    prfeature = NO
		}
		EC_REFIT(ec) = NO
	    }

	    # Recompute the coordinate information.
	    if (EC_NEWECF(ec) == YES) {
		iferr (call ec_fitdata (ec)) {
		    call erract (EA_WARN)
		    prfeature = NO
		}
		call ec_fitfeatures (ec)
		EC_NEWECF(ec) = NO
	    }

	    # Redraw new feature in zoom mode.
	    if ((EC_GTYPE(ec) == ZOOM) && (last != EC_CURRENT(ec)))
		EC_NEWGRAPH(ec) = YES

	    # Redraw graph.
	    if (EC_NEWGRAPH(ec) == YES) {
		call ec_graph (ec, EC_GTYPE(ec))
	        EC_NEWGRAPH(ec) = NO
	    }

	    # Set cursor and print current feature on status (unless canceled).
	    if (EC_CURRENT(ec) > 0) {
		call gscur (EC_GP(ec), real (FIT(ec,EC_CURRENT(ec))), wy)
		if (prfeature == YES) {
	            call printf ("%d %10.2f %10.8g %10.8g\n")
		        call pargi (APN(ec,EC_CURRENT(ec)))
		        call pargd (PIX(ec,EC_CURRENT(ec)))
		        call pargd (FIT(ec,EC_CURRENT(ec)))
		        call pargd (USER(ec,EC_CURRENT(ec)))
		}
	    }

	    last = EC_CURRENT(ec)
	} until (clgcur ("cursor", wx, wy, wcs, key, cmd, SZ_LINE) == EOF)

	# Warn user that feature data is newer than database entry.
	if (EC_NEWDBENTRY(ec) == YES) {
	    answer = true
	    if (!clgetb ("autowrite")) {
		call printf ("Write feature data to the database (yes)? ")
		call flush (STDOUT)
		if (scan() != EOF)
		    call gargb (answer)
	    }
	    if (answer)
	        call ec_dbwrite (ec, Memc[EC_IMAGE(ec)], NO)
	}

	call flush (STDOUT)

	# Free image data and MWCS
	call mfree (EC_PIXDATA(ec), TY_DOUBLE)
	call mfree (EC_FITDATA(ec), TY_DOUBLE)
	call smw_close (MW(EC_SH(ec)))
	do i = 1, EC_NLINES(ec)
	    MW(SH(ec,i)) = NULL

	# If a new image was specified by a colon command don't return.
	if (newimage[1] != EOS) {
	    call strcpy (newimage, Memc[EC_IMAGE(ec)], SZ_FNAME)
	    goto newim_
	}
end
