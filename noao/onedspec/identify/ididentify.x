include	<error.h>
include	<imhdr.h>
include	<gset.h>
include	"identify.h"

define	HELP		"noao$lib/scr/identify.key"
define	ICFITHELP	"noao$lib/scr/idicgfit.key"
define	PROMPT		"identify options"

define	PAN		1	# Pan graph
define	ZOOM		2	# Zoom graph

# ID_IDENTIFY -- Identify features in an image.
# This is the main interactive loop.

procedure id_identify (id, id_ll)

pointer	id			# ID pointer
pointer	id_ll			# Line list pointer

real	wx, wy
int	wcs, key
char	cmd[SZ_LINE]

char	newimage[SZ_FNAME]
int	i, j, last, all, prfeature, nfeatures1, npeaks
bool	answer
double	pix, fit, user, shift, pix_shift, z_shift
pointer	peaks

pointer	gopen()
int	clgcur(), scan(), nscan(), find_peaks(), errcode()
double	id_center(), fit_to_pix(), id_fitpt(), id_shift(), id_rms()
errchk	id_gdata(), id_graph(), id_dbread(), xt_mk1d()

define	newim_		10
define	newkey_		20
define	beep_		99

begin
newim_	 call xt_mk1d (Memc[ID_IMAGE(id)], Memc[ID_SECTION(id)], SZ_FNAME)

	# Get the database entry for the image if it exists.
	iferr {
	    call id_dbread (id, Memc[ID_IMAGE(id)], YES)
	    ID_NEWDBENTRY(id) = NO
	} then
	    if ((ID_NFEATURES(id) > 0) || (ID_CV(id) != NULL))
	        ID_NEWDBENTRY(id) = YES

	# Get the image data and return if there is an error.
	iferr (call id_gdata (id)) {
	    call erract (EA_WARN)
	    return
	}


	# Set the coordinate information.
	iferr (call id_fitdata (id))
	    ;

	# Set fitting limits.
	call ic_putr (ID_IC(id), "xmin", real (PIXDATA(id,1)))
	call ic_putr (ID_IC(id), "xmax", real (PIXDATA(id,ID_NPTS(id))))
	call ic_pstr (ID_IC(id), "help", ICFITHELP)

	# Open graphics.
	call clgstr ("graphics", newimage, SZ_FNAME)
	ID_GP(id) = gopen (newimage, NEW_FILE, STDGRAPH)

	# Initialize.
	ID_GTYPE(id) = PAN
	all = 0
	last = ID_CURRENT(id)
	newimage[1] = EOS
	ID_REFIT(id) = NO
	ID_NEWFEATURES(id) = NO
	ID_NEWCV(id) = NO
	key = 'r'

	repeat {
	    prfeature = YES
	    if (all != 0)
		all = mod (all + 1, 3)

	    switch (key) {
	    case '?':	# Print help
		call gpagefile (ID_GP(id), HELP, PROMPT)
	    case ':':	# Process colon commands
		if (cmd[1] == '/')
		    call gt_colon (cmd, ID_GP(id), ID_GT(id), ID_NEWGRAPH(id))
		else
		    call id_colon (id, cmd, id_ll, newimage, prfeature)
	    case ' ':	# Go to current feature
	    case '.':	# Go to nearest feature
		if (ID_NFEATURES(id) == 0)
		    goto beep_
		call id_nearest (id, double (wx))
	    case '-':	# Go to previous feature
		if (ID_CURRENT(id) == 1)
		    goto beep_
		ID_CURRENT(id) = ID_CURRENT(id) - 1
	    case '+', 'n':	# Go to next feature
		if (ID_CURRENT(id) == ID_NFEATURES(id))
		    goto beep_
		ID_CURRENT(id) = ID_CURRENT(id) + 1
	    case 'a':	# Set all flag for next key
		all = 1
	    case 'c':	# Recenter features
		if (all != 0) {
		    for (i = 1; i <= ID_NFEATURES(id); i = i + 1) {
		        call gseti (ID_GP(id), G_PLTYPE, 0)
		        call id_mark (id, i)
		        call gseti (ID_GP(id), G_PLTYPE, 1)
			FWIDTH(id,i) = ID_FWIDTH(id)
		        PIX(id,i) = id_center (id, PIX(id,i), FWIDTH(id,i),
			    FTYPE(id,i))
		        if (!IS_INDEFD (PIX(id,i))) {
			    FIT(id,i) = id_fitpt (id, PIX(id,i))
		            call id_mark (id, i)
			} else {
			    call id_delete (id, i)
			    i = i - 1
		        }
		    }
		    ID_NEWFEATURES(id) = YES
		} else {
		    if (ID_NFEATURES(id) < 1)
			goto beep_
		    call id_nearest (id, double (wx))
		    pix = PIX(id,ID_CURRENT(id))
		    pix = id_center (id, pix, ID_FWIDTH(id),
			FTYPE(id,ID_CURRENT(id)))
		    if (!IS_INDEFD (pix)) {
		        call gseti (ID_GP(id), G_PLTYPE, 0)
		        call id_mark (id, ID_CURRENT(id))
			PIX(id,ID_CURRENT(id)) = pix
		        FWIDTH(id,ID_CURRENT(id)) = ID_FWIDTH(id)
			FIT(id,ID_CURRENT(id)) = id_fitpt (id, pix)
		        call gseti (ID_GP(id), G_PLTYPE, 1)
			call id_mark (id, ID_CURRENT(id))
		        ID_NEWFEATURES(id) = YES
		    } else {
			call printf ("Centering failed")
			prfeature = NO
		    }
		}
	    case 'd':	# Delete features
		if (all != 0) {
		    ID_NFEATURES(id) = 0
		    ID_CURRENT(id) = 0
		    ID_NEWFEATURES(id) = YES
		    ID_NEWGRAPH(id) = YES
		} else {
		    if (ID_NFEATURES(id) < 1)
			goto beep_
		    call id_nearest (id, double (wx))
		    call gseti (ID_GP(id), G_PLTYPE, 0)
		    call id_mark (id, ID_CURRENT(id))
		    call gseti (ID_GP(id), G_PLTYPE, 1)
		    call id_delete (id, ID_CURRENT(id))
		    ID_CURRENT(id) = min (ID_NFEATURES(id), ID_CURRENT(id))
		    last = 0
		}
	    case 'f':	# Fit dispersion function
		call id_dofit (id, YES)
	    case 'g':	# Fit shift
		call id_doshift (id, YES)
		prfeature = NO
	    case 'i':	# Initialize
	        call dcvfree (ID_CV(id))
		ID_SHIFT(id) = 0.
	        ID_NEWCV(id) = YES
		ID_NFEATURES(id) = 0
		ID_CURRENT(id) = 0
		ID_NEWFEATURES(id) = YES
		ID_NEWGRAPH(id) = YES
	    case 'l':	# Find features from line list
		if (ID_NFEATURES(id) >= 2)
		    call id_dofit (id, NO)
		if (ID_NEWCV(id) == YES) {
		    iferr (call id_fitdata(id))
			;
		    call id_fitfeatures(id)
		    ID_NEWCV(id) = NO
		}
		call id_linelist (id, id_ll)
		if (ID_NEWFEATURES(id) == YES)
		    ID_REFIT(id) = YES
	    case 'm':	# Mark new feature
		fit = wx
		pix = fit_to_pix (id, fit)
		pix = id_center (id, pix, ID_FWIDTH(id), ID_FTYPE(id))
		if (IS_INDEFD (pix))
		    goto beep_
		fit = id_fitpt (id, pix)
		user = fit
		call id_newfeature (id, pix, fit, user, 1.0D0, ID_FWIDTH(id),
		    ID_FTYPE(id))
		USER(id,ID_CURRENT(id)) = INDEFD
		call id_match (id_ll, FIT(id,ID_CURRENT(id)),
		    USER(id,ID_CURRENT(id)), ID_MATCH(id))
		call id_mark (id, ID_CURRENT(id))
		call printf ("%10.2f %10.8g (%10.8g): ")
		    call pargd (PIX(id,ID_CURRENT(id)))
		    call pargd (FIT(id,ID_CURRENT(id)))
		    call pargd (USER(id,ID_CURRENT(id)))
		call flush (STDOUT)
		if (scan() != EOF) {
		    call gargd (user)
		    if (nscan() == 1) {
			USER(id,ID_CURRENT(id)) = user
			call id_match (id_ll, user, USER(id,ID_CURRENT(id)),
			    ID_MATCH(id))
		    }
		}
	    case 'p':	# Switch to pan mode
		if (ID_GTYPE(id) != PAN) {
		    ID_GTYPE(id) = PAN
		    ID_NEWGRAPH(id) = YES
		}
	    case 'q':	# Exit loop
		break
	    case 'r':	# Redraw the graph
		ID_NEWGRAPH(id) = YES
	    case 's', 'x':	# Shift or correlate features
		# Get coordinate shift.
		switch (key) {
		case 's':
		    call printf ("User coordinate (%10.8g): ")
		        call pargr (wx)
		        call flush (STDOUT)
		    if (scan() != EOF) {
		        call gargd (user)
		        if (nscan() == 1)
			    shift = wx - user
		    } else
		        shift = 0.
		case 'x':
		    if (ID_NFEATURES(id) > 5)
			shift = id_shift (id)
		    else
			goto beep_
		}

		ID_NEWFEATURES(id) = YES
		ID_NEWCV(id) = YES
		ID_NEWGRAPH(id) = YES
		prfeature = NO

		if (ID_NFEATURES(id) < 1) {
		    call printf ("User coordinate shift=%5f")
			call pargd (shift)
		    ID_SHIFT(id) = ID_SHIFT(id) + shift
		    goto newkey_
		}

		# Recenter features.
		pix_shift = 0.
		z_shift = 0.
		nfeatures1 = ID_NFEATURES(id)

		j = 0.
		do i = 1, ID_NFEATURES(id) {
		    pix = fit_to_pix (id, FIT(id,i) + shift)
		    pix = id_center (id, pix, FWIDTH(id,i), FTYPE(id,i))
		    if (IS_INDEFD (pix)) {
			if (ID_CURRENT(id) == i)
			    ID_CURRENT(id) = i + 1
			next
		    }
		    fit = id_fitpt (id, pix)

		    pix_shift = pix_shift + pix - PIX(id,i)
		    if (FIT(id,i) != 0.)
		        z_shift = z_shift + (fit - FIT(id,i)) / FIT(id,i)

		    j = j + 1
		    PIX(id,j) = pix
		    FIT(id,j) = FIT(id,i)
		    USER(id,j) = USER(id,i)
		    WTS(id,j) = WTS(id,i)
		    FWIDTH(id,j) = FWIDTH(id,i)
		    FTYPE(id,j) = FTYPE(id,i)
		    if (ID_CURRENT(id) == i)
			ID_CURRENT(id) = j
		}
		if (j != ID_NFEATURES(id)) {
		    ID_NFEATURES(id) = j
		    ID_CURRENT(id) = min (ID_CURRENT(id), ID_NFEATURES(id))
		}

		if (ID_NFEATURES(id) < 1) {
		    call printf ("User coordinate shift=%5f")
			call pargd (shift)
		    call printf (", No features found during recentering")
		    ID_SHIFT(id) = ID_SHIFT(id) + shift
		    goto newkey_
		}

		# Adjust shift.
		pix = ID_SHIFT(id)
		call id_doshift (id, NO)
		call id_fitfeatures (id)

		# Print results.
		call printf ("Recentered=%d/%d")
		    call pargi (ID_NFEATURES(id))
		    call pargi (nfeatures1)
		call printf (
		    ", pixel shift=%.2f, user shift=%5f, z=%7.3g, rms=%5g")
		    call pargd (pix_shift / ID_NFEATURES(id))
		    call pargd (pix - ID_SHIFT(id))
		    call pargd (z_shift / ID_NFEATURES(id))
		    call pargd (id_rms(id))
	    case 't':	# Move the current feature
		if (ID_CURRENT(id) < 1)
		    goto beep_
		pix = fit_to_pix (id, double (wx))
		call gseti (ID_GP(id), G_PLTYPE, 0)
		call id_mark (id, ID_CURRENT(id))
		PIX(id,ID_CURRENT(id)) = pix
		FIT(id,ID_CURRENT(id)) = id_fitpt (id, pix)
		call gseti (ID_GP(id), G_PLTYPE, 1)
		call id_mark (id, ID_CURRENT(id))
		ID_NEWFEATURES(id) = YES
	    case 'u':	# Set user coordinate
		if (ID_NFEATURES(id) < 1)
		    goto beep_
		call printf ("%10.2f %10.8g (%10.8g): ")
		    call pargd (PIX(id,ID_CURRENT(id)))
		    call pargd (FIT(id,ID_CURRENT(id)))
		    call pargd (USER(id,ID_CURRENT(id)))
		call flush (STDOUT)
		if (scan() != EOF) {
		    call gargd (user)
		    if (nscan() == 1) {
			USER(id,ID_CURRENT(id)) = user
			ID_NEWFEATURES(id) = YES
		    }
		}
	    case 'w':	# Window graph
		call gt_window (ID_GT(id), ID_GP(id), "cursor", ID_NEWGRAPH(id))
	    case 'y':	# Find peaks
		call malloc (peaks, ID_NPTS(id), TY_DOUBLE)
		npeaks = find_peaks (IMDATA(id,1), Memd[peaks], ID_NPTS(id), 0.,
		    int (ID_MINSEP(id)), 0, ID_MAXFEATURES(id), 0., false)
		for (j = 1; j <= ID_NFEATURES(id); j = j + 1) {
		    for (i = 1; i <= npeaks; i = i + 1) {
		        pix = Memd[peaks + i - 1]
			if (!IS_INDEFD (pix))
			    if (abs (pix - PIX(id,j)) < ID_MINSEP(id))
			        Memd[peaks + i - 1] = INDEFD
		    }
		}
		for (i = 1; i <= npeaks; i = i + 1) {
		    pix = Memd[peaks+i-1]
		    if (IS_INDEFD (pix))
			next
		    pix = id_center (id, pix, ID_FWIDTH(id), ID_FTYPE(id))
		    if (IS_INDEFD (pix))
			next
		    fit = id_fitpt (id, pix)
		    user = INDEFD
		    call id_match (id_ll, fit, user, ID_MATCH(id))
		    call id_newfeature (id, pix, fit, user, 1.0D0,
			ID_FWIDTH(id), ID_FTYPE(id))
		    call id_mark (id, ID_CURRENT(id))
		}
		call mfree (peaks, TY_DOUBLE)
	    case 'z':	# Go to zoom mode
		if (ID_NFEATURES(id) < 1)
		    goto beep_
		if (ID_GTYPE(id) == PAN)
		    ID_NEWGRAPH(id) = YES
		ID_GTYPE(id) = ZOOM
		call id_nearest (id, double (wx))
	    case 'I':
		call fatal (0, "Interrupt")
	    default:
beep_		call printf ("\007")
	    }

newkey_
	    # Set update flag if anything has changed.
	    if ((ID_NEWFEATURES(id) == YES) || (ID_NEWCV(id) == YES))
		ID_NEWDBENTRY(id) = YES

	    # If a new image exit loop, update database, and start over.
	    if (newimage[1] != EOS)
		break

	    # Refit dispersion function
	    if (ID_REFIT(id) == YES) {
		call id_dofit (id, NO)
		ID_REFIT(id) = NO
	    }

	    # If there is a new dispersion solution evaluate the coordinates
	    if (ID_NEWCV(id) == YES) {
		iferr (call id_fitdata (id))
		    ;
		call id_fitfeatures (id)
		ID_NEWCV(id) = NO
	    }

	    # Draw new graph in zoom mode if current feature has changed.
	    if ((ID_GTYPE(id) == ZOOM) && (last != ID_CURRENT(id)))
		ID_NEWGRAPH(id) = YES

	    # Draw new graph.
	    if (ID_NEWGRAPH(id) == YES) {
		call id_graph (id, ID_GTYPE(id))
	        ID_NEWGRAPH(id) = NO
	    }

	    # Set cursor and print status of current feature (unless canceled).
	    if (ID_CURRENT(id) > 0) {
		call gscur (ID_GP(id), real (FIT(id,ID_CURRENT(id))), wy)
		if (errcode() == OK && prfeature == YES) {
	            call printf ("%10.2f %10.8g %10.8g")
		        call pargd (PIX(id,ID_CURRENT(id)))
		        call pargd (FIT(id,ID_CURRENT(id)))
		        call pargd (USER(id,ID_CURRENT(id)))
		}
	    }

	    # Print delayed error message
	    if (errcode() != OK)
		call erract (EA_WARN)

	    last = ID_CURRENT(id)
	} until (clgcur ("cursor", wx, wy, wcs, key, cmd, SZ_LINE) == EOF)
	call gclose (ID_GP(id))

	# Warn user that feature data is newer than database entry.
	if (ID_NEWDBENTRY(id) == YES) {
	    answer = true
	    call printf ("Write feature data to the database (yes)? ")
	    call flush (STDOUT)
	    if (scan() != EOF)
	        call gargb (answer)
	    if (answer)
	        call id_dbwrite (id, Memc[ID_IMAGE(id)], NO)
	}

	call flush (STDOUT)

	# Free image data.
	call mfree (ID_PIXDATA(id), TY_DOUBLE)
	call mfree (ID_IMDATA(id), TY_DOUBLE)
	call mfree (ID_FITDATA(id), TY_DOUBLE)

	# If a new image was request with colon command start over.
	if (newimage[1] != EOS) {
	    call strcpy (newimage, Memc[ID_IMAGE(id)], SZ_FNAME)
	    goto newim_
	}
end
