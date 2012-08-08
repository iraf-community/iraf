include	<error.h>
include <smw.h>
include	"ecidentify.h"

# T_ECREIDENTIFY -- Reidentify echelle features starting from reference.
# If no initial shift is given then the procedure ec_shift computes a
# shift between the reference features and the features in the image.
# The purpose of the shift is to get the feature positions from the
# reference image close enough to those of the image being identified
# that the centering algorithm will determine the exact positions of the
# features.  The recentered features are then fit with either a shift
# of a full echelle function and written to database.

procedure t_ecreidentify ()

int	images			# List of images
pointer	ref			# Reference image
double	shift			# Initial shift

int	i, j, fd, nfeatures1, nfeatures2
double	shift1, pix, fit, pix_shift, fit_shift, z_shift
pointer	sp, log, ec

int	imtopenp(), ec_getim(), clpopnu(), clgfil(), open(), btoi()
double	ec_fitpt(), ec_fittopix(), ec_shift(), ec_center(), ec_rms()
double	clgetd()
bool	clgetb()
real	clgetr()
errchk	ec_dbread(), ec_gdata(), ec_fitdata()

begin
	call smark (sp)
	call salloc (ref, SZ_FNAME, TY_CHAR)
	call salloc (log, SZ_FNAME, TY_CHAR)

	# Allocate the basic data structure.
	call ec_init (ec)

	# Initialize fitting
	call ecf_seti ("niterate", 0)
	call ecf_setd ("low", 3.D0)
	call ecf_setd ("high", 3.D0)

	# Get task parameters.
	images = imtopenp ("images")
	call clgstr ("reference", Memc[ref], SZ_FNAME)
	shift = clgetd ("shift")
	call clgstr ("database", Memc[EC_DATABASE(ec)], SZ_FNAME)
	EC_CRADIUS(ec) = clgetr ("cradius")
	EC_THRESHOLD(ec) = clgetr ("threshold")
	EC_LOGFILES(ec) = clpopnu ("logfiles")
	EC_REFIT(ec) = btoi (clgetb ("refit"))

	# Write logfile header.
	while (clgfil (EC_LOGFILES(ec), Memc[log], SZ_FNAME) != EOF) {
	    iferr (fd = open (Memc[log], APPEND, TEXT_FILE)) {
		call erract (EA_WARN)
		next
	    }
	    call sysid (Memc[log], SZ_LINE)
	    call fprintf (fd, "\nECREIDENTIFY: %s\n")
		call pargstr (Memc[log])
	    call fprintf (fd,
		"  Reference image = %s, Refit = %b\n")
		call pargstr (Memc[ref])
		call pargb (EC_REFIT(ec) == YES)
	    call fprintf (fd, "%20s  %7s %7s %9s  %10s  %7s  %7s\n")
		call pargstr ("Image")
		call pargstr ("Found")
		call pargstr ("Fit")
		call pargstr ("Pix Shift")
		call pargstr ("User Shift")
		call pargstr ("Z Shift")
		call pargstr ("RMS")
	    call close (fd)
	}

	# Reidentify features in each spectrum.
	while (ec_getim (images, Memc[EC_IMAGE(ec)], SZ_FNAME) != EOF) {
	    call ec_gdata (ec)
	    call ec_dbread (ec, Memc[ref], NO)
	    call ec_fitdata (ec)
	    call ec_fitfeatures (ec)

	    if (IS_INDEFD (shift)) {
		EC_FWIDTH(ec) = FWIDTH(ec,1)
		EC_FTYPE(ec) = abs (FTYPE(ec,1))
		EC_MINSEP(ec) = 1.
		EC_MAXFEATURES(ec) = 20
	        shift1 = ec_shift (ec)
	    } else
	        shift1 = shift

	    # Recenter features.
	    pix_shift = 0.
	    fit_shift = 0.
	    z_shift = 0.
	    nfeatures1 = EC_NFEATURES(ec)

	    j = 0.
	    do i = 1, EC_NFEATURES(ec) {
	        call ec_gline (ec, LINE(ec,i))
	        pix = ec_fittopix (ec, FIT(ec,i) + shift1/ORDER(ec,i))
	        pix = ec_center (ec, pix, FWIDTH(ec,i), FTYPE(ec,i))
	        if (IS_INDEFD (pix))
		    next
	        fit = ec_fitpt (ec, APN(ec,i), pix)

	        pix_shift = pix_shift + pix - PIX(ec,i)
		fit_shift = fit_shift + (fit - FIT(ec,i)) * ORDER(ec,i)
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
	        FTYPE(ec,j) = abs (FTYPE(ec,i))
	    }
	    EC_NFEATURES(ec) = j

	    # If refitting the coordinate function is requested and there
	    # is more than one feature and there is a previously defined
	    # coordinate function then refit.  Otherwise compute a coordinate
	    # shift.

	    if ((EC_REFIT(ec)==YES)&&(EC_NFEATURES(ec)>1)&&(EC_ECF(ec)!=NULL)) {
	        iferr (call ec_dofit (ec, NO, YES)) {
		    call erract (EA_WARN)
		    next
		}
	    } else
	        call ec_doshift (ec, NO)
	    if (EC_NEWECF(ec) == YES)
	        call ec_fitfeatures (ec)

	    nfeatures2 = 0
	    do i = 1, EC_NFEATURES(ec)
		if (FTYPE(ec,i) > 0)
		    nfeatures2 = nfeatures2 + 1

	    # Write a database entry for the reidentified image.
	    if (EC_NFEATURES(ec) > 0)
	        call ec_dbwrite (ec, Memc[EC_IMAGE(ec)], NO)

	    # Record log information if a log file descriptor is given.
	    call clprew (EC_LOGFILES(ec))
	    while (clgfil (EC_LOGFILES(ec), Memc[log], SZ_FNAME) != EOF) {
	        iferr (fd = open (Memc[log], APPEND, TEXT_FILE)) {
		    call erract (EA_WARN)
		    next
	        }
	        call fprintf (fd,
		    "%20s  %3d/%-3d %3d/%-3d %9.3g  %10.3g  %7.3g  %7.3g\n")
		    call pargstr (Memc[EC_IMAGE(ec)])
		    call pargi (EC_NFEATURES(ec))
		    call pargi (nfeatures1)
		    call pargi (nfeatures2)
		    call pargi (EC_NFEATURES(ec))
		    call pargd (pix_shift / max (1, EC_NFEATURES(ec)))
		    call pargd (fit_shift / max (1, EC_NFEATURES(ec)))
		    call pargd (z_shift / max (1, EC_NFEATURES(ec)))
		    call pargd (ec_rms(ec))
	        call close (fd)
	    }

	    call smw_close (MW(EC_SH(ec)))
	    do i = 1, EC_NLINES(ec)
		MW(SH(ec,i)) = NULL
	}

	call dgsfree (EC_ECF(ec))
	call clpcls (EC_LOGFILES(ec))
	call ec_free (ec)
	call imtclose (images)
	call sfree (sp)
end
