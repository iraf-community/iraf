include	<error.h>
include	<time.h>
include	"identify.h"

# ID_REIDENTIFY -- Reidentify features using a reference image database entry.

procedure id_reidentify (id, refimage, shift, logfd, nlogfd, pfd, verbose)

pointer	id			# ID pointer
char	refimage[ARB]		# Reference image
double	shift			# Initial shift
int	logfd[ARB]		# Logfiles
int	nlogfd			# Number of logfiles
int	pfd			# Plot file
bool	verbose			# Verbose logfiles?

int	i, j, nfeatures1, nfit, mono
double	shift1, pix, fit, pix_shift, fit_shift, z_shift

double	id_fitpt(), fit_to_pix(), id_shift(), id_center(), id_rms()

errchk	id_dbread

begin
	# Get the feature information for the reference image.  If no
	# reference image is given then the feature information is already
	# defined.  This avoids repeatedly accessing the database when
	# tracing a two dimensional image.  If the database entry for the
	# reference image is not found then the error is passed back.

	if (refimage[1] != EOS)
	    call id_dbread (id, refimage, NO)

	# Get the image data for the image to be identified and set the
	# fit coordinates used for inverting fit coordinates to pixel
	# coordinates.

	call id_gdata (id)
	iferr (call id_fitdata (id))
	    call erract (EA_WARN)

	# If no initial shift is given then the procedure id_shift computes
	# a shift between the reference features and the features in the
	# image.  The purpose of the shift is to get the feature positions
	# from the reference image close enough to those of the image being
	# identified that the centering algorithm will determine the exact
	# positions of the features.  An initial shift of zero is used if
	# the two images are very nearly aligned as in the case of tracing
	# features in a two dimensional image or for a set of images taken
	# with the same observing setup.

	if (IS_INDEFD (shift))
	    shift1 = id_shift (id)
	else
	    shift1 = shift

	# For each feature from the reference image a shift is added to bring
	# the pixel position near that for the image being identified and
	# then the centering algorithm is used.  If the centering algorithm
	# fails the feature is discarded.  A mean shift is computed for the
	# features which have been reidentified.

	pix_shift = 0.
	fit_shift = 0.
	z_shift = 0.
	nfeatures1 = ID_NFEATURES(id)

	j = 0
	do i = 1, ID_NFEATURES(id) {
	    pix = fit_to_pix (id, FIT(id,i) + shift1)
	    pix = id_center (id, pix, FWIDTH(id,i), FTYPE(id,i))
	    if (IS_INDEFD (pix))
		next

	    fit = id_fitpt (id, pix)
	    pix_shift = pix_shift + pix - PIX(id,i)
	    fit_shift = fit_shift + fit - FIT(id,i)
	    if (FIT(id,i) != 0.)
	        z_shift = z_shift + (fit - FIT(id,i)) / FIT(id,i)

	    j = j + 1
	    PIX(id,j) = pix
	    FIT(id,j) = fit
	    USER(id,j) = USER(id,i)
	    WTS(id,j) = WTS(id,i)
	    FWIDTH(id,j) = FWIDTH(id,i)
	    FTYPE(id,j) = FTYPE(id,i)
	}
	ID_NFEATURES(id) = j

	# If refitting the coordinate function is requested and there
	# is more than one feature and there is a previously defined
	# coordinate function then refit.  Otherwise compute a coordinate
	# shift.

	mono = YES
	if ((ID_REFIT(id)==YES) && (ID_NFEATURES(id)>1) && (ID_CV(id)!=NULL)) {
	    call id_dofit (id, NO)
	    iferr (call id_fitdata (id))
		mono = NO
	} else
	    call id_doshift (id, NO)
	if (ID_NEWCV(id) == YES)
	    call id_fitfeatures (id)

	# Write a database entry for the reidentified image.
	if (ID_NFEATURES(id) > 0)
	    call id_dbwrite (id, Memc[ID_IMAGE(id)], NO)

	# Record log information if a log file descriptor is given.
	for (i = 1; i <= nlogfd; i = i + 1) {
	    if (verbose || (ID_NFEATURES(id) != nfeatures1)) {
		nfit = 0
		for (j=1; j<=ID_NFEATURES(id); j=j+1)
		    if (WTS(id,j) > 0.)
			nfit = nfit + 1
	        call fprintf (logfd[i],
		    "%20s  %3d/%3d %3d/%3d %9.3g  %10.3g  %7.3g  %7.3g\n")
		    call pargstr (Memc[ID_IMAGE(id)])
		    call pargi (ID_NFEATURES(id))
		    call pargi (nfeatures1)
		    call pargi (nfit)
		    call pargi (ID_NFEATURES(id))
		    call pargd (pix_shift / max (1, ID_NFEATURES(id)))
		    call pargd (fit_shift / max (1, ID_NFEATURES(id)))
		    call pargd (z_shift / max (1, ID_NFEATURES(id)))
		    call pargd (id_rms(id))
	    }
	    if (mono == NO)
		call fprintf (logfd[i], "Non-monotonic dispersion function")
	}

	# Make log plot.
	call id_replot (id, pfd)
end
