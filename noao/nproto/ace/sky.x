include	<error.h>
include	"sky.h"


# SKY -- Determine sky and sky sigma in an image.
#
# Get the sky and sigma map pointers.  This is layered on the MAPIO routines
# and lower level sky algorithms.  The sky parameter structure will be
# allocated if needed and must be freed by the calling program.
#
# If they are not defined compute an initial
# sky and/or sky sigma surface fit using a subset of the input lines.
# Whether the sky and/or the sigma are fit is determined by whether the input
# sky and sky sigma pointers are NULL.  The initial data for the surface fit
# is measured at a subset of lines with any masked pixels excluded.  Objects
# are removed by fitting a 1D curve to each line, rejection points with large
# residuals and iterating until only sky is left.  The sky points are then
# accumulated for a 2D surface fit and the residuals are added to a
# histogram.  The absolute deviations, scaled by 0.7979 to convert to an
# gausian sigma, are accumulated for a sky sigma surface fit.  After all the
# sample lines are accumulated the surface fits are computed.  The histogram
# of residuals is then fit by a gaussian to estimate an offset from the sky
# fit to the sky mode caused by unrejected object light.  The offset is
# applied to the sky surface.

procedure sky (par, im, bpm, expmap, skyname, signame, skymap, sigmap,
	dosky, dosig, logfd)

pointer	par			#I Parameters
pointer	im			#I Input image
pointer	bpm			#I Input mask
pointer	expmap			#I Exposure map
char	skyname[ARB]		#I Sky map name
char	signame[ARB]		#I Sigma map name
pointer	skymap			#O Sky map
pointer	sigmap			#O Sigma map
bool	dosky			#O Sky computed?
bool	dosig			#O Sigma computed?
int	logfd			#I Verbose?

real	rval
pointer	sp, namesky, namesig

int	errcode()
pointer	map_open()
errchk	map_open, sky_fit, sky_block

begin
	call smark (sp)
	call salloc (namesky, SZ_FNAME, TY_CHAR)
	call salloc (namesig, SZ_FNAME, TY_CHAR)

	if (logfd != NULL)
	    call fprintf (logfd, "  Set sky and sigma:\n")

	# Check whether to compute a sky.
	skymap = NULL
	if (skyname[1] != EOS) {
	    iferr (skymap = map_open (skyname, im)) {
		skymap = NULL
		if (errcode() != 2)
		   call erract (EA_ERROR)
	    }
	    if (logfd != NULL && skymap != NULL) {
		ifnoerr (call map_getr (skymap, "constant", rval)) {
		    call fprintf (logfd, "    Use constant input sky: %g\n")
			call pargr (rval)
		} else {
		    call fprintf (logfd, "    Use input sky: %s\n")
			call pargstr (skyname)
		}
	    }
	}
	dosky = (skymap == NULL)

	# Check whether to compute a sky sigma.
	sigmap = NULL
	if (signame[1] != EOS) {
	    iferr (sigmap = map_open (signame, im)) {
		sigmap = NULL
		if (errcode() != 2)
		   call erract (EA_ERROR)
	    }
	    if (logfd != NULL && sigmap != NULL) {
		ifnoerr (call map_getr (sigmap, "constant", rval)) {
		    call fprintf (logfd, "    Use constant input sigma: %g\n")
			call pargr (rval)
		} else {
		    call fprintf (logfd, "    Use input sigma: %s\n")
			call pargstr (signame)
		}
	    }
	}
	dosig = (sigmap == NULL)

	# Compute the sky.
	if (dosky || dosig) {
	    # Set parameters.
	    call sky_pars ("open", "", par)

	    switch (SKY_TYPE(par)) {
	    case SKY_FIT:
		call sky_fit (SKY_SKF(par), dosky, dosig, im, bpm, expmap,
		    skyname, signame, skymap, sigmap, logfd)
	    case SKY_BLOCK:
		call sky_fit (SKY_SKF(par), dosky, dosig, im, bpm, expmap,
		    "", "", skymap, sigmap, logfd)
		call map_seti (skymap, "sample", 5)
		call map_seti (sigmap, "sample", 5)
		call sky_block (SKY_SKB(par), dosky, dosig, im, bpm, expmap,
		    skyname, signame, skymap, sigmap, logfd)
	    default:
		call error (1, "Unknown sky type")
	    }
	}

	call sfree (sp)
end
