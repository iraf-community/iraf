include <error.h>
include <imhdr.h>
include <mwset.h>

# T_WCSRESET -- Initialize the image wcs. The user can initialize the
# pre-defined "physical" or "world" coodinate systems, or a named
# user world coordinate system, for example the "multipsec" world
# coordinate system.  If the image does not have a previously defined wcs
# then wcsreset will create the identify wcs.

procedure t_wcsreset ()

bool	verbose
int	ndim
pointer	sp, imnamelist, image, wcs, system
pointer	r, w, cd, ncd, nr, ltv, iltm, ltm
pointer	imlist, im, mwim, mw
bool    clgetb(), streq()
int	imtgetim(), mw_stati()
pointer	imtopen(), immap(), mw_openim(), mw_open()
errchk	mw_openim()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (imnamelist, SZ_FNAME, TY_CHAR)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (wcs, SZ_FNAME, TY_CHAR)
	call salloc (system, SZ_FNAME, TY_CHAR)

	# Get the parameters.
	call clgstr ("image", Memc[imnamelist], SZ_FNAME)
	call clgstr ("wcs", Memc[wcs], SZ_FNAME)
	verbose = clgetb ("verbose")

	# Loop through the list of images.
	imlist = imtopen (Memc[imnamelist])
	while (imtgetim (imlist, Memc[image], SZ_FNAME) != EOF) {

	    # Remove any image section.
	    call imgimage (Memc[image], Memc[image], SZ_FNAME)

	    # Open the image.
	    im = immap (Memc[image], READ_WRITE, 0)
	    iferr {
	        if (verbose) {
	            call printf ("Initializing wcs %s for image %s\n")
		        call pargstr (Memc[wcs])
		        call pargstr (Memc[image])
	        }
	        mwim = mw_openim (im)
	    } then {
		mwim = NULL
	    } else {
	        call mw_gsystem (mwim, Memc[system], SZ_FNAME)
	    }

	    # Reset the lterm only if the wcs is "physical".
	    if (streq (Memc[wcs], "physical") && mwim != NULL) {

	        # Allocate space for the transforms.
	        ndim = mw_stati (mwim, MW_NPHYSDIM)
	        call malloc (r, ndim * ndim, TY_DOUBLE)
	        call malloc (w, ndim * ndim, TY_DOUBLE)
	        call malloc (cd, ndim * ndim, TY_DOUBLE)
	        call malloc (ltm, ndim * ndim, TY_DOUBLE)
	        call malloc (ltv, ndim, TY_DOUBLE)
	        call malloc (iltm, ndim * ndim, TY_DOUBLE)
	        call malloc (nr, ndim * ndim, TY_DOUBLE)
	        call malloc (ncd, ndim * ndim, TY_DOUBLE)

		call mw_gwtermd (mwim, Memd[r], Memd[w], Memd[cd], ndim)
		call mw_gltermd (mwim, Memd[ltm], Memd[ltv], ndim)
		call mwvmuld (Memd[ltm], Memd[r], Memd[nr], ndim)
		call aaddd (Memd[nr], Memd[ltv], Memd[nr], ndim)
		call mwinvertd (Memd[ltm], Memd[iltm], ndim)
		call mwmmuld (Memd[cd], Memd[iltm], Memd[ncd], ndim)
		call mw_swtermd (mwim, Memd[nr], Memd[w], Memd[ncd], ndim)
		call wcs_terminit (Memd[ltm], Memd[ltv], ndim)
		call mw_sltermd (mwim, Memd[ltm], Memd[ltv], ndim)
	        call mw_saveim (mwim, im)

	        # Free the space.
	        call mfree (r, TY_DOUBLE)
	        call mfree (w, TY_DOUBLE)
	        call mfree (cd, TY_DOUBLE)
	        call mfree (ncd, TY_DOUBLE)
	        call mfree (nr, TY_DOUBLE)
	        call mfree (ltm, TY_DOUBLE)
	        call mfree (ltv, TY_DOUBLE)
	        call mfree (iltm, TY_DOUBLE)

	    # Cannot replace physical system for unknown world system.
	    } else if (streq (Memc[wcs], "physical") && mwim == NULL) {
	        if (verbose) {
	            call printf ("\tCannot initialize wcs %s for image %s\n")
		        call pargstr (Memc[wcs])
		        call pargstr (Memc[image])
	        }
	    } else if (streq (Memc[wcs], "world") || streq (Memc[wcs],
	        Memc[system])) {

	        ndim = IM_NDIM(im)
		mw = mw_open (NULL, ndim)
	        call mw_saveim (mw, im)
		call mw_close (mw)

	    # The named wcs is not present.
	    } else {
		call eprintf ("\tCannot find wcs %s\n")
		    call pargstr (Memc[wcs])
	    }

	    if (mwim != NULL)
	        call mw_close (mwim)

	    call imunmap (im)

	}

	call imtclose (imlist)

	call sfree (sp)
end


# WCS_TERMINIT -- Initialize the shift term and rotation matrix.

procedure wcs_terminit (ltm, ltv, ndim)

double	ltm[ndim,ndim]		# the rotation matrix
double	ltv[ndim]		# the shift vector
int	ndim			# the number of dimensions

int	i

begin
	call aclrd (ltm, ndim * ndim)
	do i = 1, ndim
	    ltm[i,i] = 1.0d0
	call aclrd (ltv, ndim)
end
