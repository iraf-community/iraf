include	<imhdr.h>
include	"sensfunc.h"


# SF_OUTPUT -- Write the sensitivity function image.

procedure sf_output (stds, nstds, cv, output)

pointer	stds[nstds]		# Standard star data
int	nstds			# Number of standard stars
pointer	cv			# Sensitivity function curve
char	output[SZ_FNAME]	# Output root image name (must be SZ_FNAME)

int	i, npts, scan(), nscan()
real	crval1, cdelt1, cveval()
pointer	sp, fname, std, im, buf, immap(), impl1r()
errchk	imaddi, imaddr

define	makeim_	99

begin
	# Return if no output root sensitivity imagename is specified.
	if (output[1] == EOS)
	    return

	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	# Determine a valid standard star to get aperture number.
	do i = 1, nstds
	    if (STD_FLAG(stds[i]) != SF_EXCLUDE) {
		std = stds[i]
		break
	    }

	# Make output image name with aperture number appended.  If the
	# image exists allow the user to change root name.
makeim_
	call sprintf (Memc[fname], SZ_FNAME, "%s.%04d")
	    call pargstr (output)
	    call pargi (STD_BEAM(std))

	iferr (im = immap (Memc[fname], NEW_IMAGE, 0)) {
	    call printf ("Cannot create %s --  Enter new root name: ")
		call pargstr (Memc[fname])
	    call flush (STDOUT)
	    if (scan() != EOF) {
	        call gargwrd (Memc[fname], SZ_FNAME)
	        if (nscan() == 1) {
	    	    call strcpy (Memc[fname], output, SZ_FNAME)
	    	    goto makeim_
		}
	    }
	    call printf ("No sensitivity function created for aperture %2d\n")
	        call pargi (STD_BEAM(std))
	    call flush (STDOUT)
	    return
	}

	call printf ("Sensitivity function for aperture %2d --> %s\n")
	    call pargi (STD_BEAM(std))
	    call pargstr (Memc[fname])
	call flush (STDOUT)

	IM_NDIM(im) = 1
	IM_LEN(im,1) = STD_NPTS(std)
	IM_PIXTYPE(im) = TY_REAL

	# Create a title from the STD file name
	call sprintf (IM_TITLE(im), SZ_FNAME,
	    "Sensitivity function for aperture %d")
	    call pargi (STD_BEAM(std))

	npts = STD_NPTS(std)
	crval1 = STD_WSTART(std)
	cdelt1 = (STD_WEND(std) - crval1) / (npts - 1)

	call imaddi (im, "crpix1", 1)
	call imaddr (im, "crval1", crval1)
	call imaddr (im, "cdelt1", cdelt1)
	call imaddr (im, "cd1_1", cdelt1)
	call imaddr (im, "w0", crval1)
	call imaddr (im, "wpc", cdelt1)
	call imaddi (im, "dc-flag", 0)
	call imaddi (im, "beam-num", STD_BEAM(std))

	buf = impl1r (im,1)
	do i = 0, npts-1
	    Memr[buf+i] = cveval (cv, crval1 + i * cdelt1)

	call imunmap (im)

	call sfree (sp)
end
