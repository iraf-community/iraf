include	<imhdr.h>
include	"sensfunc.h"


# SF_OUTPUT -- Write the sensitivity function image.

procedure sf_output (stds, nstds, cv, output, ignoreaps)

pointer	stds[nstds]		# Standard star data
int	nstds			# Number of standard stars
pointer	cv			# Sensitivity function curve
char	output[SZ_FNAME]	# Output root image name (must be SZ_FNAME)
bool	ignoreaps		# Ignore apertures?

int	i, nw, axis[2], scan(), nscan()
real	w1, dw, cveval()
pointer	sp, fname, std, im, mw, buf, immap(), mw_open(), impl1r()
errchk	imaddi, imaddr

data	axis/1,2/
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
	if (ignoreaps) {
	    call strcpy (output, Memc[fname], SZ_FNAME)
	} else {
	    call sprintf (Memc[fname], SZ_FNAME, "%s.%04d")
		call pargstr (output)
		call pargi (STD_BEAM(std))
	}

	iferr (im = immap (Memc[fname], NEW_IMAGE, 0)) {
	    call printf ("Cannot create %s --  Enter new name: ")
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

	if (ignoreaps) {
	    call printf ("Sensitivity function for all apertures --> %s\n")
		call pargstr (Memc[fname])
	} else {
	    call printf ("Sensitivity function for aperture %2d --> %s\n")
		call pargi (STD_BEAM(std))
		call pargstr (Memc[fname])
	}
	call flush (STDOUT)

	IM_NDIM(im) = 1
	IM_LEN(im,1) = STD_NPTS(std)
	IM_PIXTYPE(im) = TY_REAL

	# Create a title from the STD file name
	if (ignoreaps) {
	    call sprintf (IM_TITLE(im), SZ_FNAME,
		"Sensitivity function for all apertures")
	} else {
	    call sprintf (IM_TITLE(im), SZ_FNAME,
		"Sensitivity function for aperture %d")
		call pargi (STD_BEAM(std))
	}

	# Set WCS
	mw = mw_open (NULL, 2)
	call mw_newsystem (mw, "multispec", 2)
	call mw_swtype (mw, axis, 2, "multispec",
	    "label=Wavelength units=Angstroms")
	nw = STD_NPTS(std)
	w1 = STD_WSTART(std)
	dw = (STD_WEND(std) - w1) / (nw - 1)
	call shdr_swattrs (mw, 1, STD_BEAM(std), STD_BEAM(std), 0,
	    double(w1), double(dw), nw, 0D0, INDEFD, INDEFD, "")
	call smw_saveim (mw, im)
	call mw_close (mw)

	# Set sensitivity data
	buf = impl1r (im)
	do i = 0, nw-1
	    Memr[buf+i] = cveval (cv, w1 + i * dw)

	call imunmap (im)

	call sfree (sp)
end
