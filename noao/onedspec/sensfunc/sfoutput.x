include	<mach.h>
include	<imhdr.h>
include	"sensfunc.h"


# SF_OUTPUT -- Write the sensitivity function image.

procedure sf_output (stds, nstds, cv, output, ignoreaps)

pointer	stds[nstds]		# Standard star data
int	nstds			# Number of standard stars
pointer	cv			# Sensitivity function curve
char	output[SZ_FNAME]	# Output root image name (must be SZ_FNAME)
bool	ignoreaps		# Ignore apertures?

int	i, ap, nw, scan(), nscan()
real	w1, w2, dw, dw1, aplow[2], aphigh[2], cveval()
pointer	sp, fname, std, im, mw, buf, immap(), mw_open(), impl1r()
errchk	imaddi, imaddr

define	makeim_	99

begin
	# Return if no output root sensitivity imagename is specified.
	if (output[1] == EOS)
	    return

	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	# Determine wavelength range and reference standard star.
	w1 = MAX_REAL
	w2 = -MAX_REAL
	dw = MAX_REAL
	do i = 1, nstds-2 {
	    if (STD_FLAG(stds[i]) == SF_EXCLUDE)
		next
	    std = stds[i]
	    dw1 = abs ((STD_WEND(std) - STD_WSTART(std)) / (STD_NPTS(std) - 1))
	    w1 = min (w1, STD_WSTART(std), STD_WEND(std))
	    w2 = max (w2, STD_WSTART(std), STD_WEND(std))
	    dw = min (dw, dw1)
	}
	nw = (w2 - w1) / dw + 1.5

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

	# Define the image header.
	IM_NDIM(im) = 1
	IM_LEN(im,1) = nw
	IM_PIXTYPE(im) = TY_REAL
	if (ignoreaps) {
	    call sprintf (IM_TITLE(im), SZ_FNAME,
		"Sensitivity function for all apertures")
	} else {
	    call sprintf (IM_TITLE(im), SZ_FNAME,
		"Sensitivity function for aperture %d")
		call pargi (STD_BEAM(std))
	}

	mw = mw_open (NULL, 1)
	call mw_newsystem (mw, "equispec", 1)
	call mw_swtype (mw, 1, 1, "linear", "label=Wavelength units=Angstroms")
	call smw_open (mw, NULL, im)
	ap = STD_BEAM(std)
	aplow[1] = INDEF
	aphigh[1] = INDEF
	aplow[2] = INDEF
	aphigh[2] = INDEF
	call smw_swattrs (mw, 1, 1, ap, STD_BEAM(std), 0,
	    double(w1), double(dw), nw, 0D0, aplow, aphigh, "")
	call smw_saveim (mw, im)
	call smw_close (mw)

	# Write sensitivity data.
	buf = impl1r (im)
	do i = 0, nw-1
	    Memr[buf+i] = cveval (cv, w1 + i * dw)

	# Notify user.
	call printf ("%s --> %s\n")
		call pargstr (IM_TITLE(im))
		call pargstr (Memc[fname])
	call flush (STDOUT)

	call imunmap (im)
	call sfree (sp)
end
