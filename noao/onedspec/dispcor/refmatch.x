include	"refspectra.h"

# REFMATCH -- Assign reference spectrum by match against reference list.

procedure refmatch (input, refs)

pointer	input			# List of input spectra
pointer	refs			# List of reference spectra

int	ap
double	sortval
pointer	sp, image, refimage, gval

bool	refgref(), refginput()
int	imtgetim(), imtlen()

begin
	if (imtlen (input) != imtlen (refs))
	    call error (0, "Input and reference list have different lengths")

	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (refimage, SZ_FNAME, TY_CHAR)

	# Assign reference spectra to each input spectrum.
	# Skip spectra which are not of the appropriate aperture
	# or have been assigned previously (unless overriding).

	while ((imtgetim (input, Memc[image], SZ_FNAME) != EOF) &&
	       (imtgetim (refs, Memc[refimage], SZ_FNAME) != EOF)) {
	    call refnoextn (Memc[image])
	    call refnoextn (Memc[refimage])
	    if (!refginput (Memc[image], ap, sortval, gval))
		next
	    if (!refgref (Memc[refimage], ap, sortval, gval))
		next

	    call refspectra (Memc[image], Memc[refimage], 1.,
		    Memc[refimage], 0.)
	}

	call sfree (sp)
end
