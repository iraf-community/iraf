include	"refspectra.h"

# REFAVERAGE -- Assign reference spectrum by averageing reference list.
# In earlier version the reference apertures were always set to all

procedure refaverage (input, refs)

pointer	input			# List of input spectra
pointer	refs			# List of reference spectra

int	ap
double	sortval
real	wt1, wt2
pointer	sp, image, ref1, ref2, gval

bool	refgref(), refginput()
int	imtgetim(), imtlen()

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (ref1, SZ_FNAME, TY_CHAR)
	call salloc (ref2, SZ_FNAME, TY_CHAR)

	# Get reference spectra to average.
	switch (imtlen (refs)) {
	case 0:
	    call error (0, "No reference spectra specified")
	case 1:
	    ap = imtgetim (refs, Memc[ref1], SZ_FNAME)
	    call refnoextn (Memc[ref1])
	    if (!refgref (Memc[ref1], ap, sortval, gval)) {
		call sfree (sp)
		return
	    }
	    wt1 = 1.
	    wt2 = 0.
	case 2:
	    ap = imtgetim (refs, Memc[ref1], SZ_FNAME)
	    ap = imtgetim (refs, Memc[ref2], SZ_FNAME)
	    call refnoextn (Memc[ref1])
	    call refnoextn (Memc[ref2])
	    if (!refgref (Memc[ref1], ap, sortval, gval)) {
		call sfree (sp)
		return
	    }
	    if (!refgref (Memc[ref2], ap, sortval, gval)) {
		call sfree (sp)
		return
	    }
	    wt1 = 0.5
	    wt2 = 0.5
	default:
	    ap = imtgetim (refs, Memc[ref1], SZ_FNAME)
	    ap = imtgetim (refs, Memc[ref2], SZ_FNAME)
	    call refnoextn (Memc[ref1])
	    call refnoextn (Memc[ref2])
	    if (!refgref (Memc[ref1], ap, sortval, gval)) {
		call sfree (sp)
		return
	    }
	    if (!refgref (Memc[ref2], ap, sortval, gval)) {
		call sfree (sp)
		return
	    }
	    wt1 = 0.5
	    wt2 = 0.5
	    call eprintf ("WARNING: Averaging only first two reference spectra")
	}

	# Assign reference spectra to each input spectrum.
	# Skip spectra which are not of the appropriate aperture
	# or have been assigned previously (unless overriding).

	while (imtgetim (input, Memc[image], SZ_FNAME) != EOF) {
	    call refnoextn (Memc[image])
	    if (!refginput (Memc[image], ap, sortval, gval))
		next

	    call refspectra (Memc[image], Memc[ref1], wt1, Memc[ref2], wt2)
	}

	call sfree (sp)
end
