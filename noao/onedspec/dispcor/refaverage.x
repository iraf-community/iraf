include	"refspectra.h"

# REFAVERAGE -- Assign reference spectrum by averageing reference list.

procedure refaverage (input, refs)

pointer	input			# List of input spectra
pointer	refs			# List of reference spectra

pointer	aps			# Input aperture selection list
pointer	raps			# Reference aperture selection list

int	ap
real	sortval, wt1, wt2
pointer	sp, image, ref1, ref2

bool	refgref(), refginput()
int	odr_getim(), odr_len(), decode_ranges()

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (ref1, SZ_FNAME, TY_CHAR)
	call salloc (ref2, SZ_FNAME, TY_CHAR)
	call salloc (aps, 3*NRANGES, TY_INT)
	call salloc (raps, 3*NRANGES, TY_INT)

	# Task parameters
	call clgstr ("apertures", Memc[image], SZ_FNAME)
	if (decode_ranges (Memc[image], Memi[aps], NRANGES, ap) == ERR)
	    call error (0, "Bad aperture list")
#	call clgstr ("refaps", Memc[image], SZ_FNAME)
#	if (decode_ranges (Memc[image], Memi[raps], NRANGES, ap) == ERR)
#	    call error (0, "Bad reference aperture list")
	if (decode_ranges ("", Memi[raps], NRANGES, ap) == ERR)
	    call error (0, "Bad reference aperture list")

	# Get reference spectra to average.
	switch (odr_len (refs)) {
	case 0:
	    call error (0, "No reference spectra specified")
	case 1:
	    ap = odr_getim (refs, Memc[ref1], SZ_FNAME)
	    if (!refgref (Memc[ref1], raps, "", false, 0., ap, sortval)) {
		call sfree (sp)
		return
	    }
	    wt1 = 1.
	    wt2 = 0.
	case 2:
	    ap = odr_getim (refs, Memc[ref1], SZ_FNAME)
	    ap = odr_getim (refs, Memc[ref2], SZ_FNAME)
	    if (!refgref (Memc[ref1], raps, "", false, 0., ap, sortval)) {
		call sfree (sp)
		return
	    }
	    if (!refgref (Memc[ref2], raps, "", false, 0., ap, sortval)) {
		call sfree (sp)
		return
	    }
	    wt1 = 0.5
	    wt2 = 0.5
	default:
	    ap = odr_getim (refs, Memc[ref1], SZ_FNAME)
	    ap = odr_getim (refs, Memc[ref2], SZ_FNAME)
	    if (!refgref (Memc[ref1], raps, "", false, 0., ap, sortval)) {
		call sfree (sp)
		return
	    }
	    if (!refgref (Memc[ref2], raps, "", false, 0., ap, sortval)) {
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

	while (odr_getim (input, Memc[image], SZ_FNAME) != EOF) {
	    if (!refginput (Memc[image], aps, "", false, 0., ap, sortval))
		next

	    call refspectra (Memc[image], Memc[ref1], wt1, Memc[ref2], wt2)
	}

	call sfree (sp)
end
