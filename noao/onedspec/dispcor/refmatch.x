include	"refspectra.h"

# REFMATCH -- Assign reference spectrum by match against reference list.

procedure refmatch (input, refs)

pointer	input			# List of input spectra
pointer	refs			# List of reference spectra

pointer	aps			# Input aperture selection list
pointer	raps			# Reference aperture selection list

int	ap
real	sortval
pointer	sp, image, refimage

bool	refgref(), refginput()
int	odr_getim(), odr_len(), decode_ranges()

begin
	if (odr_len (input) != odr_len (refs))
	    call error (0, "Input and reference list have different lengths")

	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (refimage, SZ_FNAME, TY_CHAR)
	call salloc (aps, 3*NRANGES, TY_INT)
	call salloc (raps, 3*NRANGES, TY_INT)

	# Task parameters
	call clgstr ("apertures", Memc[image], SZ_FNAME)
	if (decode_ranges (Memc[image], Memi[aps], NRANGES, ap) == ERR)
	    call error (0, "Bad aperture list")
	call clgstr ("refaps", Memc[image], SZ_FNAME)
	if (decode_ranges (Memc[image], Memi[raps], NRANGES, ap) == ERR)
	    call error (0, "Bad reference aperture list")

	# Assign reference spectra to each input spectrum.
	# Skip spectra which are not of the appropriate aperture
	# or have been assigned previously (unless overriding).

	while ((odr_getim (input, Memc[image], SZ_FNAME) != EOF) &&
	       (odr_getim (refs, Memc[refimage], SZ_FNAME) != EOF)) {
	    if (!refginput (Memc[image], aps, "", false, 0., ap, sortval))
		next
	    if (!refgref (Memc[refimage], raps, "", false, 0., ap, sortval))
		next

	    call refspectra (Memc[image], Memc[refimage], 1.,
		    Memc[refimage], 0.)
	}

	call sfree (sp)
end
