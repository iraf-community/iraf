include	<mach.h>
include	"refspectra.h"


# REFINTERP -- Assign reference spectra to interpolate between based on sort
# key.  The nearest preceding and following spectra are assigned weights based
# on their distance.  If there is no preceding and following spectrum then
# the nearest spectrum is assigned.

procedure refinterp (input, refs)

pointer	input			# List of input spectra
pointer	refs			# List of reference spectra

pointer	sort			# Sort key
bool	time			# Is sorting key a time?
real	timewrap		# Time wrap
int	aps			# Input aperture selection list
int	raps			# Reference aperture selection list
bool	ignoreaps		# Ignore apertures?

int	i, i1, i2, nrefs, ap
real	sortval, d, d1, d2, wt1, wt2
pointer	sp, image, refimages, refaps, refvals

real	clgetr()
bool	clgetb(), streq(), refginput(), refgref()
int	odr_getim(), odr_len(), decode_ranges()

begin
	call smark (sp)
	call salloc (sort, SZ_FNAME, TY_CHAR)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (aps, 3*NRANGES, TY_INT)
	call salloc (raps, 3*NRANGES, TY_INT)

	# Task parameters
	call clgstr ("apertures", Memc[image], SZ_FNAME)
	if (decode_ranges (Memc[image], Memi[aps], NRANGES, i) == ERR)
	    call error (0, "Bad aperture list")
	call clgstr ("refaps", Memc[image], SZ_FNAME)
	if (decode_ranges (Memc[image], Memi[raps], NRANGES, i) == ERR)
	    call error (0, "Bad reference aperture list")
	ignoreaps = clgetb ("ignoreaps")
	call clgstr ("sort", Memc[sort], SZ_FNAME)
	time = clgetb ("time")
	timewrap = clgetr ("timewrap")

	# Tabulate reference spectra.  This expands the reference list,
	# checks the spectrum is a reference spectrum of the appropriate
	# aperture.

	call salloc (refimages, odr_len (refs), TY_INT)
	call salloc (refaps, odr_len (refs), TY_INT)
	call salloc (refvals, odr_len (refs), TY_REAL)
	nrefs = 0
	while (odr_getim (refs, Memc[image], SZ_FNAME) != EOF) {
	    if (!refgref (Memc[image], raps, Memc[sort], time, timewrap,
		ap, sortval))
		next

	    for (i=0; i<nrefs; i=i+1)
		if (streq (Memc[Memi[refimages+i]], Memc[image]))
		    break
	    if (i == nrefs) {
		call salloc (Memi[refimages+nrefs], SZ_FNAME, TY_CHAR)
		call strcpy (Memc[image], Memc[Memi[refimages+i]], SZ_FNAME)
		Memi[refaps+i] = ap
		Memr[refvals+i] = sortval
		nrefs = i + 1
	    }
	}
	if (nrefs < 1)
	    call error (0, "No reference images specified")


	# Assign following reference spectra to each input spectrum.
	# Skip input spectra which are not of the appropriate aperture
	# or have been assigned previously (unless overriding).

	while (odr_getim (input, Memc[image], SZ_FNAME) != EOF) {
	    if (!refginput (Memc[image], aps, Memc[sort], time, timewrap,
		ap, sortval))
		next

	    i1 = 0
	    i2 = 0
	    d1 = MAX_REAL
	    d2 = -MAX_REAL
	    do i = 1, nrefs {
	        if (!ignoreaps  && ap != Memi[refaps+i-1])
		    next
	        d = sortval - Memr[refvals+i-1]
	        if ((d >= 0.) && (d < d1)) {
		    i1 = i
		    d1 = d
	        } else if ((d <= 0.) && (d > d2)) {
		    i2 = i
		    d2 = d
	        }
	    }

	    if (i1 > 0 && i2 > 0) {	# Weight spectra
		if (d1 - d2 == 0.) {
		    wt1 = 0.5
		    wt2 = 0.5
		} else {
		    wt1 = -d2 / (d1 - d2)
		    wt2 =  d1 / (d1 - d2)
		}
		call refspectra (Memc[image], Memc[Memi[refimages+i1-1]], wt1,
		    Memc[Memi[refimages+i2-1]], wt2)
	    } else if (i1 > 0)	# Nearest preceding spectrum
		call refspectra (Memc[image], Memc[Memi[refimages+i1-1]], 1.,
		    Memc[Memi[refimages+i1-1]], 0.)
	    else if (i2 > 0)	# Nearest following spectrum
		call refspectra (Memc[image], Memc[Memi[refimages+i2-1]], 1.,
		    Memc[Memi[refimages+i2-1]], 0.)
	    else		# No reference spectrum found
		call refmsgs (NO_REFSPEC, Memc[image], ap, "", "")
	}

	call sfree (sp)
end
