include	"apertures.h"

define	NRANGES	50

# AP_RECENTER -- Recenter apertures.

procedure ap_recenter (image, line, nsum, aps, naps, apedit)

char	image[SZ_FNAME]		# Image name
int	line			# Image dispersion line
int	nsum			# Number of dispersion lines to sum
pointer	aps[ARB]		# Aperture pointers
int	naps			# Number of apertures
int	apedit			# Called by apedit?

pointer	ranges			# Apertures to select
int	npeaks			# Number of bright peaks to select 
bool	shift			# Shift instead of center?

real	center, delta
int	i, j, k, npts, apaxis
pointer	sp, str, im, imdata, title, index, peaks, deltas

int	decode_ranges()
real	apgetr(), ap_center(), cveval(), asokr()
bool	clgetb(), ap_answer(), apgetb(), is_in_range()
errchk	ap_getdata

begin
	# Check if apertures are defined.
	if (naps < 1)
	    return

	# Query user.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	if (apedit == NO) {
	    call sprintf (Memc[str], SZ_LINE, "Recenter apertures for %s?")
	        call pargstr (image)
	    if (!ap_answer ("ansrecenter", Memc[str])) {
	        call sfree (sp)
	        return
	    }

	    if (clgetb ("verbose"))
	        call printf ("Recentering apertures ...\n")
	}

	# Get parameters
	call apgstr ("apertures", Memc[str], SZ_LINE)
	delta = apgetr ("npeaks")
	shift = apgetb ("shift")

	call salloc (ranges, 3*NRANGES, TY_INT)
	if (decode_ranges (Memc[str], Memi[ranges], NRANGES, i) == ERR)
	    call error (0, "Bad aperture list")
	if (IS_INDEFR (delta))
	    npeaks = naps
	else if (delta < 1.)
	    npeaks = max (1., delta * naps)
	else
	    npeaks = delta

	# Map the image and get the image data.
	call ap_getdata (image, line, nsum, im, imdata, npts, apaxis, title)

	j = 0
	if (npeaks == naps && !shift) {
	    do i = 1, naps {
	        if (!is_in_range (Memi[ranges], i))
		    next
	        center = AP_CEN(aps[i], apaxis) +
		    cveval (AP_CV(aps[i]), real (line))
	        center = ap_center (center, Memr[imdata], npts)
	        if (!IS_INDEF(center)) {
		    AP_CEN(aps[i], apaxis) = center -
		        cveval (AP_CV(aps[i]), real (line))
		    j = j + 1
		}
	    }
	} else {
	    call salloc (index, naps, TY_REAL)
	    call salloc (peaks, naps, TY_REAL)
	    call salloc (deltas, naps, TY_REAL)
	    do i = 1, naps {
	        if (!is_in_range (Memi[ranges], i))
		    next
	        center = AP_CEN(aps[i], apaxis) +
		    cveval (AP_CV(aps[i]), real (line))
	        delta = ap_center (center, Memr[imdata], npts)
	        if (!IS_INDEF(delta)) {
		    k = max (1, min (npts, int (delta+0.5)))
		    Memr[index+j] = i
		    Memr[peaks+j] = -Memr[imdata+k-1]
		    Memr[deltas+j] = delta - center
		    j = j + 1
	        }
	    }

	    if (j > 0 && npeaks > 0) {
	        if (npeaks < j) {
	            call xt_sort3 (Memr[peaks], Memr[deltas], Memr[index], j)
		    j = npeaks
	        }

	        if (shift) {
		    if (mod (j, 2) == 0)
			delta = (asokr (Memr[deltas], j, j/2) +
				asokr (Memr[deltas], j, 1+j/2)) / 2
		    else
			delta = asokr (Memr[deltas], j, 1+j/2)
		    j = naps
		    do i = 1, j {
		        center = AP_CEN(aps[i], apaxis) + delta
		        AP_CEN(aps[i], apaxis) = center
		    }
	        } else {
		    do k = 1, j {
		        delta = Memr[deltas+k-1]
		        i = Memr[index+k-1]
		        center = AP_CEN(aps[i], apaxis) + delta
		        AP_CEN(aps[i], apaxis) = center
		    }
		}
	    }
	}
		    
	# Log the operation, write the apertures to the database,
	# unmap the image and free memory.
	if (shift) {
	    call sprintf (Memc[str], SZ_LINE,
	        "RECENTER  - %d apertures shifted by %.2f for %s.")
	        call pargi (j)
		call pargr (delta)
	        call pargstr (image)
	} else {
	    call sprintf (Memc[str], SZ_LINE,
	        "RECENTER - %d apertures recentered for %s")
	        call pargi (j)
	        call pargstr (image)
	}
	if (apedit == NO)
	    call ap_log (Memc[str], YES, YES, NO)
	else
	    call ap_log (Memc[str], YES, NO, NO)

	call appstr ("ansdbwrite1", "yes")

	call mfree (imdata, TY_REAL)
	call mfree (title, TY_CHAR)
	call imunmap (im)
	call sfree (sp)
end
