include <imhdr.h>
include <fset.h>
include "ms.h"

# T_FIND_PEAKS -- Find the spectra peaks in a MULTISPEC image and record
# their positions in the database.
#
# An average of naverage lines from the MULTISPEC image is searched
# for peaks satisfying constraints on the minimum and maximum number,
# columns, peak values, and separation between peaks.  The positions
# of the peaks satisfying these constraints is entered in the database.
# It is an error if fewer than the minimum number of peaks is found
# or if the number of peaks differs from a previously determined number.
# The peak finding is done by the function FIND_PEAKS which is numerical
# and may be used outside the MULTISPEC package.

procedure t_find_peaks ()

# CL parameters:
char	image[SZ_FNAME]		# Image to be searched
int	lines[3, MAX_RANGES]	# Image lines in which to find spectra
int	min_npeaks		# Minimum number of spectra to be found
int	max_npeaks		# Maximum number of spectra to be accepted
int	separation		# Minimum pixel separation between spectra
int	edge			# Minimum distance to edge of image
real	threshold		# Minimum peak value
real	contrast		# Max contrast between strongest and weakest
int	columns[3, MAX_RANGES]	# Spectra positions limited to these columns
int	naverage		# Number of image lines to average
bool	debug			# Print debugging information

char	comment[SZ_LINE]
int	i, j, k, line, sample, nsamples, npoints, nspectra
pointer	ms, im
pointer	sp, data, x, samples

int	find_peaks(), get_sample_lines()
int	clgeti(), clgranges()
real	clgetr()
bool	clgetb(), is_in_range()
pointer	msmap(), immap()

begin
	# Get task parameters and access files.
	call clgstr ("image", image, SZ_FNAME)
	ms = msmap (image, READ_WRITE, 0)
	im = immap (image, READ_ONLY, 0)
	i = clgranges ("lines", 1, IM_LEN(im, 2), lines, MAX_RANGES)
	min_npeaks = clgeti ("min_npeaks")
	max_npeaks = clgeti ("max_npeaks")
	separation = clgeti ("separation")
	edge = clgeti ("edge")
	threshold = clgetr ("threshold")
	contrast = clgetr ("contrast")
	i = clgranges ("columns", 1, IM_LEN(im, 1), columns, MAX_RANGES)
	naverage = clgeti ("naverage")
	debug = clgetb ("debug")

	call fseti (STDOUT, F_FLUSHNL, YES)

	# Allocate working memory.
	npoints = IM_LEN(im, 1)
	call smark (sp)
	call salloc (samples, MS_NSAMPLES(ms), TY_INT)
	call salloc (data, npoints, TY_REAL)
	call salloc (x, npoints, TY_REAL)

	# Get the sample lines.
	nsamples = get_sample_lines (ms, lines, Memi[samples])

	# Loop through each sample line.
	do i = 1, nsamples {
	    sample = Memi[samples + i - 1]
	    line = LINE(ms, sample)

	    # Get the image data with averaging.
	    call  msgimage (im, line, naverage, Memr[data])

	    # Mark columns which are to be ignored with INDEFR.
	    do j = 1, npoints
	        if (!is_in_range (columns, j))
		    Memr[data + j - 1] = INDEFR

	    # Find the peaks.
	    nspectra = find_peaks (Memr[data], Memr[x], npoints,
		contrast, separation, edge, max_npeaks, threshold, debug)

	    if (debug) {
	        call printf ("  Number of spectra found in line %d = %d.\n")
		    call pargi (line)
	            call pargi (nspectra)
	    }
	    if (nspectra < min_npeaks)
		call error (MS_ERROR, "Too few spectra found")

	    # Enter the spectra found in the database.  If the number of
	    # spectra has not been previously set in the database then
	    # enter the number of spectra and make entries in the
	    # database.  Otherwise check that the number of spectra found
	    # agrees with that already in the database.

	    if (MS_NSPECTRA(ms) == 0) {
	        if (nspectra == 0)
		    next
		MS_NSPECTRA(ms) = nspectra
	        call dbenter (MS_DB(ms), NAME(ms, I0), nspectra * SZ_REAL,
		    MS_NSAMPLES(ms))
	        call dbenter (MS_DB(ms), NAME(ms, X0), nspectra * SZ_REAL,
		    MS_NSAMPLES(ms))
	    } else if (MS_NSPECTRA(ms) != nspectra)
		call error (MS_ERROR, "Attempt to change the number of spectra")

	    call msgparam (ms, X0, sample)
	    call amovr (Memr[x], PARAMETER(ms, X0, 1), nspectra)
	    call mspparam (ms, X0, sample)

	    # The peak scale is taken and the pixel value at the peak.
	    call msgparam (ms, I0, sample)
	    do j = 1, nspectra {
		k = PARAMETER(ms, X0, j)
		PARAMETER(ms, I0, j) = Memr[data + k - 1]
	    }
	    call mspparam (ms, I0, sample)

	    # Enter a comment in the database.
	    call sprintf (comment, SZ_LINE,
		"Spectra located in sample line %d.")
		call pargi (sample)
	    call history (ms, comment)
	}

	# Update the database and close the database and image.
	call msphdr (ms)
	call msunmap (ms)
	call imunmap (im)
	call sfree (sp)
end
