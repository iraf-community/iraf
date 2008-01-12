# AP_CLIP -- Clip the ends of a sorted pixel distribution by a certain
# percent.

int procedure ap_clip (skypix, index, npix, loclip, hiclip, loindex, hiindex)

real	skypix[ARB]		# the unsorted array of sky pixels
int	index[ARB]		# the array of sorted indices
int	npix			# the number of sky pixels
real	loclip, hiclip		# the clipping factors in percent
int	loindex, hiindex	# the clipping indices

begin
	# Sort the pixels.
	call apqsort (skypix, index, index, npix)

	# Determine the clipping factors.
	loindex = nint (0.01 * loclip * npix) + 1
	hiindex = npix - nint (0.01 * hiclip * npix)
	if ((hiindex - loindex + 1) <= 0)
	    return (npix)
	else
	    return (loindex - 1 + npix - hiindex)
end
