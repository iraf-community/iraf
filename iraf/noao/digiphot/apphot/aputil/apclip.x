# AP_CLIP -- Clip the ends of a sorted pixel distribution by a certain
# percent.

long procedure ap_clip (skypix, index, npix, loclip, hiclip, loindex, hiindex)

real	skypix[ARB]		# the unsorted array of sky pixels
long	index[ARB]		# the array of sorted indices
size_t	npix			# the number of sky pixels
real	loclip, hiclip		# the clipping factors in percent
long	loindex, hiindex	# the clipping indices

long	lnint()

begin
	# Sort the pixels.
	call apqsort (skypix, index, index, npix)

	# Determine the clipping factors.
	loindex = lnint (0.01 * loclip * npix) + 1
	hiindex = npix - lnint (0.01 * hiclip * npix)
	if ((hiindex - loindex + 1) <= 0)
	    return (npix)
	else
	    return (loindex - 1 + npix - hiindex)
end
