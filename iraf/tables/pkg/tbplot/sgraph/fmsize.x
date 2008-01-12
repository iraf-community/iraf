procedure fmsize (gd, size, xsize, ysize)

# Adjust the marker size for the device aspect ratio.
# Make the marker smaller in NDC along the longer side of the frame.

#  9/13/91 ZGL

pointer	gd			# Graphics descriptor
real	size			# Specified marker size
real	xsize, ysize		# Adjusted sizes

real	xs, ys			# Device size (meters)
real	ar			# Aspect ratio

real	ggetr()

begin
	# Find the frame size in the graphcap (we must trust this)
	xs = ggetr (gd, "xs")
	ys = ggetr (gd, "ys")
	ar = ys / xs

	if (ar > 1.0) {
	    # Portrait (vertical)
	    xsize = size
	    ysize = size / ar

	} else if (ar < 1.0) {
	    # Landscape (horizontal)
	    xsize = size * ar
	    ysize = size

	} else {
	    # Square
	    xsize = size
	    ysize = size
	}
end
