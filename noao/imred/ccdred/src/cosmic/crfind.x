include	<math/gsurfit.h>

# CR_FIND -- Find cosmic ray candidates.
# This procedure is an interface to special procedures specific to a given
# window size.

procedure cr_find (cr, threshold, data, nc, nl, col, line,
    sf1, sf2, x, y, z, w)

pointer	cr						# Cosmic ray list
real	threshold					# Detection threshold
pointer	data[ARB]					# Data lines
int	nc						# Number of columns
int	nl						# Number of lines
int	col						# First column
int	line						# Center line
pointer	sf1, sf2					# Surface fitting
real	x[ARB], y[ARB], z[ARB], w[ARB]			# Surface arrays

pointer	a, b, c, d, e, f, g

begin
	switch (nl) {
	case 5:
	    a = data[1]
	    b = data[2]
	    c = data[3]
	    d = data[4]
	    e = data[5]
	    call cr_find5 (cr, threshold,  col, line, Memr[a], Memr[b],
		Memr[c], Memr[d], Memr[e], nc, sf1, sf2, x, y, z, w)
	case 7:
	    a = data[1]
	    b = data[2]
	    c = data[3]
	    d = data[4]
	    e = data[5]
	    f = data[6]
	    g = data[7]
	    call cr_find7 (cr, threshold,  col, line, Memr[a], Memr[b],
		Memr[c], Memr[d], Memr[e], Memr[f], Memr[g], nc,
		sf1, sf2, x, y, z, w)
	}
end


# CR_FIND7 -- Find cosmic rays candidates in 7x7 window.
# This routine finds cosmic rays candidates with the following algorithm.
#    1. If the pixel is not a local maximum relative to it's 48 neighbors
#	go on to the next pixel.
#    2. Identify the next strongest pixel in the 7x7 region.
#	This suspect pixel is excluded in the following.
#    2. Compute the flux of the 7x7 region excluding the cosmic ray
#	candidate and the suspect pixel.
#    3. The candidate must exceed the average flux per pixel by a specified
#	threshold.  If not go on to the next pixel.
#    4. Fit a plane to the border pixels (excluding the suspect pixel).
#    5. Subtract the background defined by the plane.
#    6. Determine a replacement value as the average of the four adjacent
#	pixels (excluding the suspect pixels).
#    7. Add the pixel to the cosmic ray candidate list.

procedure cr_find7 (cr, threshold, col, line, a, b, c, d, e, f, g, n,
	sf1, sf2, x, y, z, w)

pointer	cr						# Cosmic ray list
real	threshold					# Detection threshold
int	col						# First column
int	line						# Line
real	a[ARB], b[ARB], c[ARB], d[ARB]			# Image lines
real	e[ARB], f[ARB], g[ARB]				# Image lines
int	n						# Number of columns
pointer	sf1, sf2					# Surface fitting
real	x[49], y[49], z[49], w[49]			# Surface arrays

real	bkgd[49] 
int	i1, i2, i3, i4, i5, i6, i7, j, j1, j2
real	p, flux, replace, asumr()
pointer	sf

begin
	for (i4=4; i4<=n-3; i4=i4+1) {
	    # Must be local maxima.
	    p = d[i4]
	    if (p<a[i4]||p<b[i4]||p<c[i4]||p<e[i4]||p<f[i4]||p<g[i4])
		next
	    i1 = i4 - 3
	    if (p<a[i1]||p<b[i1]||p<c[i1]||p<d[i1]||p<e[i1]||p<f[i1]||p<g[i1])
		next
	    i2 = i4 - 2
	    if (p<a[i2]||p<b[i2]||p<c[i2]||p<d[i2]||p<e[i2]||p<f[i2]||p<g[i2])
		next
	    i3 = i4 - 1
	    if (p<a[i3]||p<b[i3]||p<c[i3]||p<d[i3]||p<e[i3]||p<f[i3]||p<g[i3])
		next
	    i5 = i4 + 1
	    if (p<a[i5]||p<b[i5]||p<c[i5]||p<d[i5]||p<e[i5]||p<f[i5]||p<g[i5])
		next
	    i6 = i4 + 2
	    if (p<a[i6]||p<b[i6]||p<c[i6]||p<d[i6]||p<e[i6]||p<f[i6]||p<g[i6])
		next
	    i7 = i4 + 3
	    if (p<a[i7]||p<b[i7]||p<c[i7]||p<d[i7]||p<e[i7]||p<f[i7]||p<g[i7])
		next

	    # Convert to a single array in surface fitting order. 
	    call amovr (a[i1], z[1], 7)
	    z[8] = b[i7]; z[9] = c[i7]; z[10] = d[i7]; z[11] = e[i7]
	    z[12] = f[i7]; z[13] = g[i7]; z[14] = g[i6]; z[15] = g[i5]
	    z[16] = f[i4]; z[17] = g[i3]; z[18] = g[i2]; z[19] = g[i1]
	    z[20] = f[i1]; z[21] = e[i1]; z[22] = d[i1]; z[23] = c[i1]
	    z[24] = b[i1]
	    call amovr (b[i2], z[25], 5)
	    call amovr (c[i2], z[30], 5)
	    call amovr (d[i2], z[35], 5)
	    call amovr (e[i2], z[40], 5)
	    call amovr (f[i2], z[45], 5)

	    # Find the highest point excluding the center.
	    j1 = 37; j2 = 1
	    do j = 2, 49 {
		if (j == j1)
		    next
		if (z[j] > z[j2])
		    j2 = j 
	    }

	    # Compute the flux excluding the extreme points.
	    flux = (asumr (z, 49) - z[j1] - z[j2]) / 47

	    # Pixel must be exceed specified threshold.
	    if (p < flux + threshold)
		next

	    # Fit and subtract the background.
	    if (j2 < 25) {
	        w[j2] = 0
		sf = sf2
	        call gsfit (sf, x, y, z, w, 24, WTS_USER, j)
		w[j2] = 1
	    } else {
		sf = sf1
	        call gsrefit (sf, x, y, z, w, j)
	    }

	    call gsvector (sf, x, y, bkgd, 49)
	    call asubr (z, bkgd, z, 49)
	    p = z[j1]

	    # Compute the flux excluding the extreme points.
	    flux = (asumr (z, 49) - z[j1] - z[j2]) / 47

	    # Determine replacement value from four nearest neighbors again
	    # excluding the most deviant pixels.
	    replace = 0
	    j = 0
	    if (j2 != 32) {
		replace = replace + c[i4]
		j = j + 1
	    }
	    if (j2 != 36) {
		replace = replace + d[i3]
		j = j + 1
	    }
	    if (j2 != 38) {
		replace = replace + d[i5]
		j = j + 1
	    }
	    if (j2 != 42) {
		replace = replace + e[i4]
		j = j + 1
	    }
	    replace = replace / j

	    # Add pixel to cosmic ray list.
	    flux = 100. * flux
	    call cr_add (cr, col+i4-1, line, flux, flux/p, 0., replace, 0)
	    i4 = i7
	}
end


# CR_FIND5 -- Find cosmic rays candidates in 5x5 window.
# This routine finds cosmic rays candidates with the following algorithm.
#    1. If the pixel is not a local maximum relative to it's 24 neighbors
#	go on to the next pixel.
#    2. Identify the next strongest pixel in the 5x5 region.
#	This suspect pixel is excluded in the following.
#    2. Compute the flux of the 5x5 region excluding the cosmic ray
#	candidate and the suspect pixel.
#    3. The candidate must exceed the average flux per pixel by a specified
#	threshold.  If not go on to the next pixel.
#    4. Fit a plane to the border pixels (excluding the suspect pixel).
#    5. Subtract the background defined by the plane.
#    6. Determine a replacement value as the average of the four adjacent
#	pixels (excluding the suspect pixels).
#    7. Add the pixel to the cosmic ray candidate list.

procedure cr_find5 (cr, threshold, col, line, a, b, c, d, e, n,
	sf1, sf2, x, y, z, w)

pointer	cr						# Cosmic ray list
real	threshold					# Detection threshold
int	col						# First column
int	line						# Line
real	a[ARB], b[ARB], c[ARB], d[ARB], e[ARB]		# Image lines
int	n						# Number of columns
pointer	sf1, sf2					# Surface fitting
real	x[25], y[25], z[25], w[25]			# Surface arrays

real	bkgd[25] 
int	i1, i2, i3, i4, i5, j, j1, j2
real	p, flux, replace, asumr()
pointer	sf

begin
	for (i3=3; i3<=n-2; i3=i3+1) {
	    # Must be local maxima.
	    p = c[i3]
	    if (p<a[i3]||p<b[i3]||p<d[i3]||p<e[i3])
		next
	    i1 = i3 - 2
	    if (p<a[i1]||p<b[i1]||p<c[i1]||p<d[i1]||p<e[i1])
		next
	    i2 = i3 - 1
	    if (p<a[i2]||p<b[i2]||p<c[i2]||p<d[i2]||p<e[i2])
		next
	    i4 = i3 + 1
	    if (p<a[i4]||p<b[i4]||p<c[i4]||p<d[i4]||p<e[i4])
		next
	    i5 = i3 + 2
	    if (p<a[i5]||p<b[i5]||p<c[i5]||p<d[i5]||p<e[i5])
		next

	    # Convert to a single array in surface fitting order. 
	    call amovr (a[i1], z[1], 5)
	    z[6] = b[i5]; z[7] = c[i5]; z[8] = d[i5]; z[9] = e[i5]
	    z[10] = e[i4]; z[11] = e[i3]; z[12] = e[i2]; z[13] = e[i1]
	    z[14] = d[i1]; z[15] = c[i1]; z[16] = b[i1]
	    call amovr (b[i2], z[17], 3)
	    call amovr (c[i2], z[20], 3)
	    call amovr (d[i2], z[23], 3)

	    # Find the highest point excluding the center.
	    j1 = 21; j2 = 1
	    do j = 2, 25 {
		if (j == j1)
		    next
		if (z[j] > z[j2])
		    j2 = j 
	    }

	    # Compute the flux excluding the extreme points.
	    flux = (asumr (z, 25) - z[j1] - z[j2]) / 23

	    # Pixel must be exceed specified threshold.
	    if (p < flux + threshold)
		next

	    # Fit and subtract the background.
	    if (j2 < 17) {
	        w[j2] = 0
		sf = sf2
	        call gsfit (sf, x, y, z, w, 16, WTS_USER, j)
		w[j2] = 1
	    } else {
		sf = sf1
	        call gsrefit (sf, x, y, z, w, j)
	    }

	    call gsvector (sf, x, y, bkgd, 25)
	    call asubr (z, bkgd, z, 25)
	    p = z[j1]

	    # Compute the flux excluding the extreme points.
	    flux = (asumr (z, 25) - z[j1] - z[j2]) / 23

	    # Determine replacement value from four nearest neighbors again
	    # excluding the most deviant pixels.
	    replace = 0
	    j = 0
	    if (j2 != 18) {
		replace = replace + b[i3]
		j = j + 1
	    }
	    if (j2 != 20) {
		replace = replace + c[i2]
		j = j + 1
	    }
	    if (j2 != 22) {
		replace = replace + c[i4]
		j = j + 1
	    }
	    if (j2 != 24) {
		replace = replace + d[i3]
		j = j + 1
	    }
	    replace = replace / j

	    # Add pixel to cosmic ray list.
	    flux = 100. * flux
	    call cr_add (cr, col+i3-1, line, flux, flux/p, 0., replace, 0)
	    i3 = i5
	}
end
