# AP_YLEVEL -- Set the aperture to intercept the specified y level.

procedure ap_ylevel (imdata, npts, y, center, low, high)

real	imdata[npts]		# Image data
int	npts			# Number of image points
real	y			# Y value
real	center			# Center of aperture
real	low, high		# Equal flux points

int	i1, i2, j1, j2
real	y1

begin
	if ((center < 1.) || (center >= npts))
	    return

	i1 = center
	i2 = i1 + 1
	y1 = imdata[i1] * (i2 - center) + imdata[i2] * (center - i1)

	if (y1 > y) {
	    for (j1 = i1; (j1 >= 1) && (imdata[j1] > y); j1 = j1 - 1)
		;
	    if (j1 >= 1) {
	        j2 = j1 + 1
	        low = (y + imdata[j2] * j1 - imdata[j1] * j2) /
		    (imdata[j2] - imdata[j1]) - center
	    }

	    for (j2 = i2; (j2 <= npts) && (imdata[j2] > y); j2 = j2 + 1)
		;
	    if (j2 <= npts) {
	        j1 = j2 - 1
	        high = (y + imdata[j2] * j1 - imdata[j1] * j2) /
		    (imdata[j2] - imdata[j1]) - center
	    }

	} else {
	    for (j1 = i1; (j1 >= 1) && (imdata[j1] < y); j1 = j1 - 1)
		;
	    if (j1 >= 1) {
	        j2 = j1 + 1
	        low = (y + imdata[j2] * j1 - imdata[j1] * j2) /
		    (imdata[j2] - imdata[j1]) - center
	    }

	    for (j2 = i2; (j2 <= npts) && (imdata[j2] < y); j2 = j2 + 1)
		;
	    if (j2 <= npts) {
	        j1 = j2 - 1
	        high = (y + imdata[j2] * j1 - imdata[j1] * j2) /
		    (imdata[j2] - imdata[j1]) - center
	    }
	}
end
