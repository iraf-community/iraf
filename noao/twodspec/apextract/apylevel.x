# AP_YLEVEL -- Set the aperture to intercept the specified y level.

procedure ap_ylevel (imdata, npts, ylevel, peak, bkg, grow, center, low, high)

real	imdata[npts]		# Image data
int	npts			# Number of image points
real	ylevel			# Y value
bool	peak			# Is y a fraction of peak?
bool	bkg			# Subtract a background?
real	grow			# Grow factor
real	center			# Center of aperture
real	low, high		# Equal flux points

int	i1, i2, j1, j2, k1, k2
real	y, y1, y2, a, b, ycut, x

begin
	if ((center < 1.) || (center >= npts) || IS_INDEF (ylevel))
	    return

	if (bkg) {
	    i1 = nint (center) 
	    i2 = max (1, nint (center + low))
	    for (k1=i1; k1 > i2 && imdata[k1] <= imdata[k1-1]; k1=k1-1)
		;
	    for (; k1 > i2 && imdata[k1] >= imdata[k1-1]; k1=k1-1)
		;
	   
	    i2 = min (npts, nint (center + high))
	    for (k2=i1; k2 < i2 && imdata[k2] <= imdata[k2+1]; k2=k2+1)
		;
	    for (; k2 < i2 && imdata[k2] >= imdata[k2+1]; k2=k2+1)
		;
	   
	    a = imdata[k1]
	    b = (imdata[k2] - imdata[k1]) / (k2 - k1)
	} else {
	    k1 = center
	    a = 0.
	    b = 0.
	}
	    
	i1 = center
	i2 = i1 + 1
	y1 = imdata[i1] - a - b * (i1 - k1)
	y2 = imdata[i2] - a - b * (i2 - k1)
	y = y1 * (i2 - center) + y2 * (center - i1)

	if (peak)
	    ycut = ylevel * y
	else
	    ycut = ylevel

	if (y > ycut) {
	    for (j1 = i1; j1 >= 1; j1 = j1 - 1) {
		y1 = imdata[j1] - a - b * (j1 - k1)
		if (y1 <= ycut)
		    break
	    }
	    if (j1 >= 1) {
	        j2 = j1 + 1
		y2 = imdata[j2] - a - b * (j2 - k1)
	        x = (ycut + y2 * j1 - y1 * j2) / (y2 - y1) - center
	        low = max (low, (1.+grow)*x)
	    }

	    for (j2 = i2; j2 <= npts; j2 = j2 + 1) {
		y2 = imdata[j2] - a - b * (j2 - k1)
		if (y2 <= ycut)
		    break
	    }
	    if (j2 <= npts) {
	        j1 = j2 - 1
		y1 = imdata[j1] - a - b * (j1 - k1)
	        x = (ycut + y2*j1 - y1*j2) / (y2 - y1) - center
	        high = min (high, (1.+grow)*x)
	    }
	} else {
	    for (j1 = i1; j1 >= 1; j1 = j1 - 1) {
		y1 = imdata[j1] - a - b * (j1 - k1)
		if (y1 >= ycut)
		    break
	    }
	    if (j1 >= 1) {
	        j2 = j1 + 1
		y2 = imdata[j2] - a - b * (j2 - k1)
	        x = (ycut + y2 * j1 - y1 * j2) / (y2 - y1) - center
	        low = max (low, (1.+grow)*x)
	    }

	    for (j2 = i2; j2 <= npts; j2 = j2 + 1) {
		y2 = imdata[j2] - a - b * (j2 - k1)
		if (y2 >= ycut)
		    break
	    }
	    if (j2 <= npts) {
	        j1 = j2 - 1
		y1 = imdata[j1] - a - b * (j1 - k1)
	        x = (ycut + y2*j1 - y1*j2) / (y2 - y1) - center
	        high = min (high, (1.+grow)*x)
	    }
	}
end
