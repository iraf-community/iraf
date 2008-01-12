#  FHGLIN -- Draw a histogram style curve (bar graph) through the
#  points. 

## 7/21/93  Fixed a bug causing the last bar to disappear.  ZGL


procedure fhglin (gd, xdata, ydata, npts, fillpat)

pointer	gd		# Graphics descriptor                                                                                                       
real	xdata[ARB]	# X coordinates of the line endpoints
real	ydata[ARB]	# Y coordinates of the line endpoints
int	npts		# Number of data values
int	fillpat		# "Bar" fill pattern

int	bin, bin1, bin2
real	xbin[5], ybin[5]		# Fill polyline
real	dx
real	left, right, bottom, top	# Data window

int	nxtbin()

begin
	call ggwind (gd, left, right, bottom, top)

	bin1 = nxtbin (1, xdata, ydata, npts)
	if (bin1 >= npts)
		call error (0, "No valid data\n")
	bin2 = nxtbin (bin1+1, xdata, ydata, npts)
	if (bin2 >= npts)
		call error (0, "No valid data\n")

	dx = (xdata[bin2] - xdata[bin1]) / 2.0
	xbin[1] = xdata[bin1] - dx
	ybin[1] = bottom
	xbin[2] = xbin[1]
	ybin[2] = ydata[bin1]
	xbin[3] = (xdata[bin1] + xdata[bin2]) / 2.0
	ybin[3] = ybin[2]
	xbin[4] = xbin[3]
	ybin[4] = bottom
	xbin[5] = xbin[1]
	ybin[5] = ybin[1]
	call gfill (gd, xbin, ybin, 5, fillpat)

	bin = bin2

	while (bin < npts) {

	    bin2 = nxtbin (bin+1, xdata, ydata, npts)

	    xbin[1] = (xdata[bin1] + xdata[bin]) / 2.0
	    ybin[1] = bottom
	    xbin[2] = xbin[1]
	    ybin[2] = ydata[bin]
	    xbin[3] = (xdata[bin] + xdata[bin2]) / 2.0
	    ybin[3] = ybin[2]
	    xbin[4] = xbin[3]
	    ybin[4] = bottom
	    xbin[5] = xbin[1]
	    ybin[5] = ybin[1]
	    call gfill (gd, xbin, ybin, 5, fillpat)

	    bin1 = bin
	    bin  = bin2
	}

	if (!IS_INDEF(xdata[npts]) && !IS_INDEF(ydata[npts])) {
		dx = (xdata[npts] - xdata[bin1]) / 2.0
		xbin[1] = (xdata[bin1] + xdata[npts]) / 2.0
		ybin[1] = bottom
		xbin[2] = xbin[1]
		ybin[2] = ydata[npts]
		xbin[3] = xdata[npts] + dx
		ybin[3] = ybin[2]
		xbin[4] = xbin[3]
		ybin[4] = bottom
		xbin[5] = xbin[1]
		ybin[5] = ybin[1]
		call gfill (gd, xbin, ybin, 5, fillpat)
	}
end


int procedure nxtbin (start, x, y, npts)

int	start
real	x[ARB], y[ARB]
int	npts

int	bin

begin
	for (bin = start;  
		bin <= npts && (IS_INDEF(x[bin]) || IS_INDEF(y[bin]));  
		bin = bin + 1)
		;
	return (bin)
end
