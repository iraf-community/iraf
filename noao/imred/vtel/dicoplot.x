include <mach.h>
include <imhdr.h>
include <imset.h>
include <math/curfit.h>
include <gset.h>
include "dicoplot.h"
include "vt.h"

# DICOPLOT -- Make dicomed (or other graphics device) plots of Carrington
# rotation maps.  The output of this program is a metacode file called
# "metacode" which can be plotted on whichever graphics device the user
# chooses.  Before the program is run, STDGRAPH should be set to the target
# device.

procedure t_dicoplot()

char	image1[SZ_FNAME]			# first image to draw
char	image2[SZ_FNAME]			# second image to draw
int	rotnum					# carrington rotation number
char	device[SZ_FNAME]			# plot device

int	type1, type2				# types of the two images
pointer	imout1
pointer	imout2
int	count, obsdate
int	i, longitude, latitude, month, day, year
int	xresolution, yresolution
real	delta_gray, delta_long, delta_gblock, x, y
real	offset, longituder
real	mapx1, mapx2, mapy1, mapy2
char	ltext[SZ_LINE]
char	system_id[SZ_LINE]

bool	up, pastm
int	dateyn

short	gray[16]
pointer	imgray1
pointer	imgray2
pointer	gp, p, sp
pointer	im1, im2
pointer	subras1, subras2

pointer	imgs2r()
pointer	immap()
pointer	gopen()
int	imaccf()
int	ggeti()
real	imgetr()
int	clgeti(), imgeti()
errchk	gopen, immap, imgs2r, sysid

begin
	call smark (sp)
	call salloc (imout1, DIM_SQUAREIM*DIM_XCARMAP, TY_REAL)
	call salloc (imout2, DIM_SQUAREIM*DIM_XCARMAP, TY_REAL)
	call salloc (imgray1, DIM_SQUAREIM*DIM_XCARMAP, TY_SHORT)
	call salloc (imgray2, DIM_SQUAREIM*DIM_XCARMAP, TY_SHORT)

	# Get parameters from the cl.
	call clgstr ("image1", image1, SZ_FNAME)
	call clgstr ("image2", image2, SZ_FNAME)
	rotnum = clgeti ("rotnum")
	call clgstr ("device", device, SZ_FNAME)

	# Open the output file.
	gp = gopen (device, NEW_FILE, STDPLOT)

	# Open the images
	im1 = immap (image1, READ_ONLY, 0)
	im2 = immap (image2, READ_ONLY, 0)

	# Find out what kind of images we have.
	call gimtype (im1, type1)
	call gimtype (im2, type2)

	# Draw boxes around the grayscale and the data images.
	call box (gp, IM1BL_X, IM1BL_Y, IM1TR_X, IM1TR_Y)
	call box (gp, IM2BL_X, IM2BL_Y, IM2TR_X, IM2TR_Y)

	delta_gblock = (IMGTR_X - IMGBL_X)/16.
	y = IMGBL_Y - .005
	do i = 1, 16 {
	    x = IMGBL_X + real(i-1) * delta_gblock + delta_gblock/2.
	    call sprintf (ltext, SZ_LINE, "%d")
		call pargi ((i-1)*int((254./15.)+0.5))
	    call gtext (gp, x, y, ltext, "v=t;h=c;s=.20")
	}


	# Draw tic marks and labels on the image boxes.
	# First the longitudes.

	delta_long = (IM1TR_Y-IM1BL_Y)/36.
	longitude = 0
	do i = 1,37 {
	    call sprintf (ltext, SZ_LINE, "%d")
		call pargi (longitude)
	    y = IM1TR_Y - real(i-1)*delta_long
	    x = IM1TR_X
	    call gline (gp, x,y,x+TICLENGTH,y)
	    x = IM1BL_X
	    call gline (gp, x,y,x-TICLENGTH,y)
	    call gtext (gp, x-.005, y, ltext, "v=c;h=r;s=.25;u=0")
	    x = IM2TR_X
	    call gline (gp, x,y,x+TICLENGTH,y)
	    x = IM2BL_X
	    call gline (gp, x,y,x-TICLENGTH,y)
	    call gtext (gp, x-.005, y, ltext, "v=c;h=r;s=.25;u=0")
	    longitude = longitude + 10
	}

	# Now the latitudes.
	# First draw the tics and labels at 0 degrees on both images

	latitude = 0
	call sprintf (ltext, SZ_LINE, "%d")
	    call pargi (latitude)
	x = (IM1BL_X + IM1TR_X)/2.
	y = IM1TR_Y
	call gline (gp, x, y, x, y+TICLENGTH)
	call gtext (gp, x, y+.005, ltext, "v=b;h=c;s=.25;u=0")
	y = IM1BL_Y
	call gline (gp, x, y, x, y-TICLENGTH)
	x = (IM2BL_X + IM2TR_X)/2.
	y = IM2TR_Y
	call gline (gp, x, y, x, y+TICLENGTH)
	call gtext (gp, x, y+.005, ltext, "v=b;h=c;s=.25;u=0")
	y = IM2BL_Y
	call gline (gp, x, y, x, y-TICLENGTH)

	# Now the north latitudes.
	do i = 1,4 {
	    switch (i) {
	    case 1:
		latitude = 20
	    case 2:
		latitude = 40
	    case 3:
		latitude = 60
	    case 4:
		latitude = 90
	    }
	    offset = ((IM1TR_X - IM1BL_X)/2.) * sin(real(latitude)*3.1415/180.)
	    x = IM1BL_X + ((IM1TR_X - IM1BL_X)/2.) + offset
	    y = IM1TR_Y
	    call sprintf (ltext, SZ_LINE, "%s%d")
		call pargstr ("N")
	        call pargi (latitude)
	    call gline (gp, x, y, x, y+TICLENGTH)
	    call gtext (gp, x, y+.005, ltext, "v=b;h=c;s=.25;u=0")
	    y = IM1BL_Y
	    call gline (gp, x, y, x, y-TICLENGTH)
	    x = x + IM2BL_X - IM1BL_X
	    y = IM2TR_Y
	    call gline (gp, x, y, x, y+TICLENGTH)
	    call gtext (gp, x, y+.005, ltext, "v=b;h=c;s=.25;u=0")
	    y = IM2BL_Y
	    call gline (gp, x, y, x, y-TICLENGTH)
	}

	# Finally the south latitudes.
	do i = 1,4 {
	    switch (i) {
	    case 1:
		latitude = -20
	    case 2:
		latitude = -40
	    case 3:
		latitude = -60
	    case 4:
		latitude = -90
	    }
	    offset = ((IM2TR_X - IM2BL_X)/2.) * sin(real(latitude)*3.1415/180.)
	    x = IM1BL_X + ((IM1TR_X - IM1BL_X)/2.) + offset
	    y = IM1TR_Y
	    call sprintf (ltext, SZ_LINE, "%s%d")
		call pargstr ("S")
	        call pargi (-latitude)
	    call gline (gp, x, y, x, y+TICLENGTH)
	    call gtext (gp, x, y+.005, ltext, "v=b;h=c;s=.25;u=0")
	    y=IM1BL_Y
	    call gline (gp, x, y, x, y-TICLENGTH)
	    x = x + IM2BL_X - IM1BL_X
	    y = IM2TR_Y
	    call gline (gp, x, y, x, y+TICLENGTH)
	    call gtext (gp, x, y+.005, ltext, "v=b;h=c;s=.25;u=0")
	    y=IM2BL_Y
	    call gline (gp, x, y, x, y-TICLENGTH)
	}

	# Put the titles on.
	# We got the carrington rotation number from the cl.

	call sprintf (ltext, SZ_LINE, "CARRINGTON ROTATION %d %s")
	    call pargi (rotnum)
	switch (type1) {
	case T10830:
	    call pargstr ("10830")
	case TABSFLX:
	    call pargstr ("ABS. FLUX")
	case TWEIGHT:
	    call pargstr ("WEIGHT")
	case TFLUX:
	    call pargstr ("FLUX")
	case TPLRTY:
	    call pargstr ("POLARITY")
	}

	x = IM1TR_X+.025
	y = IM1BL_Y + (IM1TR_Y - IM1BL_Y) / 2.
	call gtext (gp, x, y, ltext, "v=c;h=c;s=.5;u=0")
	call sprintf (ltext, SZ_LINE, "CARRINGTON ROTATION %d %s")
	    call pargi (rotnum)
	switch (type2) {
	case T10830:
	    call pargstr ("10830")
	case TABSFLX:
	    call pargstr ("ABS. FLUX")
	case TWEIGHT:
	    call pargstr ("WEIGHT")
	case TFLUX:
	    call pargstr ("FLUX")
	case TPLRTY:
	    call pargstr ("POLARITY")
	}

	x = IM2TR_X+.025
	y = IM2BL_Y + (IM2TR_Y - IM2BL_Y) / 2.
	call gtext (gp, x, y, ltext, "v=c;h=c;s=.5;u=0")

	# Put on the dates at the appropriate longitudes.
	# Get the dates and longitudes from the image header.
	# Read dates until we run out.
	# This code alternates between long and short tics for the dates.
	# For this to work it is assumed that the dates are in
	# cronological order.

	# Get the first date and longitude from the image header to check
	# whether or not there are any dates.

	count = 1
	call sprintf (ltext, SZ_LINE, "DATE%04d")
	    call pargi (count)
	dateyn = imaccf (im1, ltext)
	if (dateyn == NO)
	    call error(0, "no dates in image header")
	obsdate = imgeti (im1, ltext)
	call sprintf (ltext, SZ_LINE, "LONG%04d")
	    call pargi (count)
	longituder = imgetr (im1, ltext)
	longitude = int(longituder + .5)

	# If we find some dates near the beginning of the list which have
	# longitudes smaller than 180, they probably are some "extra" grams
	# merged in to fill out the plot, don't plot these dates because they
	# are really off the image and will come out in the wrong place if we
	# allow them to be plotted.

	while (longitude < 180) {
	    count = count + 1
	    call sprintf (ltext, SZ_LINE, "DATE%04d")
	        call pargi (count)
	    dateyn = imaccf (im1, ltext)
	    if (dateyn == NO)
		break
	    obsdate = imgeti (im1, ltext)
	    call sprintf (ltext, SZ_LINE, "LONG%04d")
		call pargi (count)
	    longituder = imgetr (im1, ltext)
	    longitude = int(longituder + .5)
	}

	# Calculate the month/day/year.
	month = obsdate/10000
	day = obsdate/100 - 100 * (obsdate/10000)
	year = obsdate - 100 * (obsdate/100)

	up = FALSE
	pastm = FALSE

	while (dateyn == YES) {

	    # We check to see whether or not we have gotten past 180 degrees
	    # so that if we find some images near the end of the list with
	    # longitudes greater than 180 degrees we will know not to plot 
	    # them since they are off the image.  Longitudes of images in the
	    # image merge list decrease as we go down the list.

	    # Past the middle yet?
	    if (longitude < 180)
		pastm = true

	    # Figure out where this longitude is in y on the image.
	    y = real(IM1BL_Y) + ((360. - real(longitude))/360.) *
	        real(IM1TR_Y - IM1BL_Y)
	    x = real(IM1TR_X)

	    # Draw the tic and the label.
	    if (!up)
	        call gline (gp, x, y, x+.005, y)
	    else
	        call gline (gp, x, y, x+.011, y)
	    call sprintf(ltext, SZ_LINE, "%d/%d/%d")
	        call pargi(month)
	        call pargi(day)
	        call pargi(year)
	    if (!up)
	        call gtext (gp, x+.006, y, ltext, "v=c;h=l;s=.20;u=0")
	    else
	        call gtext (gp, x+.012, y, ltext, "v=c;h=l;s=.20;u=0")

	    # Do the other image.
	    x = real(IM2TR_X)
	    if (!up)
	        call gline (gp, x, y, x+.005, y)
	    else
	        call gline (gp, x, y, x+.011, y)
	    if (!up)
	        call gtext (gp, x+.006, y, ltext, "v=c;h=l;s=.20;u=0")
	    else
	        call gtext (gp, x+.012, y, ltext, "v=c;h=l;s=.20;u=0")

	    # Toggle up switch.
	    up = !up

	    count = count + 1
	    call sprintf (ltext, SZ_LINE, "DATE%04d")
	        call pargi (count)
	    dateyn = imaccf (im1, ltext)

	    if (dateyn == YES) {
		# Calculate the month/day/year.
	        obsdate = imgeti (im1, ltext)
		month = obsdate/10000
		day = obsdate/100 - 100 * (obsdate/10000)
		year = obsdate - 100 * (obsdate/100)

		# Read in the next longitude.
	        call sprintf (ltext, SZ_LINE, "LONG%04d")
	            call pargi (count)
	        longituder = imgeti (im1, ltext)
		longitude = int(longituder + .5)

		# If we are past the middle and find a longitude in the list
		# which is greater than 180 degrees, do not plot this date
		# since it is off the image and will be plotted in the wrong
		# place.

		if (pastm && longitude > 180)
		    dateyn = NO
	    }
	}	    # End of while loop on dates/longitudes.

	# Fill in the gray scale.
	delta_gray = 254./15.
	do i = 1, 16 {
	    gray[i] = 1.+real(i-1)*delta_gray+0.5
	}
	call gpcell (gp, gray, 16, 1, IMGBL_X, IMGBL_Y, IMGTR_X, IMGTR_Y)
	
	# Now map the input images from 360x180 to 180x360 and put them
	# out to the image. We also map the data values into the appropriate
	# gray scale.

	# Get subrasters of the images.
	subras1 = imgs2r (im1, 1, DIM_XCARMAP, 1, DIM_SQUAREIM)
	subras2 = imgs2r (im2, 1, DIM_XCARMAP, 1, DIM_SQUAREIM)
	
	# Call the image maping routine on both images.
	call remap (Memr[subras1], DIM_XCARMAP, DIM_SQUAREIM, Memr[imout1])
	call remap (Memr[subras2], DIM_XCARMAP, DIM_SQUAREIM, Memr[imout2])

	# Call the gray scale mapper.
	call graymap (Memr[imout1], DIM_SQUAREIM, DIM_XCARMAP, Mems[imgray1],
	    type1)
	call graymap (Memr[imout2], DIM_SQUAREIM, DIM_XCARMAP, Mems[imgray2],
	    type2)

	# Put the images out to the final image.
	xresolution = ggeti (gp, "xr")
	yresolution = ggeti (gp, "yr")
	mapx1 = IM1BL_X
	mapx2 = IM1TR_X
	mapy1 = IM1BL_Y
	mapy2 = IM1TR_Y
	call gpcell (gp, Mems[imgray1], DIM_SQUAREIM, DIM_XCARMAP, mapx1, mapy1,
	    mapx2, mapy2)
	mapx1 = IM2BL_X
	mapx2 = IM2TR_X
	mapy1 = IM2BL_Y
	mapy2 = IM2TR_Y
	call gpcell (gp, Mems[imgray2], DIM_SQUAREIM, DIM_XCARMAP, mapx1, mapy1,
	    mapx2, mapy2)

	# Put the system identification on the plot.
	call sysid (system_id, SZ_LINE)
	call gtext (gp, .51, .076, system_id, "h=c;s=0.45")

	# Close the graphics pointer.
	call gclose(gp)
	call close(p)

	call sfree (sp)
end


# BOX -- Draw a box around the square described by x1, y1 (bottom left corner)
# and x2, y2 (top right corner).

procedure box(gp, x1, y1, x2, y2)

real	x1, y1		# bottom left corner position
real	x2, y2		# top right corner position
pointer	gp		# graphics pointer

begin
	call gline (gp, x1, y1, x1, y2)
	call gline (gp, x1, y2, x2, y2)
	call gline (gp, x2, y2, x2, y1)
	call gline (gp, x2, y1, x1, y1)
end


# REMAP -- Reformat a 360x180 image into a 180x360 image by rotating the image
# by 90 degrees clockwise.

procedure remap (inim, x, y, outim)

real	inim[x,y]		# input image
real	outim[y,x]		# output image
int	x, y			# size of images

int	i, j	

begin
	do i = 1, x
	    do j = 1, y
		outim[j,x-i+1] = inim[i,j]
end


# GREYMAP -- Map an integer image into a short integer image using a specific
# scaling algorithm to make the full scale 1 to 256.

procedure graymap (inim, x, y, outim, type)

real	inim[x,y]		# input image
int	x, y			# size of images
int	type			# type of image
short	outim[x,y]		# output image

real	zpp[5], zcc[5], zp, zc	# parameters for different image types
int	i, j, index
short	ztbl[512]		# grayscale map array, (in gryscl.inc)

data	zpp /.25, .80, 0.2, 1.0, 100. /
data	zcc /384., 80., 0., 128., 128. /
include	"gryscl.inc"

begin
	# If the image is not a 10830 gram then just multiply each pixel
	# by a constant and then add another constant. (different constants
	# for flux, abs. flux, weight, and polarity)
	# If it is a 10830 gram then multiply and add as above, then use
	# the result as an index into a lookup table.  The table is enumerated
	# above.

	zp = zpp[type]
	zc = zcc[type]
	do i = 1, x {
	    do j = 1, y {
		outim[i,j] = inim[i,j] * zp + zc
		if (type == 1) {                      # if this is a 10830 gram:
		    if (outim[i,j] <= 0)	      # make it fit in the table
			outim[i,j] = 1
		    if (outim[i,j] > 512)
			outim[i,j] = 512
		    index = outim[i,j]
		    outim[i,j] = ztbl[index] + 10     # look it up in the table.
		}
		if (outim[i,j] <= 0)                  # check boundaries
		    outim[i,j] = 1
		if (outim[i,j] >= 255)
		    outim[i,j] = 254
	    }
	}
end


# GIMTYPE -- Get IMage TYPE.  Using information in the image header determine
# what type of image it is. 1 = 10830, 2 = ABS. FLUX, 3 = WEIGHTS,
# 4 = ABS. VALUE, 5 = POLARITY.

procedure gimtype (im, type)

pointer	im		# image pointer
int	type		# type

int	wavelength, imgeti()
int	weightyn, absyn, polarityyn
int	imaccf()

begin
	wavelength = imgeti (im, "WV_LNGTH")
	weightyn = imaccf (im, "WEIGHTS")
	absyn = imaccf (im, "ABS_VALU")
	polarityyn = imaccf (im, "POLARITY")

	if (weightyn == NO && absyn == NO && polarityyn == NO) {
	    if (wavelength == 10830)
		type = T10830
	    if (wavelength == 8688)
		type = TFLUX
	}
	if (weightyn == YES)
	    type = TWEIGHT
	if (absyn == YES)
	    type = TABSFLX
	if (polarityyn == YES)
	    type = TPLRTY
end
