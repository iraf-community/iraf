include <mach.h>
include	<imhdr.h>
include	"vt.h"
include "numeric.h"

define	LEN_HISTO	1025
define	SPACING		1

# RMAP -- Project a full disk solar image [2048x2048] into a square
# image [180x180] such that lines of latitude and longitude are
# perpendicular straight lines.

procedure t_rmap()

char	inputimage[SZ_FNAME]	# input image
char	outputimage[SZ_FNAME]	# output data image
char	outweight[SZ_FNAME]	# output weight image
char	outabs[SZ_FNAME]	# output absolute value image
char	histoname[SZ_FNAME]	# output histogram name

real	bzero			# latitude of sub-earth point
real	el[LEN_ELSTRUCT]	# ellipse parameters data structure
pointer	inputim, outputim, outw, outa, sp
pointer inim_subras_ptr, outim_subras_ptr
pointer	outwei_subras_ptr, outav_subras_ptr
int	outputrow, wvlngth
int	inim_subras_bottom
double	meanf, meanaf, zcm, muzero
int	numpix
bool	skip, helium
real	tempr

real	imgetr()
int	imgeti()
pointer	immap()
pointer imgs2s(), imps2s(), imps2i()
double	rmap_mode()
errchk	immap, imgs2s, imps2s, imps2i, checkimmem, rowmap

begin
	# Get parameters from the cl.

	# Image names.
	call clgstr ("inputimage", inputimage, SZ_FNAME)
	call clgstr ("outputimage", outputimage, SZ_FNAME)
	call clgstr ("outweight", outweight, SZ_FNAME)
	call clgstr ("outabs", outabs, SZ_FNAME)
	call clgstr ("histoname", histoname, SZ_FNAME)

	# Open images.
	inputim = immap (inputimage, READ_ONLY, 0)
	wvlngth = imgeti (inputim, "wv_lngth")
	helium = false
	if (wvlngth == 10830)
	    helium = true
	outputim = immap (outputimage, NEW_COPY, inputim)
	outw = immap (outweight, NEW_COPY, inputim)
	if (!helium)
	    outa = immap (outabs, NEW_COPY, inputim)

	# Compute mode estimate from the input image.
	muzero = rmap_mode (inputim, histoname, helium)

	# Define some parameters for output images.
	IM_LEN(outputim, 1) = DIM_SQUAREIM
	IM_LEN(outputim, 2) = DIM_SQUAREIM
	IM_PIXTYPE(outputim) = TY_INT

	IM_LEN(outw, 1) = DIM_SQUAREIM
	IM_LEN(outw, 2) = DIM_SQUAREIM

	if (!helium) {
	    IM_LEN(outa, 1) = DIM_SQUAREIM
	    IM_LEN(outa, 2) = DIM_SQUAREIM
	    IM_PIXTYPE(outa) = TY_INT
	}

	# Get latitude of sub-earth point from input image header.
	bzero = imgetr (inputim, "B_ZERO")

	# Ellipse parameters.
	E_XCENTER(el) = imgetr (inputim, "E_XCEN")
	E_YCENTER(el) = imgetr (inputim, "E_YCEN")
	E_XSEMIDIAMETER(el) = imgetr (inputim, "E_XSMD")
	E_YSEMIDIAMETER(el) = imgetr (inputim, "E_YSMD")

	# Remove the elipse parameters from the header records of the
	# output images

	call imdelf (outputim, "E_XCEN")
	call imdelf (outputim, "E_YCEN")
	call imdelf (outputim, "E_XSMD")
	call imdelf (outputim, "E_YSMD")

	call imdelf (outw, "E_XCEN")
	call imdelf (outw, "E_YCEN")
	call imdelf (outw, "E_XSMD")
	call imdelf (outw, "E_YSMD")
	call imaddb (outw, "WEIGHTS", YES)

	if (!helium) {
	    call imdelf (outa, "E_XCEN")
	    call imdelf (outa, "E_YCEN")
	    call imdelf (outa, "E_XSMD")
	    call imdelf (outa, "E_YSMD")
	    call imaddb (outa, "ABS_VALU", YES)
	}

	# Set the variable that keeps track of where in the input image the
	# bottom of the subraster is, map in the initial subraster.

	inim_subras_bottom = 1
	inim_subras_ptr = imgs2s (inputim, 1, DIM_VTFD, inim_subras_bottom,
	    inim_subras_bottom+DIM_IN_RAS-1)

	# Map the outputimages into memory.
	outim_subras_ptr = imps2i (outputim, 1, DIM_SQUAREIM, 1, DIM_SQUAREIM)
	outwei_subras_ptr = imps2s (outw, 1, DIM_SQUAREIM, 1, DIM_SQUAREIM)
	if (!helium)
	    outav_subras_ptr = imps2i (outa, 1, DIM_SQUAREIM, 1, DIM_SQUAREIM)
	else {
	    call smark (sp)
	    call salloc (outav_subras_ptr, DIM_SQUAREIM*DIM_SQUAREIM, TY_INT)
	}

	# Initialize meanf, meanaf, numpix.
	meanf = 0.0
	meanaf = 0.0
	numpix = 0

	# Map the input image into the output image by output image rows.
	do outputrow = 1, DIM_SQUAREIM {

	    # Check the current input subraster to see if it covers
	    # the next output row to be mapped and map in a new subraster
	    # if necessary.

	    call checkimmem (inim_subras_bottom, bzero, inputim, outputrow,
	        inim_subras_ptr, el, skip)

	    # If checkimmem returns skip = true then this row is not contained
	    # in the input image so fill it with zeros and skip it.

	    if (skip) {
		# Fill the empty row with zeros.
		call emptyrow (outputrow, Memi[outim_subras_ptr],
		    Mems[outwei_subras_ptr], Memi[outav_subras_ptr])
		next
	    }

	    # Map this pixel row.
	    call rowmap (inim_subras_bottom, Mems[inim_subras_ptr], bzero,
		outputrow, Memi[outim_subras_ptr], Mems[outwei_subras_ptr],
		Memi[outav_subras_ptr], el, muzero, meanf, meanaf, numpix,
		helium)
	}

	# Put the mean field, the number of pixels, the zero corrected mean
	# absolute field, the mode estimate, the zero corrected mean field,
	# and the standard deviation in the output image header.

	meanaf = meanaf/double(numpix)
	meanf = meanf/double(numpix)
	zcm = meanf - muzero
	tempr = real(meanf)
	call imaddr (outputim, "MEAN_FLD", tempr)
	call imaddi (outputim, "NUMPIX", numpix)
	if (!helium) {
	    tempr = real(meanaf)
	    call imaddr (outputim, "MEANAFLD", tempr)
	    tempr = real(muzero)
	    call imaddr (outputim, "MUZERO", tempr)
	    tempr = real(zcm)
	    call imaddr (outputim, "ZCM", tempr)
	}

	# Close images.
	call imunmap (inputim)
	call imunmap (outputim)
	call imunmap (outw)
	if (!helium)
	    call imunmap (outa)
	if (helium)
	    call sfree (sp)
end


# CHECKIMMEM -- Check this row to see if the input subraster in memory
# covers it and if it doesn't, map in a new subraster.

procedure checkimmem (inim_subras_bottom, bzero, inputim, outputrow,
	inim_subras_ptr, el, skip)

int	inim_subras_bottom	# current bottom of the loaded subraster
real	bzero			# latitude of sub-earth point for this image
pointer	inputim			# pointer to input image
int	outputrow		# which output row to map
pointer	inim_subras_ptr		# input image subraster pointer
real	el[LEN_ELSTRUCT]	# ellipse parameters data structure
bool	skip			# returned flag saying to skip this line

real	x ,y
int	ymax, ymin
real	uplat, downlat, lminusl0, latitude
pointer	imgs2s()
errchk	imgs2s

begin
	skip = false

	# Find values for the latitudes of the upper and lower edges of this
	# pixel row.

	uplat = 180./3.1415926*asin(float(outputrow - 90)/90.)
	downlat = 180./3.1415926*asin(float(outputrow - 91)/90.)

	# Check to see if this row is either completely off the image or
	# partially off the image.  If it is off the image then return
	# skip = true.  If it is partially off the image then truncate
	# the appropriate boundary latitude at the image boundary.

	if (bzero > 0) {
	    if ( downlat < (-90 + bzero) && uplat < (-90 + bzero)) {

		# This row is not on the image.
		skip = true
		return
	    }
	    if (downlat < (-90 + bzero))
		downlat = -90 + bzero
	} else {
	    if ( downlat > (90 - bzero) && uplat > (90 - bzero)) {

		# This row is not on the image.
		skip = true
		return
	    }
	    if (uplat > (90 - bzero))
		uplat = 90 - bzero
	}

	# Calculate the minimum and maximum values of y in the input image that
	# we will need to map this output row of pixels and check these
	# values against the value of the current bottom of the subraster.

	if (bzero > 0) {

	    # Calculate y position in image.
	    lminusl0 = 90.
	    latitude = uplat
	    call getxy (latitude, lminusl0, bzero, el, x, y, skip)
	    ymax = int(y + .5)
	    lminusl0 = -90.
	    latitude = uplat
	    call getxy (latitude, lminusl0, bzero, el, x, y, skip)
	    if (int(y + .5) > ymax)
		ymax = int(y + .5)

	    # Calculate min y position.
	    lminusl0 = 0.
	    latitude = downlat
	    call getxy (latitude, lminusl0, bzero, el, x, y, skip)
	    ymin = int(y + .5)

	} else {

	    # Calculate y position in image.
	    lminusl0 = 90.
	    latitude = downlat
	    call getxy (latitude, lminusl0, bzero, el, x, y, skip)
	    ymin = int(y + .5)
	    lminusl0 = -90.
	    latitude = downlat
	    call getxy (latitude, lminusl0, bzero, el, x, y, skip)
	    if (int(y + .5) < ymin) ymin = int(y + .5)

	    # Calculate max y position.
	    lminusl0 = 0.
	    latitude = uplat
	    call getxy (latitude, lminusl0, bzero, el, x, y, skip)
	    ymax = int(y + .5)
	}

	# If ymin or ymax is outside the current subraster, then map in
	# an appropriate subraster.

	if ((ymin < (inim_subras_bottom + 5)) ||
	    (ymax > (inim_subras_bottom + 140))) {
	    if ((ymax - ymin) > 150) {
		call printf ("Subraster too small(ymax-ymin > 150), bye")
	    }
	    if ((ymin +  144) > 2048) {
		ymin = 2048 - 144
	    }
	    if ((ymin - 5) < 1) {
		skip = true
		return
	    }
	    inim_subras_ptr = imgs2s (inputim, 1, DIM_VTFD, (ymin - 5),
		(ymin + 144))
	    inim_subras_bottom = ymin - 5
	}
end


# ROWMAP -- Map this output row pixel by pixel.

procedure rowmap (inim_subras_bottom, in_subraster, bzero, outputrow,
	out_subraster, outw_subraster, outa_subraster, el, muzero, meanf,
	meanaf, numpix helium)

real	bzero						# lat of sub-earth
real	el[LEN_ELSTRUCT]		# ellipse parameters data structure
int	inim_subras_bottom				# bottom of current
int	outputrow					# output row
short	in_subraster[DIM_VTFD, DIM_IN_RAS]		# subraster
int	out_subraster[DIM_SQUAREIM, DIM_SQUAREIM]	# output image
short	outw_subraster[DIM_SQUAREIM, DIM_SQUAREIM]	# output weights
int	outa_subraster[DIM_SQUAREIM, DIM_SQUAREIM]	# output abs. value
double	muzero						# mode estimate
double	meanf						# mean field
double	meanaf						# mean absolute field
int	numpix						# number of pixels
bool	helium						# 10830 flag

int	pixel

errchk	pixelmap

begin
	# Do all 180 pixels in this output row.
	do pixel = 1,180 {
	    call pixelmap (pixel, in_subraster, inim_subras_bottom,
		bzero, outputrow, out_subraster, outw_subraster, outa_subraster,
		el, muzero, meanf, meanaf, numpix, helium)
	}
end


# PIXELMAP -- Sum up and count the input pixels contained inside the
# given output pixel.  The sum is carried out in the following way:
#
# Calculate, on the input image, the position of the center of the
#    output pixel to be mapped.
# Calculate the values of the partial derivitives of latitude and
#    longitude with respect to x and y.
# Calculate the boundaries of the pixel in the input image and
#    sum and count all the pixels inside, assign the value
#    to the output pixel = sum/count.

procedure pixelmap (pixel, in_subraster, inim_subras_bottom,
	bzero, outputrow, out_subraster, outw_subraster, outa_subraster,
	el, muzero, meanf, meanaf, numpix, helium)

int	pixel						# which pixel
short	in_subraster[DIM_VTFD, DIM_IN_RAS]		# subraster
int	inim_subras_bottom				# bottom of current
real	bzero						# lat of sub-earth
int	outputrow					# output row
int	out_subraster[DIM_SQUAREIM, DIM_SQUAREIM]	# output image
short	outw_subraster[DIM_SQUAREIM, DIM_SQUAREIM]	# output weights
int	outa_subraster[DIM_SQUAREIM, DIM_SQUAREIM]	# output abs. value
real	el[LEN_ELSTRUCT]		# ellipse parameters data structure
double	muzero						# mode estimate
double	meanf						# first moment accum.
double	meanaf						# mean absolute field
int	numpix						# number of pixels
bool	helium						# helium flag

real	lat_mid, long_mid, lat_bot, lat_top
real	long_rite, long_left
double	sum, sumabs
int	count
real	xpixcenter, ypixcenter
real	dlongdx, dlatdy
int	xleft,xright,ybottom,ytop,x,y
int	num_pix_vert, num_pix_horz
pointer	sp
pointer	num					# numeric structure pointer
real	dat

begin
	call smark (sp)
	call salloc (num, VT_LENNUMSTRUCT, TY_STRUCT)

	# First obtain the parameters necessary from numeric.
	call numeric (bzero, el, outputrow, pixel, xpixcenter, ypixcenter, num)

	dlongdx = VT_DLODX(num)
	dlatdy = VT_DLATDY(num)
	lat_top = VT_LATTOP(num)
	lat_bot = VT_LATBOT(num)
	long_left = VT_LOLEFT(num)
	long_rite = VT_LORITE(num)
	lat_mid = VT_LATMID(num)
	long_mid = VT_LOMID(num)

	if (lat_top == 10000.) {
	    out_subraster[pixel,outputrow] = 0
	    outw_subraster[pixel,outputrow] = 0
	    outa_subraster[pixel,outputrow] = 0
	    call sfree (sp)
	    return
	}

	# Calculate the box of pixels we want.
	num_pix_horz = int((1.0 / dlongdx) + .5)
	xleft = xpixcenter - int((.5 / dlongdx) + .5)
	xright = xleft + num_pix_horz - 1
	num_pix_vert = int((abs(abs(lat_top) - abs(lat_bot)) / dlatdy) + .5)
	ybottom = ypixcenter - int(((abs(abs(lat_mid) - abs(lat_bot))) /
	    dlatdy) + .5) - (inim_subras_bottom - 1)
	ytop = ybottom + num_pix_vert - 1
	
	# Sum up the pixels inside this box.
	count = 0
	sum = 0.0
	sumabs = 0.0

	do x = xleft, xright {
	    do y = ybottom, ytop {
		if (and(int(in_subraster[x,y]),17B) >= THRESHOLD+1) {
		    count = count + 1

		    # Divide by 16 to remove squibby brightness
		    # Accumulate the various moment data.
		    dat = real(in_subraster[x,y]/16)
		    sum = sum + double(dat)
		    sumabs = sumabs + double(abs(dat - muzero))
		}
	    }
	}

	outw_subraster[pixel,outputrow] = short(count)
	out_subraster[pixel,outputrow] = int(sum - double(count*muzero) + .5)
	if (!helium)
	    outa_subraster[pixel,outputrow] = int(sumabs + .5)
	meanf = meanf + sum
	meanaf = meanaf + sumabs
	numpix = numpix + count

	call sfree (sp)
end


# EMPTYROW -- Set this row in the output image to zero.

procedure emptyrow (outputrow, out_subraster, outw_subraster, outa_subraster)

int	outputrow
int	out_subraster[DIM_SQUAREIM, DIM_SQUAREIM]
short	outw_subraster[DIM_SQUAREIM, DIM_SQUAREIM]
int	outa_subraster[DIM_SQUAREIM, DIM_SQUAREIM]

int	pixel

begin
	# Do all 180 pixels in this output row.
	do pixel = 1,180 {
	    out_subraster[pixel, outputrow] = 0
	    outw_subraster[pixel, outputrow] = 0
	    outa_subraster[pixel, outputrow] = 0
	}
end


double procedure rmap_mode (inputim, histoname, helium)

pointer	inputim			# Input image
char	histoname[SZ_FNAME]	# Histogram name
bool	helium

int	count, i, j
int	dati, hist_middle
pointer	imline, histim, hiptr
int	histo[LEN_HISTO]

# Stuff for mrqmin.
real    a[3], x[LEN_HISTO], y[LEN_HISTO], sig[LEN_HISTO]
int     lista[3]
real    alambda, chisq, covar[3,3], alpha[3,3]
short	k

pointer	imgl2s(), impl1i(), immap()
short	shifts()

extern	gauss

begin
	# Initialize.
	count = 0
	k = -4
	do i = 1, LEN_HISTO
	    histo[i] = 0

	do i = 1, DIM_VTFD, SPACING{
	    imline = imgl2s (inputim, i)
	    do j = 1, DIM_VTFD, SPACING {
		if (and(int(Mems[imline+j-1]),17B) >= THRESHOLD+1) {
		    count = count + 1
		    dati = shifts(Mems[imline+j-1], k)

		    # Put the data into a histogram.
		    hist_middle = (LEN_HISTO-1)/2 + 1
		    if (abs(dati) <= hist_middle-1)
			histo[dati+hist_middle] = histo[dati+hist_middle] + 1
		}
	    }
	}

	# Write this histogram out to an image.
	histim = immap (histoname, NEW_COPY, inputim)
	IM_NDIM(histim) = 1
	IM_LEN(histim, 1) = LEN_HISTO
	IM_PIXTYPE(histim) = TY_INT
	hiptr = impl1i (histim)

	# Put the histogram into this image.
	do i = 1, LEN_HISTO
	    Memi[hiptr+i-1] = histo[i]

	if (!helium) {
	    # Set up arrays, etc. for gaussian fit.
	    a[2] = 1.0
	    a[1] = real(histo[1])
	    do i = 1, LEN_HISTO {
	        x[i] = real(i)
	        y[i] = real(histo[i])
	        sig[i] = 1.0
	        if (histo[i] > a[1]) {
		    a[1] = real(histo[i])
		    a[2] = real(i)
	        }
	    }
	    a[3] = 15.0

	    do i = 1, 3
	        lista[i] = i

	    # Fit the gaussian.
	    alambda = -1.0
	    call mrqmin (x, y, sig, LEN_HISTO, a, 3, lista, 3, covar, alpha, 3,
	        chisq, gauss, alambda)
	    do i = 1, 10 {
	        call mrqmin (x, y, sig, LEN_HISTO, a, 3, lista, 3, covar,
		    alpha, 3, chisq, gauss, alambda)
	    }

	    call imaddr (histim, "GSS_AMPL", a[1])
	    call imaddr (histim, "GSS_CNTR", a[2])
	    call imaddr (histim, "GSS_WDTH", a[3])

	    # Put the mode estimate in the header.
	    call imaddr (histim, "MUZERO", (a[2] - real(hist_middle)))
	}

	call imunmap (histim)

	if (helium)
	    return (0.0)
	else
	    return (double(a[2] - real(hist_middle)))
end
