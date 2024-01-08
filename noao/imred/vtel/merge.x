include <mach.h>
include	<imhdr.h>
include	"vt.h"

# MERGE -- Put together all appropriate daily grams to produce a full
# carrington rotation map.  This is done both for the average input images
# and for the absolute value input images.  The output of the program is
# 4 images, average image, absolute value image, weight image, ratio of
# first image to second image.

procedure t_merge()

char	mergelist[SZ_FNAME]		# list of images to be merged

int	wavelength, listfd
char	inputimage[SZ_FNAME]
pointer	inputim

pointer	immap()
int	imgeti(), open(), fscan()
errchk	immap, open

begin
	# Get the image name file from the cl and open it.
	call clgstr ("mergelist", mergelist, SZ_FNAME)
	listfd = open (mergelist, READ_ONLY, TEXT_FILE)

	# Get the wavelength from the first image in the mergelist.
	if (fscan (listfd) != EOF) {
	    call gargwrd (inputimage, SZ_FNAME)
	    inputim = immap (inputimage, READ_ONLY, 0)
	    wavelength = imgeti (inputim, "WV_LNGTH")
	    call close (listfd)
	} else {
	    call error (0, "No images in 'mergelist'")
	    call close (listfd)
	    return
	}

	if (wavelength == 8688)
	    call mergem (mergelist, wavelength)
	else
	    call mergeh (mergelist, wavelength)
end


# MERGEM -- MERGE Magnetograms.

procedure mergem (mergelist, wavelength)

char	mergelist[SZ_FNAME]		# list of images to be merged
int	wavelength			# wavelength of images

pointer	outputim, outw, outa, outr
pointer outptr, outwptr, outaptr, outrptr
char	outputimage[SZ_FNAME], outweight[SZ_FNAME]
char	outabs[SZ_FNAME], outratio[SZ_FNAME]
real	longout, weight_tbl[SZ_WTBL], bzeroave
int	i, mapmonth, mapday, mapyear

real	clgetr()
int	clgeti()
pointer	immap(), imps2r()
errchk	immap, imps2r

begin
	# Get parameters from the cl.

	# Output images.
	call clgstr ("outputimage", outputimage, SZ_FNAME)
	call clgstr ("outweight", outweight, SZ_FNAME)
	call clgstr ("outabs", outabs, SZ_FNAME)
	call clgstr ("outratio", outratio, SZ_FNAME)

	# Longitude of center of output Carrington rotation map.
	longout = clgetr ("longout")

	# Month, day, and year of the center of the output map.
	mapmonth = clgeti ("mapmonth")
	mapday = clgeti ("mapday")
	mapyear = clgeti ("mapyear")

	# Open output image.
	outputim = immap (outputimage, NEW_IMAGE, 0)

	# Define some parameters for the output images.
	IM_NDIM(outputim) = 2
	IM_LEN(outputim, 1) = DIM_XCARMAP
	IM_LEN(outputim, 2) = DIM_SQUAREIM
	IM_PIXTYPE(outputim) = TY_REAL

	# Open the rest of the output images.
	outw = immap (outweight, NEW_COPY, outputim)
	outa = immap (outabs, NEW_COPY, outputim)
	outr = immap (outratio, NEW_COPY, outputim)

	# Map the outputimages into memory.
	outptr = imps2r (outputim, 1, DIM_XCARMAP, 1, DIM_SQUAREIM)
	outwptr = imps2r (outw, 1, DIM_XCARMAP, 1, DIM_SQUAREIM)
	outaptr = imps2r (outa, 1, DIM_XCARMAP, 1, DIM_SQUAREIM)
	outrptr = imps2r (outr, 1, DIM_XCARMAP, 1, DIM_SQUAREIM)

	# Create weight table.
	do i = 1,SZ_WTBL
	    weight_tbl[i] = (cos((real(i-91)+.5)*3.1415926/180.))**4

	call mmall (mergelist, Memr[outptr], Memr[outwptr], Memr[outaptr],
	    outputim, outw, outa, outr, wavelength, weight_tbl, longout,
	    mapmonth, mapday, mapyear, bzeroave)

	# Fill the ratio image.
	call imratio (Memr[outptr],Memr[outaptr],Memr[outrptr],DIM_XCARMAP,
	    DIM_SQUAREIM)

	# Write some information out to the image headers.
	call imaddr (outputim, "AV_BZERO", bzeroave)
	call imaddi (outputim, "WV_LNGTH", wavelength)
	call imaddr (outw, "AV_BZERO", bzeroave)
	call imaddr (outw, "WV_LNGTH", wavelength)
	call imaddb (outw, "WEIGHTS", TRUE)
	call imaddr (outa, "AV_BZERO", bzeroave)
	call imaddr (outr, "AV_BZERO", bzeroave)
	call imaddr (outa, "WV_LNGTH", wavelength)
	call imaddr (outr, "WV_LNGTH", wavelength)
	call imaddb (outa, "ABS_VALU", TRUE)
	call imaddb (outr, "POLARITY", TRUE)
	
	# Weight the data image and the abs image.
	call imratio (Memr[outptr],Memr[outwptr],Memr[outptr],DIM_XCARMAP,
	    DIM_SQUAREIM)
	call imratio (Memr[outaptr],Memr[outwptr],Memr[outaptr],DIM_XCARMAP,
	    DIM_SQUAREIM)

	# Close images
	call imunmap (outputim)
	call imunmap (outw)
	call imunmap (outa)
	call imunmap (outr)
end


# MERGEH -- MERGE Helium 10830 grams.

procedure mergeh (mergelist, wavelength)

char	mergelist[SZ_FNAME]		# list of images to merge
int	wavelength			# wavelength of observation

pointer	outputim, outw
pointer outptr, outwptr
char	outputimage[SZ_FNAME], outweight[SZ_FNAME]
real	longout, weight_tbl[SZ_WTBL], bzeroave
int	i, mapmonth, mapday, mapyear

real	clgetr()
int	clgeti()
pointer	immap(), imps2r()
errchk	immap, imps2r

begin
	# Get parameters from the cl.

	# Output images.
	call clgstr ("outputimage", outputimage, SZ_FNAME)
	call clgstr ("outweight", outweight, SZ_FNAME)

	# Longitude of center of output Carrington rotation map.
	longout = clgetr ("longout")

	# Month, day, and year of the center of the output map.
	mapmonth = clgeti ("mapmonth")
	mapday = clgeti ("mapday")
	mapyear = clgeti ("mapyear")

	# Open output image.
	outputim = immap (outputimage, NEW_IMAGE, 0)

	# Define some parameters for the output images.
	IM_NDIM(outputim) = 2
	IM_LEN(outputim, 1) = DIM_XCARMAP
	IM_LEN(outputim, 2) = DIM_SQUAREIM
	IM_PIXTYPE(outputim) = TY_REAL

	# Open the other output image.
	outw = immap (outweight, NEW_COPY, outputim)

	# Map the outputimages into memory.
	outptr = imps2r (outputim, 1, DIM_XCARMAP, 1, DIM_SQUAREIM)
	outwptr = imps2r (outw, 1, DIM_XCARMAP, 1, DIM_SQUAREIM)

	# Create weight table.
	do i = 1,SZ_WTBL
	    weight_tbl[i] = (cos((real(i-91)+.5)*3.1415926/180.))**4

	call mhall (mergelist, Memr[outptr], Memr[outwptr], outputim,
	    outw, wavelength, weight_tbl, longout, mapmonth,
	    mapday, mapyear, bzeroave)

	# Write some information out to the image headers.
	call imaddr (outputim, "AV_BZERO", bzeroave)
	call imaddi (outputim, "WV_LNGTH", wavelength)
	call imaddr (outw, "AV_BZERO", bzeroave)
	call imaddr (outw, "WV_LNGTH", wavelength)
	call imaddb (outw, "WEIGHTS", TRUE)
	
	# Weight the data image.
	call imratio (Memr[outptr],Memr[outwptr],Memr[outptr],DIM_XCARMAP,
	    DIM_SQUAREIM)

	# Close images.
	call imunmap (outputim)
	call imunmap (outw)
end


# MMALL -- Merge Magnetograms ALL.
# Map in each input image, weight it, figure out where it goes
# and add it to the output image.

procedure mmall (mergelist, outarray, outarrayw, outarraya, outputim,
	outw, outa, outr, wavelength, weight_tbl, longout, mapmonth, mapday,
	mapyear, bzeroave)

char	mergelist[SZ_FNAME]			# list of images to be merged
int	wavelength				# wavelength of observations
real	outarray[DIM_XCARMAP, DIM_SQUAREIM]	# output data array
real	outarrayw[DIM_XCARMAP, DIM_SQUAREIM]	# output weights array
real	outarraya[DIM_XCARMAP, DIM_SQUAREIM]	# output absolute value array
pointer	inputim					# pointer to input image
pointer	outputim				# pointer to output image
pointer	outw					# pointer to weight image
pointer	outa					# pointer to abs value image
pointer	outr					# pointer to ratio image
int	mapmonth, mapday, mapyear		# date of output map
real	weight_tbl[SZ_WTBL]			# weight table
real	longout					# longitude of map center
real	bzeroave				# average b-zero for map

char	inputimage[SZ_FNAME], inweight[SZ_FNAME], inabs[SZ_FNAME]
pointer	inw, ina, inptr, inwptr, inaptr
int	listfd, month, day, year, count
real	longin, bzero, bzerosum
int	obsdate, temp, i, j
char	ltext[SZ_LINE]

int	open(), fscan(), imgeti()
real	imgetr()
pointer	immap(), imgs2i(), imgs2s()
errchk	open, immap, imgs2i, imgs2s

begin
	count = 0
	bzerosum = 0.0
	listfd = open (mergelist, READ_ONLY, TEXT_FILE)

	# Zero the output images.
	do i = 1, DIM_XCARMAP {
	    do j = 1, DIM_SQUAREIM {
		outarray[i,j] = 0.0
		outarrayw[i,j] = 0.0
		outarraya[i,j] = 0.0
	    }
	}

	# Get inputimages from the mergelist until they are all used up.
	while (fscan (listfd) != EOF) {
	    call gargwrd (inputimage, SZ_FNAME)

	    # Get absolute value image.
	    if(fscan (listfd) != EOF)
	        call gargwrd (inabs, SZ_FNAME)
	    else
	        call error (0, "wrong number of file names in mergelist")

	    # Get weight image.
	    if(fscan (listfd) != EOF)
	        call gargwrd (inweight, SZ_FNAME)
	    else
	        call error (0, "wrong number of file names in mergelist")

	    # Open input image, its corresponding weight map, and its
	    # corresponding absolute value map.

	    inputim = immap (inputimage, READ_ONLY, 0)
	    inw = immap (inweight, READ_ONLY, 0)
	    ina = immap (inabs, READ_ONLY, 0)

	    bzero = imgetr (inputim, "B_ZERO")
	    bzerosum = bzerosum + bzero
	    longin = imgetr (inputim, "L_ZERO")
	    obsdate = imgeti (inputim, "OBS_DATE")

	    # Check to see that the date is same on the three input images.
	    temp = imgeti (inw, "OBS_DATE")
 	    if (temp != obsdate) {
		call eprintf ("ERROR: date on weight image differs from that ")
		call eprintf ("on data image!\n")
		break
	    }

	    temp = imgeti (ina, "OBS_DATE")
	    if (temp != obsdate) {
		call eprintf ("ERROR: date on abs image differs from that ")
	        call eprintf ("on data image!\n")
	        break
	    }

	    # Decode month, day, year.
	    month = obsdate/10000
	    day = obsdate/100 - 100 * (obsdate/10000)
	    year = obsdate - 100 * (obsdate/100)

	    # Pack a name for this date and longitude and then put them out
	    # into the outputimages' headers.

	    count = count + 1
	    call sprintf (ltext, SZ_LINE, "DATE%04d")
		call pargi (count)
	    call imaddi (outputim, ltext, obsdate)
	    call imaddi (outw, ltext, obsdate)
	    call imaddi (outa, ltext, obsdate)
	    call imaddi (outr, ltext, obsdate)

	    call sprintf (ltext, SZ_LINE, "LONG%04d")
		call pargi (count)
	    call imaddr (outputim, ltext, longin)
	    call imaddr (outw, ltext, longin)
	    call imaddr (outa, ltext, longin)
	    call imaddr (outr, ltext, longin)
	    
	    # Map the inputimage, the weight map, and abs_image into memory.
	    inptr = imgs2i (inputim, 1, DIM_SQUAREIM, 1, DIM_SQUAREIM)
	    inwptr = imgs2s (inw, 1, DIM_SQUAREIM, 1, DIM_SQUAREIM)
	    inaptr = imgs2i (ina, 1, DIM_SQUAREIM, 1, DIM_SQUAREIM)

	    # Weight this image and add it to the output image.
	    call addmweight (Memi[inptr],Mems[inwptr],Memi[inaptr],outarray,
	        outarrayw, outarraya, weight_tbl, longin, longout,
		month, day, year, mapmonth, mapday, mapyear)

	    # Close this input image.
	    call imunmap (inputim)
	    call imunmap (inw)
	    call imunmap (ina)

	} # end of do loop on input images

	bzeroave = bzerosum/real(count)
	call close (listfd)
end


# MHALL -- Merge Heliumgrams ALL.
# Map in each input image, weight it, figure out where it goes
# and add it to the output image.

procedure mhall (mergelist, outarray, outarrayw, outputim,
	outw, wavelength, weight_tbl, longout, mapmonth, mapday,
	mapyear, bzeroave)

char	mergelist[SZ_FNAME]			# list of images to be merged
int	wavelength				# wavelength of observations
real	outarray[DIM_XCARMAP, DIM_SQUAREIM]	# output data array
real	outarrayw[DIM_XCARMAP, DIM_SQUAREIM]	# output weights array
pointer	inputim					# pointer to input image
pointer	outputim				# pointer to output image
pointer	outw					# pointer to weight image
int	mapmonth, mapday, mapyear		# date of output map
real	weight_tbl[SZ_WTBL]			# weight table
real	longout					# longitude of map center
real	bzeroave				# average b-zero for map

char	inputimage[SZ_FNAME], inweight[SZ_FNAME]
pointer	inw, inptr, inwptr
int	listfd, month, day, year, count
real	longin, bzero, bzerosum
int	obsdate, temp, i, j
char	ltext[SZ_LINE]

real	imgetr()
int	open(), fscan(), imgeti()
pointer	immap(), imgs2i(), imgs2s()
errchk	open, immap, imgs2i, imgs2s

begin
	count = 0
	bzerosum = 0.0
	listfd = open (mergelist, READ_ONLY, TEXT_FILE)

	# Zero the output images.
	do i = 1, DIM_XCARMAP {
	    do j = 1, DIM_SQUAREIM {
		outarray[i,j] = 0.0
		outarrayw[i,j] = 0.0
	    }
	}

	# Get inputimages from the mergelist until they are all used up.
	while (fscan (listfd) != EOF) {
	    call gargwrd (inputimage, SZ_FNAME)

	    # Get weight image.
	    if (fscan (listfd) != EOF)
	        call gargwrd (inweight, SZ_FNAME)
	    else
	        call error (0, "wrong number of file names in mergelist")

	    # Open input image, its corresponding weight map, and its
	    # corresponding absolute value map.

	    inputim = immap (inputimage, READ_ONLY, 0)
	    inw = immap (inweight, READ_ONLY, 0)

	    bzero = imgetr (inputim, "B_ZERO")
	    bzerosum = bzerosum + bzero
	    longin = imgetr (inputim, "L_ZERO")
	    obsdate = imgeti (inputim, "OBS_DATE")

	    # Check to see that the date is same on the three input images.
	    temp = imgeti (inw, "OBS_DATE")
 	    if (temp != obsdate) {
		call eprintf ("ERROR: date on weight image differs from that ")
		call eprintf ("on data image!\n")
		break
	    }

	    # Decode month, day, year.
	    month = obsdate/10000
	    day = obsdate/100 - 100 * (obsdate/10000)
	    year = obsdate - 100 * (obsdate/100)

	    # Pack a name for this date and longitude and then put them out
	    # into the outputimages' headers.

	    count = count + 1
	    call sprintf (ltext, SZ_LINE, "DATE%04d")
		call pargi (count)
	    call imaddi (outputim, ltext, obsdate)
	    call imaddi (outw, ltext, obsdate)

	    call sprintf (ltext, SZ_LINE, "LONG%04d")
		call pargi (count)
	    call imaddr (outputim, ltext, longin)
	    call imaddr (outw, ltext, longin)
	    
	    # Map the inputimage, the weight map, and abs_image into memory.
	    inptr = imgs2i (inputim, 1, DIM_SQUAREIM, 1, DIM_SQUAREIM)
	    inwptr = imgs2s (inw, 1, DIM_SQUAREIM, 1, DIM_SQUAREIM)

	    # Weight this image and add it to the output image.
	    call addhweight (Memi[inptr], Mems[inwptr], outarray, outarrayw,
		weight_tbl, longin, longout, month, day, year, mapmonth,
		mapday, mapyear)

	    # Close this input image.
	    call imunmap (inputim)
	    call imunmap (inw)

	} # end of do loop on input images

	bzeroave = bzerosum/real(count)
	call close (listfd)
end


# ADDMWEIGHT -- Weight input image by cos(longitude - (L-L0))**4, and add
# it to the output image in the proper place.

procedure addmweight (inim, inwim, inaim, outim, outwim, outaim,
	weight_tbl, longin, longout, month, day, year, mapmonth, mapday,
	mapyear)

int	inim[DIM_SQUAREIM, DIM_SQUAREIM]	# input image
short	inwim[DIM_SQUAREIM, DIM_SQUAREIM]	# input image weights
int	inaim[DIM_SQUAREIM, DIM_SQUAREIM]	# input absolute image
real	outim[DIM_XCARMAP, DIM_SQUAREIM]	# outputimage
real	outwim[DIM_XCARMAP, DIM_SQUAREIM]	# output image weights
real	outaim[DIM_XCARMAP, DIM_SQUAREIM]	# output absolute image
int	month, day, year			# date of input image
int	mapmonth, mapday, mapyear		# date of output image
real	weight_tbl[DIM_SQUAREIM]		# weight table
real	longin, longout				# longitudes of images

int	p1offset, p2offset, firstpix, lastpix, column, row
int	offset, datein, dateout, temp, temp2
int	d1900()

begin
	# Translate the two dates into julian day numbers to make comparisons
	# simpler.

	datein = d1900 (month, day, year)
	dateout = d1900 (mapmonth, mapday, mapyear)

	# Figure out the pixel offset between the first pixel of the input
	# image and the first pixel of ther output image.
	# Actually, there may be two pixel offsets for a particular image
	# corresponding to the correct position of the image and the 360
	# degree offset position.

	p1offset = mod(abs(int(longin - longout + .5)), 360)    # This is one.
	p2offset = 360 - p1offset                         # This is the other.

	# Determine which side of the output image center is each of these
	# offsets.

	if (datein > dateout) {
	    if (longout > 180) {
	        if (((longin >= longout) && (longin <= 360))  || 
		    (longin <= mod((longout + 180.),360.))) {
		    if (p1offset < 180)
		        offset = p2offset
		    else
		        offset = p1offset
	        } else {
		    if (p1offset >= 180)
		        offset = p2offset
		    else
		        offset = p1offset
		}
	    } else {
	        if ((longin >= longout) && (longin <= (longout + 180))) {
		    if (p1offset <= 180)
		        offset = p2offset
		    else
		        offset = p1offset
	        } else {
		    if (p1offset >= 180)
		        offset = p2offset
		    else
		        offset = p1offset
		}
	    }
        } else {
	    if (longout < 180) {
	        if (((longin >= (180 + longout)) && (longin <= 360))  || 
		    (longin <= longout)) {
		    if (p1offset < 180)
		        offset = p2offset
		    else
		        offset = p1offset
	        } else {
		    if (p1offset >= 180)
		        offset = p2offset
		    else
		        offset = p1offset
		}
	    } else {
	        if ((longin < longout) && (longin > (longout - 180))) {
		    if (p1offset < 180)
		        offset = p2offset
		    else
		        offset = p1offset
	        } else {
		    if (p1offset >= 180)
		        offset = p2offset
		    else
		        offset = p1offset
		}
	    }
	}

        # Make sure the sign is right
        if (datein > dateout)
	    offset = -offset

        # Check for the case that the two longitudes are equal.
        if (longin == longout) {
	    if (abs(datein - dateout) <= 1) {
	        offset = 0
	    } else {
	        call eprintf ("input day too far from center of output map\n")
	        return
	    }
        }
	    
        # Check for the case that the two dates are equal.
        if (datein == dateout)
	    offset = longin - longout

        # If the offset is too large then do not use this image.
        if (abs(offset) > 240) {
	    call eprintf ("input day too far from center of output map\n")
	    return
        }

        # Determine what part, if not all, of the input image will lie on the
        # output image.

        firstpix = 1
        if (offset < -90)
	    firstpix = abs(offset+90)
        lastpix = DIM_SQUAREIM
        if (offset > 90) 
	    lastpix = 180 - (offset - 90)


        # Do all 180 columns in the image.
	if (offset <= 0)
	    temp = 91
	else
	    temp = 90

	do column = firstpix,lastpix {
	    do row = 1, DIM_SQUAREIM {
	        temp2 = column + temp + offset
	        outim[temp2,row] = outim[temp2, row] +
		    inim[column, row] * weight_tbl[column]
	        outwim[temp2,row] = outwim[temp2, row] +
		    inwim[column, row] * weight_tbl[column]
	        outaim[temp2,row] = outaim[temp2, row] +
		    inaim[column, row] * weight_tbl[column]
	    }
	}
end


# ADDHWEIGHT -- Weight input image by cos(longitude - (L-L0))**4, and add
# it to the output image in the proper place. (For 10830 grams)

procedure addhweight (inim, inwim, outim, outwim, weight_tbl, longin, longout,
	month, day, year, mapmonth, mapday, mapyear)

int	inim[DIM_SQUAREIM, DIM_SQUAREIM]	# input image
short	inwim[DIM_SQUAREIM, DIM_SQUAREIM]	# input image weights
real	outim[DIM_XCARMAP, DIM_SQUAREIM]	# outputimage
real	outwim[DIM_XCARMAP, DIM_SQUAREIM]	# output image weights
int	month, day, year			# date of input image
int	mapmonth, mapday, mapyear		# date of output image
real	weight_tbl[DIM_SQUAREIM]		# weight table
real	longin, longout				# longitudes of images

int	p1offset, p2offset, firstpix, lastpix, column, row
int	offset, datein, dateout, temp, temp2
int	d1900()

begin
	# Translate the two dates into julian day numbers to make comparisons
	# simpler.

	datein = d1900 (month, day, year)
	dateout = d1900 (mapmonth, mapday, mapyear)

	# Figure out the pixel offset between the first pixel of the input
	# image and the first pixel of ther output image.
	# Actually, there may be two pixel offsets for a particular image
	# corresponding to the correct position of the image and the 360
	# degree offset position.

	p1offset = mod(abs(int(longin - longout + .5)), 360)    # this is one.
	p2offset = 360 - p1offset                         # this is the other.

	# Determine which side of the output image center is each of these
	# offsets.

	if (datein > dateout) {
	    if (longout > 180) {
	        if (((longin >= longout) && (longin <= 360))  || 
		    (longin <= mod((longout + 180.),360.))) {
		    if (p1offset < 180)
		        offset = p2offset
		    else
		        offset = p1offset
	        } else {
		    if (p1offset >= 180)
		        offset = p2offset
		    else
		        offset = p1offset
		}
	    } else {
	        if ((longin >= longout) && (longin <= (longout + 180))) {
		    if (p1offset <= 180)
		        offset = p2offset
		    else
		        offset = p1offset
	        } else {
		    if (p1offset >= 180)
		        offset = p2offset
		    else
		        offset = p1offset
		}
	    }
        } else {
	    if (longout < 180) {
	        if (((longin >= (180 + longout)) && (longin <= 360))  || 
		    (longin <= longout)) {
		    if (p1offset < 180)
		        offset = p2offset
		    else
		        offset = p1offset
	        } else {
		    if (p1offset >= 180)
		        offset = p2offset
		    else
		        offset = p1offset
		}
	    } else {
	        if ((longin < longout) && (longin > (longout - 180))) {
		    if (p1offset < 180)
		        offset = p2offset
		    else
		        offset = p1offset
	        } else {
		    if (p1offset >= 180)
		        offset = p2offset
		    else
		        offset = p1offset
		}
	    }
	}

        # Make sure the sign is right.
        if (datein > dateout)
	    offset = -offset

        # Check for the case that the two longitudes are equal.
        if (longin == longout) {
	    if (abs(datein - dateout) <= 1) {
	        offset = 0
	    } else {
	        call eprintf ("Input day too far from center of output map.\n")
	        return
	    }
        }
	    
        # Check for the case that the two dates are equal.
        if (datein == dateout)
	    offset = longin - longout

        # If the offset is too large then do not use this image.
        if (abs(offset) > 240) {
	    call eprintf ("input day too far from center of output map\n")
	    return
        }

        # Determine what part, if not all, of the input image will lie on the
        # output image.

        firstpix = 1
        if (offset < -90)
	    firstpix = abs(offset+90)
        lastpix = DIM_SQUAREIM
        if (offset > 90) 
	    lastpix = 180 - (offset - 90)


        # Do all 180 columns in the image.
	if (offset <= 0)
	    temp = 91
	else
	    temp = 90

	do column = firstpix, lastpix {
	    do row = 1, DIM_SQUAREIM {
	        temp2 = column + temp + offset
	        outim[temp2,row] = outim[temp2, row] +
		    inim[column, row] * weight_tbl[column]
	        outwim[temp2,row] = outwim[temp2, row] +
		    inwim[column, row] * weight_tbl[column]
	    }
	}
end
