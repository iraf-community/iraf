include <mach.h>
include <imhdr.h>
include <imset.h>
include <gset.h>
include "syndico.h"
include "vt.h"

# SYNDICO -- Make Dicomed prints of synoptic images.  This program is tuned
# to make the images 18 centimeters in diameter.

procedure t_syndico()

char	image[SZ_FNAME]				# image to plot
char	logofile[SZ_FNAME]			# file containing logo
char	device[SZ_FNAME]			# plot device
int	sbthresh				# squibby brightness threshold
bool	verbose					# verbose flag
bool	plotlogo				# plotlogo flag
bool	forcetype				# force image type flag
bool	magnetic				# image type = magnetic flag

int	obsdate, wavelength, obstime
int	i, j, month, day, year, hour, minute, second, stat, bufptr
real	delta_gblock, x, y
real	excen, eycen, exsmd, eysmd, rguess
real	b0, l0
real	mapy1, mapy2, radius, scale, diskfrac
char	ltext[SZ_LINE]
char	system_id[SZ_LINE]

short	grey[16]
pointer	gp, sp, im, lf
pointer	subrasp, subras1, buff
int	trnsfrm[513]
int	lkup10830[1091]
int	gs10830[16]
real	xstart, xend, ystart, yend, yinc
real	xcenerr, ycenerr, ndc_xcerr, ndc_ycerr
real	temp_xcenter, temp_ycenter

pointer	immap(), gopen(), imgl2s()
int	imgeti(), clgeti(), open(), read()
real	imgetr()
bool	clgetb(), imaccf()
include	"trnsfrm.inc"
errchk	gopen, immap, sysid, imgs2s, imgl2s

# Grey scale points for 10830.
data (gs10830[i], i = 1, 6) /-1000,-700,-500,-400,-300,-250/
data (gs10830[i], i = 7, 10) /-200,-150,-100,-50/
data (gs10830[i], i = 11, 16) /0,10,20,40,60,90/

begin
	call smark (sp)
	call salloc (subrasp, DIM_VTFD, TY_SHORT)
	call salloc (subras1, 185*185, TY_SHORT)
	call salloc (buff, 185, TY_CHAR)

	# Get parameters from the cl.
	call clgstr ("image", image, SZ_FNAME)
	call clgstr ("logofile", logofile, SZ_FNAME)
	call clgstr ("device", device, SZ_FNAME)
	sbthresh = clgeti ("sbthresh")
	plotlogo = clgetb ("plotlogo")
	verbose = clgetb ("verbose")
	forcetype = clgetb ("forcetype")
	magnetic = clgetb ("magnetic")

	# Open the input image, open the logo image if requested.
	im = immap (image, READ_ONLY, 0)
	if (plotlogo)
	    iferr {
	        lf = open (logofile, READ_ONLY, TEXT_FILE)
	    } then {
		call eprintf ("Error opening the logo file, logo not made.\n")
		plotlogo = false
	    }

	# Get/calculate some of the housekeeping data.
	if (imaccf (im, "obs_date")) {
	    obsdate = imgeti (im, "obs_date")
	    obstime = imgeti (im, "obs_time")
	    month = obsdate/10000
	    day = obsdate/100 - 100 * (obsdate/10000)
	    year = obsdate - 100 * (obsdate/100)
	    hour = int(obstime/3600)
	    minute = int((obstime - hour * 3600)/60)
	    second = obstime - hour * 3600 - minute * 60
	} else {
	    # Use cl query parameters to get these values.
	    call eprintf ("Date and Time not found in image header.\n")
	    call eprintf ("Please enter them below.\n")
	    month = clgeti ("month")
	    day = clgeti ("day")
	    year = clgeti ("year")
	    hour = clgeti ("hour")
	    minute = clgeti ("minute")
	    second = clgeti ("second")
	}

	# Get the solar image center and radius from the image header,
	# get the solar image radius from the ephemeris routine.  If 
	# the two radii are similar, use the former one, if they are
	# %10 percent or more different, use the ephemeris radius and
	# assume the center is at (1024,1024).

	# Get ellipse parameters from image header.
	# If they are not there, warn the user that we are using ephemeris
	# values.
	if (imaccf (im, "E_XCEN")) {
	    excen =  imgetr (im, "E_XCEN")
	    eycen =  imgetr (im, "E_YCEN")
	    exsmd =  imgetr (im, "E_XSMD")
	    eysmd =  imgetr (im, "E_YSMD")

	    # Get rguess from ephem.
	    iferr (call ephem (month, day, year, hour, minute, second, rguess,
	        b0, l0, false))
	        call eprintf ("Error getting ephemeris data.\n")

	    radius = (exsmd + eysmd) / 2.0
	    if (abs(abs(radius-rguess)/rguess - 1.0) > 0.1) {
	        radius = rguess
	        excen = 1024.0
	        eycen = 1024.0
	    }

	} else {
	    call eprintf ("No ellipse parameters in image header.\n Using")
	    call eprintf (" ephemeris value for radius and setting center to")
	    call eprintf (" 1024, 1024\n")

	    # Get rguess from ephem.
	    iferr (call ephem (month, day, year, hour, minute, second, rguess,
	        b0, l0, false))
	        call eprintf ("Error getting ephemeris data.\n")

	    radius = rguess
	    excen = 1024.0
	    eycen = 1024.0
	}

	# Error in center. (units of pixels)
	xcenerr = excen - 1024.0
	ycenerr = eycen - 1024.0

	# Transform error to NDC.
	ndc_xcerr = xcenerr * (1.0/4096.0)
	ndc_ycerr = ycenerr * (1.0/4096.0)

	# Next, knowing that the image diameter must be 18 centimeters,
	# calculate the scaling factor we must use to expand the image.
	# DICO_18CM is a MAGIC number = 18 centimeters on dicomed prints
	# given the way the NOAO photo lab currently enlarges the images.
	scale = DICO_18CM / real(radius*2)

	# Open the output file.
	gp = gopen (device, NEW_FILE, STDGRAPH)

	# Put feducial(sp?) marks on plot.
	diskfrac = radius/1024.0
	temp_xcenter = DICO_XCENTER-ndc_xcerr
	temp_ycenter = DICO_YCENTER-ndc_ycerr
	call gline (gp, temp_xcenter, temp_ycenter+diskfrac*.25*scale+.01,
	    temp_xcenter, temp_ycenter+diskfrac*.25*scale+.025)
	call gline (gp, temp_xcenter, temp_ycenter-diskfrac*.25*scale-.01,
	    temp_xcenter, temp_ycenter-diskfrac*.25*scale-.025)
	
	# Draw a little compass on the plot.
	call gline (gp, .25, DICO_YCENTER+.25+.01,
	    .25, DICO_YCENTER+.25+.035)
	call gtext (gp, .25, DICO_YCENTER+.25+.037,
	    "N", "v=b;h=c;s=.50")
	call gmark (gp, .2565, DICO_YCENTER+.25+.037,
	    GM_CIRCLE, .006, .006)
	call gmark (gp, .2565, DICO_YCENTER+.25+.037,
	    GM_CIRCLE, .001, .001)
	call gline (gp, .25, DICO_YCENTER+.25+.01,
	    .28, DICO_YCENTER+.25+.01)
	call gtext (gp, .282, DICO_YCENTER+.25+.01,
	    "W", "v=c;h=l;s=.50")
	call gmark (gp, .290, DICO_YCENTER+.25+.01-.006,
	    GM_CIRCLE, .006, .006)
	call gmark (gp, .290, DICO_YCENTER+.25+.01-.006,
	    GM_CIRCLE, .001, .001)

	# Get the wavelength from the image header.  If the user wants
	# to force the wavelength, do so.  (this is used if the header
	# information about wavelength is wrong.)
	wavelength = imgeti (im, "wv_lngth")
	if (forcetype)
	    if (magnetic)
		wavelength = 8688
	    else
		wavelength = 10830

	# Write the grey scale labels onto the plot.
	delta_gblock = (IMGTR_X - IMGBL_X)/16.
	y = IMGBL_Y - .005
	do i = 1, 16 {
	    x = IMGBL_X + real(i-1) * delta_gblock + delta_gblock/2.
	    call sprintf (ltext, SZ_LINE, "%d")
	    if (wavelength == 8688)
		call pargi ((i-1)*(int((512./15.)+0.5))-256)
	    else if (wavelength == 10830)
		call pargi (gs10830(i))
	    call gtext (gp, x, y, ltext, "v=t;h=c;s=.20")
	}

	# Label on grey scale.
	call sprintf (ltext, SZ_LINE, "%s")
	if (wavelength == 8688)
	    call pargstr ("gauss")
	else if (wavelength == 10830)
	    call pargstr ("relative line strength")
        call gtext (gp, DICO_XCENTER, (IMGBL_Y-.024), ltext, "v=c;h=c;s=.5")

	# Put the title on.
	call sprintf (ltext, SZ_LINE, "%s")
	if (wavelength == 8688)
	    call pargstr ("8688 MAGNETOGRAM")
	else if (wavelength == 10830)
	    call pargstr ("10830 SPECTROHELIOGRAM")
	else
	    call pargstr (" ")
	call gtext (gp, DICO_XCENTER, .135, ltext, "v=c;h=c;s=.7")

	# If we don't have a logo to plot, write the data origin on the plot.
	if (!plotlogo) {

	    call sprintf (ltext, SZ_LINE, "%s")
	        call pargstr ("National")
	    call gtext (gp, .24, .155, ltext, "v=c;h=c;s=.7")
	    call sprintf (ltext, SZ_LINE, "%s")
	        call pargstr ("Solar")
	    call gtext (gp, .24, .135, ltext, "v=c;h=c;s=.7")
	    call sprintf (ltext, SZ_LINE, "%s")
	        call pargstr ("Observatory")
	    call gtext (gp, .24, .115, ltext, "v=c;h=c;s=.7")
	}

	# Put month/day/year on plot.
	call sprintf (ltext, SZ_LINE, "%02d/%02d/%02d")
	    call pargi (month)
	    call pargi (day)
	    call pargi (year)
	call gtext (gp, .70, .175, ltext, "v=c;h=l;s=.5")

	# Put the hour:minute:second on plot.
	call sprintf (ltext, SZ_LINE, "%02d:%02d:%02d UT")
	    call pargi (hour)
	    call pargi (minute)
	    call pargi (second)
	call gtext (gp, .70, .155, ltext, "v=c;h=l;s=.5")

	# Fill in the grey scale.
	if (wavelength == 8688) {
	    do i = 1, 16
	        grey[i] = (trnsfrm[(i-1)*(int((512./15.)+0.5))+1])
	    call gpcell (gp, grey, 16, 1, IMGBL_X, IMGBL_Y, IMGTR_X, IMGTR_Y)
	} else if (wavelength == 10830) {
	    do i = 1, 16
	        grey[i] = (lkup10830[gs10830(i)+1001])
	    call gpcell (gp, grey, 16, 1, IMGBL_X, IMGBL_Y, IMGTR_X, IMGTR_Y)
	}

	# Prepare some constants for plotting.
	xstart = temp_xcenter - .25 * scale
	xend = temp_xcenter + .25 * scale
	ystart = temp_ycenter - .25 * scale
	yend = temp_ycenter + .5 * scale
	mapy1 = ystart
	mapy2 = ystart
	yinc = (.5*scale)/real(DIM_VTFD)
	
	# Put the data on the plot. Line by line.
	do i = 1, DIM_VTFD {

	    if (verbose) {
		call printf ("line = %d\n")
		    call pargi (i)
		call flush (STDOUT)
	    }
	
	    subrasp = imgl2s (im, i)

	    # Call the limb trimmer and data divider.
	    call fixline (Mems[subrasp], DIM_VTFD, wavelength, sbthresh)

	    # Update the top and bottom edges of this line.
	    mapy1 = mapy2
	    mapy2 = mapy2 + yinc

	    # Put the line on the output plot.
	    call gpcell (gp, Mems[subrasp], DIM_VTFD, 1, xstart,
		mapy1, xend, mapy2)

	} # End of do loop on image lines.

	# Put the system identification on the plot.
	call sysid (system_id, SZ_LINE)
	call gtext (gp, DICO_XCENTER, .076, system_id, "h=c;s=0.45")

	# Put the NSO logo on the plot.
	if (plotlogo) {

	    # Read in the image. (the image is encoded in a text file)
	    do i = 1, 185 {
	        bufptr = 0
	        while (bufptr < 185-79) {
	            stat = read (lf, Memc[buff+bufptr], 80)
		    bufptr = bufptr + 79
	        }
	        stat = read (lf, Memc[buff+bufptr], 80)
	        do j = 1, 185 {
		    Mems[subras1+(i-1)*185+j-1] =
			short((Memc[buff+j-1]-32.)*2.7027027)
		}
	    }

	    # Put it on the plot.
	    call gpcell (gp, Mems[subras1], 185, 185, .24, .13, .32, .21)
	}

	# Close the graphics pointer, unmap images, free stack.
	call gclose (gp)
	call imunmap (im)
	if (plotlogo)
	    call close (lf)
	call sfree (sp)
end


# FIXLINE -- Clean up the line.  Set the value of pixels off the limb to
# zero, remove the squibby brightness from each pixel, and apply a
# nonlinear lookup table to the greyscale mapping.

procedure fixline (ln, xlength, wavelength, sbthresh)

int	xlength			# length of line buffer
short	ln[xlength]		# line buffer
int	wavelength		# wavelength of the observation
int	sbthresh		# squibby brightness threshold

int	trnsfrm[513]
int	lkup10830[1091]
bool	found
int	i, left, right
include	"trnsfrm.inc"

begin
	# Look in from the left end till squibby brightness goes above the
	# threshold, remember where this limbpoint is.
	found = false
	do i = 1, xlength {		# Find left limbpoint.
	    if (and(int(ln[i]),17B) > sbthresh) {
		found = true
		left = i
		break
	    }
	}

	if (found) {
	    # Find the right limbpoint.
	    do i = xlength, 1, -1 {
	        if (and(int(ln[i]),17B) > sbthresh) {
		    right = i
		    break
	        }
	    }

	    # Divide the image by 16, map the greyscale, and trim the limb.
	    do i = left+1, right-1 {

		# Remove squibby brightness.
		ln[i] = ln[i]/16

		if (wavelength == 8688) {
		    # Magnetogram, nonlinear greyscale.
		    # Make data fit in the table.
		    if (ln[i] < -256)
		        ln[i] = -256
		    if (ln[i] > 256)
		        ln[i] = 256

		    # Look it up in the table.
		    ln[i] = trnsfrm[ln[i]+257]
		} else if (wavelength == 10830) {
		    # 10830 spectroheliogram, nonlinear greyscale.
		    # Make data fit in the table.
		    if (ln[i] < -1000)
		        ln[i] = -1000
		    if (ln[i] > 90)
		        ln[i] = 90
		    # Look it up in the table.
		    ln[i] = lkup10830[ln[i]+1001]
		} else {
		    # Unknown type, linear greyscale.
		    if (ln[i] < 1)
		        ln[i] = 1
		    if (ln[i] > 255)
		        ln[i] = 255
		}
	    }

	    # Set stuff outside the limb to zero.
	    do i = 1, left
		ln[i] = 0
	    do i = right, xlength
		ln[i] = 0
	} else {
	    # This line is off the limb, set it to zero.
	    do i = 1, xlength
		ln[i] = 0
        }
end
