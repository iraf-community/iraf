include "vt.h"

# PIMTEXT -- Put a text string directly into an image using a pixel font
# and writing over the image pixels.

procedure t_pimtext()

char	im[SZ_FNAME]			# image to put text in
char	refim[SZ_FNAME]			# reference image (get date/time)
int	x, y				# position to put text
int	xmag, ymag			# text magnification parameters
int	val				# value to use for text pixels
int	bgndval				# value to use for background pixels
bool	setbgnd				# flag, should we set the background?
bool	ref				# flag, are we using a ref image

int	obstime, obsdate, hour, minute, second
int	list, nfiles
int	month, day, year
char	dt[DTSTRING]
bool	istime, isdate, date, time
pointer	imp, rimp

bool	clgetb(), imaccf()
int	clgeti(), imgeti()
int	clpopni(), clplen(), clgfil()
pointer	immap()
errchk	immap

begin
	# Get file name template from the CL.
	list = clpopni ("iraf_files")
	nfiles = clplen (list)

	# Get some other parameters.
	ref = clgetb ("ref")
	if (ref)
	    call clgstr ("refim", refim, SZ_FNAME)
	x = clgeti ("x")
	y = clgeti ("y")
	xmag = clgeti ("xmag")
	ymag = clgeti ("ymag")
	val = clgeti ("val")
	setbgnd = clgetb ("setbgnd")
	bgndval = clgeti ("bgndval")
	date = clgetb ("date")
	time = clgetb ("time")

	while (clgfil (list, im, SZ_FNAME) != EOF) {
	    # Open the image(s).
	    imp = immap (im, READ_WRITE, 0)
	    if (ref)
	        rimp = immap (refim, READ_ONLY, 0)

	    if (date || time) {
	        # Find out if the date and time exist in the image header.
		if (ref) {
	            istime = imaccf (rimp, "obs_time")
	            isdate = imaccf (rimp, "obs_date")
		} else {
	            istime = imaccf (imp, "obs_time")
	            isdate = imaccf (imp, "obs_date")
		}

		# Get the date and/or time.
		if (date && isdate && !time) {
		    if (ref)
			obsdate = imgeti (rimp, "obs_date")
		    else
	                obsdate = imgeti (imp, "obs_date")

	            month = obsdate / 10000
	            day = obsdate/100 - 100 * (obsdate/10000)
	            year = obsdate - 100 * (obsdate/100)
	            call sprintf (dt, DTSTRING, "%02d/%02d/%02d")
	                call pargi (month)
	                call pargi (day)
	                call pargi (year)

		} else if (time && istime && !date) {
		    if (ref)
			obstime = imgeti (rimp, "obs_time")
		    else
	                obstime = imgeti (imp, "obs_time")

	            hour = int(obstime/3600)
	            minute = int((obstime - hour * 3600)/60)
	            second = obstime - hour * 3600 - minute * 60
	            call sprintf (dt, DTSTRING, "%02d:%02d:%02d")
	                call pargi (hour)
	                call pargi (minute)
	                call pargi (second)

		} else if (istime && isdate && time && date) {
		    if (ref) {
	                obstime = imgeti (rimp, "obs_time")
	                obsdate = imgeti (rimp, "obs_date")
		    } else {
	                obstime = imgeti (imp, "obs_time")
	                obsdate = imgeti (imp, "obs_date")
		    }

	            month = obsdate/10000
	            day = obsdate/100 - 100 * (obsdate/10000)
	            year = obsdate - 100 * (obsdate/100)
	            hour = int(obstime/3600)
	            minute = int((obstime - hour * 3600)/60)
	            second = obstime - hour * 3600 - minute * 60
	            call sprintf (dt, DTSTRING, "%02d:%02d:%02d %02d/%02d/%02d")
	                call pargi (hour)
	                call pargi (minute)
	                call pargi (second)
	                call pargi (month)
	                call pargi (day)
	                call pargi (year)
	        } else {
	            call printf ("Warning: cannot get date and/or time.\n")
	            call printf ("Getting text string fron the CL.\n")
	            call clgstr ("text", dt, DTSTRING)
	        }
	    } else
	        call clgstr ("text", dt, DTSTRING)

	    call textim (imp, dt, x, y, xmag, ymag, val, setbgnd, bgndval)
	    call imunmap (imp)
	    if (ref)
		call imunmap (rimp)
	} # end while

	call clpcls (list)
end
