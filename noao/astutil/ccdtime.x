# CCDTIME -- Compute the time required to reach a given signal-to-noise
# ratio for CCD in UBVRI.

procedure t_ccdtime()

real	mag, time[5], adu, seeing, scale, aper, fratio, dark
real	nread, pixsiz, size, rpix, xtime, tol
real	nsky[5], nstar[5], snr, ndark, noise, prec
real	skyval[5,2]
real	starv [5,2]
int	npix, ichip, i, isum
real	clgetr()
int	clgeti()
char	chip[4], color[1,5]
bool	streq()
#
# sky values per square arcsec for RCA and TI dewars
#
data	skyval /.05, .26, .42, .73, .7,
                .05, .32, .70, 1.51, 2.16/
#
# star count rates for mag=10 star at 0.9m
#
data	starv  /3300, 14000, 13000, 13400, 6400,
	        3800, 18000, 22000, 28000, 20000/
#
# iteration tolerance in seconds of time
#
data	tol    /.01/

begin
	mag    = clgetr ("magnitude")
	prec   = clgetr ("precision")
	seeing = clgetr ("seeing")
	aper   = clgetr ("aperture")
	fratio = clgetr ("fratio")
	dark   = clgetr ("dark")
	isum   = clgeti ("isum")
	call     clgstr ("chip", chip, 4)

	call strcpy ("U", color[1,1], 1)
	call strcpy ("B", color[1,2], 1)
	call strcpy ("V", color[1,3], 1)
	call strcpy ("R", color[1,4], 1)
	call strcpy ("I", color[1,5], 1)

	if (streq(chip,"RCA1")) {
	    adu = 11
	    size= .030 * isum
	    nread = 75
	    ichip = 1
	} else if (streq(chip,"RCA2")) {
	    adu = 14
	    size= .030 * isum
	    nread = 75
	    ichip = 1
	} else if (streq(chip,"TI"))   {
	    adu = 4.5
	    size= .015 * isum
	    nread = 8.6
	    ichip = 2
	} else
	    call error (1, "Unknown detector")

	scale = 206265/(aper*fratio*1000)
	pixsiz = scale * size
	rpix  =  (2 * seeing / pixsiz)**2
	npix  =  int(rpix) + 1

	
	snr  = 1.0/prec

	# Compute exposure time thru each filter.

	for (i=1; i <= 5; i=i+1) {
	    time[i]  = 10.0
	    xtime = 0.0

	    # Use resubstitution to iterate for exposure time.

	    while (abs(time[i]-xtime) > tol) {
	        time[i] = xtime
	        nstar[i] = starv[i,ichip] * (aper/0.9)**2 * adu *
		    10**(0.4*(10.0-mag))

	        nsky[i]  = skyval[i,ichip] * pixsiz**2 * time[i] * adu *
                    (aper/0.9)**2
	        ndark = dark * time[i]
	        noise = sqrt(nstar[i]*time[i] + npix*(nsky[i] + ndark +
                             nread**2))
	        xtime = snr * noise / nstar[i]
	    }
	    nstar[i] = nstar[i] * time[i]
	}

	call printf ("\nTele: %3.1fm f/%3.1f     Seeing:%3.1f arcsec\n")
	    call pargr   (aper)
	    call pargr   (fratio)
	    call pargr   (seeing)
	call printf ("Magnitude: %5.2f     Precision: %4.3f\n")
	    call pargr   (mag)
	    call pargr   (prec)
	call printf ("Pixels in aperture: %4d \n\n")
	    call pargi   (npix)

	call printf ("Color Star ph      Sky ph      Time(sec)\n\n")
	
	for (i=1; i <= 5; i=i+1) {
	    call printf ("  %s  %8f    %8f      %8.2f\n")
		call pargstr (color[1,i])
		call pargr   (nstar[i])
		call pargr   (nsky [i])
		call pargr   (time [i])
	}
end
