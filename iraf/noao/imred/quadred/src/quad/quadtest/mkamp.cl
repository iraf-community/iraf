# MKAMP -- Make a CCD observation

procedure mkamp (image, exptime, ccdtype)

string	image			{prompt="Image name"}
real	exptime			{prompt="Exposure time"}
string	ccdtype			{prompt="CCD type"}

int	ncols=132		{prompt="Number of columns"}
int	nlines=100		{prompt="Number of lines"}
string	filter=""		{prompt="Filter"}
string	datasec=""		{prompt="Data section"}
string	trimsec=""		{prompt="Trim section"}
string	biassec=""		{prompt="Bias section"}

file	imdata=""		{prompt="Image data"}
real	skyrate=0.		{prompt="Sky count rate"}
real	zeroval=0.		{prompt="Zero level value"}
real	zeroslope=0.		{prompt="Slope of zero level"}
real	flashval=0.		{prompt="Preflash value"}
real	flashslope=0.		{prompt="Slope of preflash value"}
real	darkrate=0.		{prompt="Dark count rate"}
real	darkslope=0.		{prompt="Slope of dark count rate"}
real	flatslope=0.		{prompt="Flat field slope"}
file	badpix=""		{prompt="Bad pixel regions"}
real	badval=0.		{prompt="Bad pixel value"}
real	gain=1.			{prompt="Gain (e-/adu)", min=1.0e-9}
real	ron=0.			{prompt="Read out noise e-"}
string	nonlin			{prompt="Non-linearity coefficiants"}
bool	poisson=yes		{prompt="Add poisson noise?"}
bool	overwrite=no		{prompt="Overwrite existing image?"}
struct	*fdnl			{prompt="Internal use"}

begin
	int	c1, c2, l1, l2, rseed, i, dummy
	real	exp, value, valslope, invgain, date, rval, coef[7]
	string	im, type, s, lincoefs, ampsec

	im = image
	exp = exptime
	type = ccdtype

	# Check for zero (or very small) gain
	if (abs (gain) < 1.0e-9)
	    call error (0, "zero (or very small) gain specified")

	invgain = 1.0 / gain

	if (access (im//".imh") == yes)
	    im = im // ".imh"
	if (access (im//".hhh") == yes)
	    im = im // ".hhh"
	if (access (im) == yes) {
	    if (overwrite == yes)
		imdelete (im, verify=no)
	    else
	        return
	}

	# Create the image.
	s = str (ncols) // " " // str (nlines)
	mkimage (im, "make", 0., 2, s, pixtype="real", slope=0., sigma=0.)

	# Add a data image.
	if (access (imdata//".imh") == yes)
	    imdata = imdata // ".imh"
	if (access (imdata//".hhh") == yes)
	    imdata = imdata // ".hhh"
	if (access (imdata) == yes)
	    imcopy (imdata//datasec, im//datasec, verbose=no)

	# Add sky.
	value = exp * skyrate
	if (value != 0.)
	    mkimage (im//datasec, "add", value, slope=0., sigma=0.)

	# Add flat field response.
	if (flatslope != 0.)
	    mkimage (im//datasec, "mul", 1., slope=flatslope, sigma=0.)
	    
	# Add preflash level and dark count.
	value = flashval + exp * darkrate
	valslope = flashslope + exp * darkslope
	if ((value != 0.) && (valslope != 0.))
	    mkimage (im//datasec, "add", value, slope=valslope, sigma=0.)

	# Convert to ADU
	mkimage (im//datasec, "mul", invgain, slope=0., sigma=0.)

	# Add poisson and readout noise
	# if seed is 0 pick a fairly arbitrary value
	if (seed == 0) {
	    date | translit ("STDIN", from_string="a-zA-Z: ", delete+) |
	    scan (date)
	    rseed = abs (date / 10000)
	} else {
	    rseed = seed
	}

	# Add non-linearity
	if (nonlin != "") {
	    lincoefs = mktemp ("uparm$tmp")
	    files (nonlin, >> lincoefs)
	    fdnl = lincoefs
	    coef[1] = 1.0
	    for (i=2; i <= 7; i = i+1) {
		dummy = fscan (fdnl, rval)
		if (dummy == EOF) {
		   coef[i] = 0.0
		} else {
		    coef[i] = rval
		}
	    }

	    irlincor (im, im, section= "", coeff1=coef[1], coeff2=coef[2],
	    coeff3=coef[3], coeff4=coef[4], coeff5=coef[5], coeff6=coef[6],
	    coeff7=coef[7], maxadu=65535.0)
	    delete (lincoefs, ver-)
	}

	mknoise (im, background=0., gain=gain, rdnoise=ron, poisson=poisson, 
	seed=rseed, cosrays="", ncosrays=0, comments=no)

	# decrement seed for next use
	if (seed < 0) 
	    seed.p_value = seed - 1

	# Add zero level
	# We add an extra 0.5 so that we nint rather than truncate when 
	# converting to short integer.
	zeroval = zeroval + 0.5
	mkimage (im, "add", zeroval, slope=zeroslope, sigma=0.)

	# Set bad pixels.
	if (access (badpix)) {
	    list = badpix
	    while (fscan (list, c1, c2, l1, l2) != EOF) {
	        if (nscan() != 4)
		    next
	        c1 = max (1, c1)
	        c2 = min (ncols, c2)
	        l1 = max (1, l1)
	        l2 = min (nlines, l2)
	        s = "["//c1//":"//c2//","//l1//":"//l2//"]"
	        mkimage (im//s, "replace", badval, slope=0., sigma=0.)
	    }
	}

	# Convert to ushort data type
	chpixtype (im, im, "ushort", oldpixtype="all", ver-)

	# Set image header
	ccdhedit (im, "exptime", exp, type="real")
	if (type != "")
	    ccdhedit (im, "imagetyp", type, type="string")

	if (datasec != "") {
	    ccdhedit (im, "datasec", datasec, type="string")
	}
	if (trimsec != "")
	    ccdhedit (im, "trimsec", trimsec, type="string")
	if (biassec != "")
	    ccdhedit (im, "biassec", biassec, type="string")
	if (filter != "")
	    ccdhedit (im, "subset", filter, type="string")
end
