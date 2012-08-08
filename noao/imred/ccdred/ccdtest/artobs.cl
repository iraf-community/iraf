# ARTOBS -- Make a CCD observation

procedure artobs (image, exptime, ccdtype)

string	image			{prompt="Image name"}
real	exptime			{prompt="Exposure time"}
string	ccdtype			{prompt="CCD type"}

int	ncols=132		{prompt="Number of columns"}
int	nlines=100		{prompt="Number of lines"}
string	filter=""		{prompt="Filter"}
string	datasec="[1:100,1:100]"	{prompt="Data section"}
string	trimsec="[3:98,3:98]"	{prompt="Trim section"}
string	biassec="[103:130,*]"	{prompt="Bias section"}

file	imdata=""		{prompt="Image data"}
real	skyrate=0.		{prompt="Sky count rate"}
file	badpix=""		{prompt="Bad pixel regions"}
real	biasval=500.		{prompt="Bias value"}
real	badval=500.		{prompt="Bad pixel value"}
real	zeroval=100.		{prompt="Zero level value"}
real	darkrate=1.		{prompt="Dark count rate"}
real	zeroslope=0.01		{prompt="Slope of zero level"}
real	darkslope=0.002		{prompt="Slope of dark count rate"}
real	flatslope=0.0003	{prompt="Flat field slope"}
real	sigma=5.		{prompt="Gaussian sigma"}
int	seed=0			{prompt="Random number seed"}
bool	overwrite=no		{prompt="Overwrite existing image?"}

begin
	int	c1, c2, l1, l2
	real	exp, value, valslope
	string	im, type, s

	im = image
	exp = exptime
	type = ccdtype

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
	mkimage (im, "make", 0., 2, s, pixtype="short", slope=0., sigma=sigma,
	    seed=seed)

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
	    
	# Add zero level and dark count.
	value = zeroval + exp * darkrate
	valslope = zeroslope + exp * darkslope
	if ((value != 0.) && (valslope != 0.))
	    mkimage (im//datasec, "add", value, slope=valslope, sigma=0.)

	# Add bias.
	if (biasval != 0.)
	    mkimage (im, "add", biasval, slope=0., sigma=sigma, seed=0)

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

	# Set image header
	ccdhedit (im, "exptime", exp, type="real")
	if (type != "")
	    ccdhedit (im, "imagetyp", type, type="string")
	if (datasec != "")
	    ccdhedit (im, "datasec", datasec, type="string")
	if (trimsec != "")
	    ccdhedit (im, "trimsec", trimsec, type="string")
	if (biassec != "")
	    ccdhedit (im, "biassec", biassec, type="string")
	if (filter != "")
	    ccdhedit (im, "subset", filter, type="string")
end
