# SUBSECTION -- Make a subsection CCD observation

procedure subsection (subimage, image)

string	subimage		{prompt="Subsection image name"}
string	image			{prompt="Full image name"}

int	ncols=82		{prompt="Number of columns"}
int	nlines=50		{prompt="Number of lines"}
string	ccdsec="[26:75,26:75]"	{prompt="CCD section"}
string	datasec="[1:50,1:50]"	{prompt="Data section"}
string	trimsec=""		{prompt="Trim section"}
string	biassec="[51:82,1:50]"	{prompt="Bias section"}
bool	overwrite=no		{prompt="Overwrite existing image?"}

begin
	string	im, imdata, s
	real	biasval, sigma

	im = subimage
	imdata = image
	biasval = artobs.biasval
	sigma = artobs.sigma

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
	s = "[1:" // str (ncols) // ",1:" // str(nlines) // "]"
	imcopy (imdata//s, im, verbose=no)

	# Copy subsection image.
	imcopy (imdata//ccdsec, im//datasec, verbose=no)

	# Add bias.
	if (biasval != 0.)
	    mkimage (im//biassec, "replace", biasval, slope=0., sigma=sigma,
		seed=0)

	# Set image header
	ccdhedit (im, "ccdsec", ccdsec, type="string")
	ccdhedit (im, "datasec", datasec, type="string")
	ccdhedit (im, "trimsec", trimsec, type="string")
	ccdhedit (im, "biassec", biassec, type="string")
end
