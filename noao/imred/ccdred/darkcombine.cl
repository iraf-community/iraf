# DARKCOMBINE -- Process and combine dark count CCD images.

procedure darkcombine (images)

string	images			{prompt="List of dark count images to combine"}
file	output="Dark"		{prompt="Output dark count root name"}
file	sigma=""		{prompt="Output sigma image (optional)"}
string	combine="avsigclip"	{prompt="Type of combine operation"}
string	ccdtype="dark"		{prompt="CCD image type to combine"}
bool	process=no		{prompt="Process images before combining?"}
bool	subsets=no		{prompt="Combine images by subset parameter?"}
bool	delete=no		{prompt="Delete input images after combining?"}
bool	clobber=no		{prompt="Clobber existing output image?"}
bool	exposure=no		{prompt="Scale by the exposure times?"}
bool	scale=no		{prompt="Scale by the mode?"}
bool	offset=no		{prompt="Add offset determined from the mode?"}
bool	weight=no		{prompt="Use a weighted average?"}
string	modesec=""		{prompt="Image section for computing mode"}
real	lowreject=3.		{prompt="Lower sigma clipping factor"}
real	highreject=3.		{prompt="Upper sigma clipping factor"}

begin
	string	ims

	ims = images

	# Process images first if desired.
	if (process == YES)
	    ccdproc (ims, ccdtype=ccdtype)

	# Combine the dark count images.
	combine (ims, output=output, sigma=sigma, combine=combine,
	    ccdtype=ccdtype, subsets=subsets, delete=delete, clobber=clobber,
	    exposure=exposure, scale=scale, offset=offset, weight=weight,
	    modesec=modesec, lowreject=lowreject, highreject=highreject)
end
