# BPLOT -- Batch plotting of spectra with SPLOT

procedure bplot (images)

string	images				{prompt="List of images to plot"}
string	apertures = ""			{prompt="List of apertures to plot"}
int	band = 1			{prompt="Band to plot"}
string	graphics = "stdgraph"		{prompt="Graphics output device"}
string	cursor   = "onedspec$gcurval.dat"	{prompt="Cursor file(s)\n\nSPLOT query parameters to fix"}

string	next_image = ""			{prompt="Next image to plot"}
string	new_image = ""			{prompt="Image to create"}
bool	overwrite = yes			{prompt="Overwrite image?"}
string	spec2 = ""			{prompt="Spectrum"}
real	constant = 0.			{prompt="Constant to be applied"}
real	wavelength = 0.			{prompt="Dispersion coordinate"}
file	linelist = ""			{prompt="File"}
real	wstart = 0.			{prompt="Starting wavelength"}
real	wend = 0.			{prompt="Ending wavelength"}
real	dw = 0.				{prompt="Wavelength per pixel"}
int	boxsize = 2			{prompt="Smoothing box size\n"}

struct	*ilist, *clist

begin
	int	line, ap
	file	ifile, cfile, cur, image

	ifile = mktemp ("bplot")
	cfile = mktemp ("bplot")

	slist (images, apertures=apertures, long_header=no, > ifile)
	files (cursor, > cfile) 
	cur = ""

	ilist = ifile; clist = cfile
	while (fscan (ilist, image, line, ap) != EOF) {
	    if (nscan() < 3)
		next
	    if ((cursor != "") && (fscan (clist, cur) == EOF)) {
		clist = cfile
		line = fscan (clist, cur)
	    }
	    splot (image, line=ap, band=band, graphics=graphics, cursor=cur,
		next_image=next_image, new_image=new_image,
		overwrite=overwrite, spec2=spec2, constant=constant,
		wavelength=wavelength, linelist=linelist, wstart=wstart,
		wend=wend, dw=dw, boxsize=boxsize)
	}
	clist = ""; ilist = ""

	delete (ifile, verify=no)
	delete (cfile, verify=no)
end
