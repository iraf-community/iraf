# BPLOT -- Batch plotting of spectra with SPLOT

procedure bplot (images)

string	images				{prompt="List of images to plot"}
string	apertures = ""			{prompt="List of apertures to plot"}
int	band = 1			{prompt="Band to plot"}
string	graphics = "stdgraph"		{prompt="Graphics output device"}
string	cursor   = "onedspec$gcurval"	{prompt="Cursor file(s)\n"}

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
	    splot (image, line=ap, band=band, graphics=graphics, cursor=cur)
	}
	clist = ""; ilist = ""

	delete (ifile, verify=no)
	delete (cfile, verify=no)
end
