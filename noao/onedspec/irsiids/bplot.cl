# BPLOT -- Batch plotting of spectra with SPLOT

procedure bplot (images, records)

string	images				{prompt="List of images to plot"}
string	records = ""			{prompt="List of records to plot"}
string	graphics = "stdgraph"		{prompt="Graphics output device"}
string	cursor   = "onedspec$gcurval.dat"	{prompt="Cursor file(s)\n"}

struct	*ilist, *clist

begin
	int	line, ap
	file	ifile, cfile, cur, image

	ifile = mktemp ("bplot")
	cfile = mktemp ("bplot")

	names (images, records, >& ifile)
	files (cursor, > cfile) 
	cur = ""

	ilist = ifile; clist = cfile
	while (fscan (ilist, image) != EOF) {
	    if ((cursor != "") && (fscan (clist, cur) == EOF)) {
		clist = cfile
		line = fscan (clist, cur)
	    }
	    splot (image, graphics=graphics, cursor=cur)
	}
	clist = ""; ilist = ""

	delete (ifile, verify=no)
	delete (cfile, verify=no)
end
