# ECBPLOT -- Batch plotting of echelle spectra

procedure ecbplot (images)

string	images			{prompt="List of images to plot"}
string	lines = ""		{prompt="Range list of echelle orders to plot"}
string	graphics = "stdgraph"	{prompt="Graphics output device"}
string	cursor   = "onedspec$gcurval"	{prompt="Cursor file(s) one/order\n"}

struct	*ilist, *olist, *tlist, *rlist

begin
	string	emsg = ""
	int	ymax = 1
	int	nord = 1
	int	ncur = 1

	file	ifile, ofile, tfile, rfile
	string	l_images, img, delim, junk, cur
	int	naxis, xsiz, ysiz, lo, hi, i

	cache ("sections")

	ifile = mktemp ("tmp$ecb.")
	ofile = mktemp ("tmp$ecb.")
	tfile = mktemp ("tmp$ecb.")
	rfile = mktemp ("tmp$ecb.")
	rlist = rfile

	l_images = images

	sections (l_images, option="fullname", > tfile)
	if (sections.nimages == 0) goto err

	tlist = tfile
	while (fscan (tlist, img) != EOF) {
	    hselect (img, "i_naxis", "yes", >> rfile)

	    if (fscan (rlist, naxis, xsiz, ysiz) < 2) {
		emsg = "Problem reading the header for: " // img; goto err
	    } else if (naxis > 2) {
		emsg = img // " is " // naxis // " dimensional!"; goto err
	    } else if (naxis == 1)
		ysiz = 1

	    ymax = max (ysiz, ymax)
	    print (img, "\t", naxis, xsiz, ysiz, >> ifile)
	}
	delete (tfile, ver-, >& "dev$null")

	if (lines == "")
	    for (i = 1; i <= ymax; i += 1)
		print (i, >> ofile)
	else {
	    print (lines, ",") | translit ("", "^-,0-9", del+) |
		translit ("", "-", "!", del-) | tokens (new-) |
		translit ("", "\n,", " \n", del-, > ofile)

	    list = ofile
	    while (fscan (list, lo, delim, hi, junk) != EOF)
		if (nscan() == 0)
		    next
		else if (nscan() == 1 && lo >= 1 && lo <= ymax)
		    print (lo, >> tfile)
		else if (nscan() == 3 && delim == "!")
		    for (i=min(ymax,max(1,lo)); i<=min(ymax,max(1,hi)); i+=1)
			print (i, >> tfile)
		else
		    { emsg = "Problem with range specification"; goto err }

	    if (access (tfile)) {
		delete (ofile, ver-, >& "dev$null")
		sort (tfile, col=0, ign+, num+, rev-) | unique (> ofile)
		delete (tfile, ver-, >& "dev$null")
	    } else
		{ emsg = "Problem with range specification"; goto err }
	}

	count (ofile, >>& rfile)
	if (fscan (rlist, nord) == 0 || nord == 0)
	    { emsg = "Problem with range specification"; goto err }

	cur = cursor
	if (cur != "") {
	    files (cur, > tfile)
	    count (tfile, >>& rfile)
	    if (fscan (rlist, ncur) == 0 || ncur == 0)
		{ emsg = "Problem reading cursor filename(s)"; goto err }
	    if (ncur > 1 && ncur != nord)
		{ emsg = "Order and cursor list lengths don't match"; goto err }
	}

	ilist = ifile
	while (fscan (ilist, img, naxis, xsiz, ysiz) != EOF) {
	    olist = ofile
	    if (ncur > 1) tlist = tfile
	    while (fscan (olist, i) != EOF && i <= ysiz) {
		if (ncur > 1) cur = tlist
		splot (img, line=i, graphics=graphics, cursor=cur)
	    }
	}

err:	if (emsg != "") print ("ERROR: ", emsg)
	ilist = ""; olist = ""; tlist = ""; rlist = ""
	delete (ifile//","//ofile//","//tfile//","//rfile, ver-, >& "dev$null")
end
