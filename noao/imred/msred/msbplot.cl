# MSBPLOT -- Batch plotting of multispec spectra

procedure msbplot (images)

string	images			{prompt="List of images to plot"}
string	lines = ""		{prompt="Range list of lines to plot"}
string	graphics = "stdgraph"	{prompt="Graphics output device"}
string	cursor   = "onedspec$gcurval"	{prompt="Cursor file(s) one/line\n"}

struct	*ilist, *llist, *tlist, *rlist

begin
	string	emsg = ""
	int	ymax = 1
	int	nline = 1
	int	ncur = 1

	file	ifile, lfile, tfile, rfile
	string	l_images, img, delim, junk, cur
	int	naxis, xsiz, ysiz, lo, hi, i

	cache ("sections")

	ifile = mktemp ("tmp$msb.")
	lfile = mktemp ("tmp$msb.")
	tfile = mktemp ("tmp$msb.")
	rfile = mktemp ("tmp$msb.")
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
		print (i, >> lfile)
	else {
	    print (lines, ",") | translit ("", "^-,0-9", del+) |
		translit ("", "-", "!", del-) | tokens (new-) |
		translit ("", "\n,", " \n", del-, > lfile)

	    list = lfile
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
		delete (lfile, ver-, >& "dev$null")
		sort (tfile, col=0, ign+, num+, rev-) | unique (> lfile)
		delete (tfile, ver-, >& "dev$null")
	    } else
		{ emsg = "Problem with range specification"; goto err }
	}

	count (lfile, >>& rfile)
	if (fscan (rlist, nline) == 0 || nline == 0)
	    { emsg = "Problem with range specification"; goto err }

	cur = cursor
	if (cur != "") {
	    files (cur, > tfile)
	    count (tfile, >>& rfile)
	    if (fscan (rlist, ncur) == 0 || ncur == 0)
		{ emsg = "Problem reading cursor filename(s)"; goto err }
	    if (ncur > 1 && ncur != nline)
		{ emsg = "Line and cursor list lengths don't match"; goto err }
	}

	ilist = ifile
	while (fscan (ilist, img, naxis, xsiz, ysiz) != EOF) {
	    llist = lfile
	    if (ncur > 1) tlist = tfile
	    while (fscan (llist, i) != EOF && i <= ysiz) {
		if (ncur > 1) cur = tlist
		splot (img, line=i, graphics=graphics, cursor=cur)
	    }
	}

err:	if (emsg != "") print ("ERROR: ", emsg)
	ilist = ""; llist = ""; tlist = ""; rlist = ""
	delete (ifile//","//lfile//","//tfile//","//rfile, ver-, >& "dev$null")
end
