# REVIEWPROTO

procedure reviewproto (catalog)

string	catalog			{prompt="Catalog"}
bool	nooverlay = yes		{prompt="Display image without overlays"}
bool	overlay = yes		{prompt="Display image with overlays"}
bool	comparison = yes	{prompt="Display comparison image"}
file	compimage = ""		{prompt="Comparison image"}
int	box = 200		{prompt="Box size (pixels)"}
string	ocolors = "green"	{prompt="Object mask color"}
string	lcolor = "red"		{prompt="Label color"}

struct	*fd

begin
	file	cat, im, mask, coords, compim, temp
	int	naxis1, naxis2, icolor, frame, nframe, x1, x2, y1, y2
	real	r, d, x, y, xt, yt
	bool	pause
	string	key, imsec
	struct	title

	coords = mktemp ("tmp$iraf")
	temp = mktemp ("tmp$iraf")

	# Get query parameters.
	cat = catalog

	# Get header and coordinates.
	tdump (cat, cdfile="", pfile=temp, datafile=coords,
	    columns="ra,dec", rows="", pwidth=80)
	match ("IMAGE", temp, stop-) | scan (im, im, im)
	match ("MASK", temp, stop-) | scan (mask, mask, mask)
	delete (temp, verify-)

	# Set image size.
	sections (im, option="root") | scan (im)
	hselect (im, "naxis1,naxis2", yes) | scan (naxis1, naxis2)

	# Set comparison.
	sections (compimage, option="root") | scan (compim)

	# Translate color specification.
	match (lcolor, "ace$colors.dat", stop-) | scan (lcolor, icolor)
	if (nscan() != 2)
	    icolor = 200

	# Number of frames.
	nframe = 0
	if (nooverlay)
	    nframe = nframe + 1
	if (overlay)
	    nframe = nframe + 1
	if (comparison && compim != "")
	    nframe = nframe + 1

	# Loop through the list of catalog coordinates.
	pause = NO
	fd = coords
	while (fscan (fd, r, d) != EOF) {
	    if (nscan() < 2)
		next
	    if (r == INDEF ||d == INDEF)
		next

	    # Pause with cursor read if there is more than one coordinate.
	    if (pause) {
		printf ("q to quit any other key to continue...\n")
		if (fscan (imcur, x, y, i, key) == EOF)
		    break
		if (key == 'q')
		    break
		pause = NO
	    }

	    # Display.
	    frame = nframe

	    if (comparison && compim != "") {
		# Convert world coordinate to image section.
		print (r, d) | wcsctran ("STDIN", "STDOUT", compim, "world",
		    "logical", columns="1 2", units="native native",
		    formats="", min_sigdigit=9, verbose=no) | scan (x, y)
		x = nint (x); y = nint (y)
		x1 = max (1, nint (x-box/2.))
		x2 = min (naxis1, nint (x+box/2.))
		y1 = max (1, nint (y-box/2.))
		y2 = min (naxis2, nint (y+box/2.))
		if (x2 > x1 && y2 > y1) {
		    # Display section.
		    printf ("%s[%d:%d,%d:%d]\n", compim, x1, x2, y1, y2) |
			scan (imsec)
		    acedisplay (imsec, frame, bpmask="", bpdisplay="none",
			bpcolors="red", overlay="", ocolors=ocolors,
			erase=yes, border_erase=no, select_frame=yes,
			repeat=no, fill=no, zscale=yes, contrast=0.25,
			zrange=yes, zmask="", nsample=1000, xcenter=0.5,
			ycenter=0.5, xsize=1., ysize=1., xmag=2., ymag=2.,
			order=0, z1=0., z2=0., ztrans="linear", lutfile="",
			>> "dev$null")

		    # Mark.
		    printf ("%g %g\n", x, y, >> temp)
		    tvmark (frame, temp, logfile="", autolog=no,
			outimage="", deletions="", commands="",
			mark="circle", radii="10", lengths="0",
			font="raster", color=icolor, label=no,
			number=no, nxoffset=0, nyoffset=0, pointsize=1,
			txsize=1, tolerance=1.5, interactive=no)
		    delete (temp, verify-)

		    # Label.
		    xt = x1 + 10
		    yt = y2 + 10
		    printf ("%g %g '%.2H %.1h'\n", xt, yt, r, d, >> temp)
		    tvmark (frame, temp, logfile="", autolog=no,
			outimage="", deletions="", commands="",
			mark="none", radii="0", lengths="0",
			font="raster", color=icolor, label=yes,
			number=no, nxoffset=0, nyoffset=0, pointsize=1,
			txsize=2, tolerance=1.5, interactive=no)
		    delete (temp, verify-)
		}
		frame = frame - 1
	    }

	    # Convert world coordinate to image section.
	    print (r, d) | wcsctran ("STDIN", "STDOUT", im, "world",
		"logical", columns="1 2", units="native native",
		formats="", min_sigdigit=9, verbose=no) | scan (x, y)
	    x = nint (x); y = nint (y)
	    x1 = max (1, nint (x-box/2.))
	    x2 = min (naxis1, nint (x+box/2.))
	    y1 = max (1, nint (y-box/2.))
	    y2 = min (naxis2, nint (y+box/2.))
	    if (x2 <= x1 || y2 <= y1)
		next

	    # Display.
	    if (overlay) {
		printf ("%s[%d:%d,%d:%d]\n", im, x1, x2, y1, y2) | scan (imsec)
		acedisplay (imsec, frame, bpmask="", bpdisplay="none",
		    bpcolors="red", overlay=mask, ocolors=ocolors,
		    erase=yes, border_erase=no, select_frame=yes,
		    repeat=no, fill=no, zscale=yes, contrast=0.25,
		    zrange=yes, zmask="", nsample=1000, xcenter=0.5,
		    ycenter=0.5, xsize=1., ysize=1., xmag=2., ymag=2.,
		    order=0, z1=0., z2=0., ztrans="linear", lutfile="",
		    >> "dev$null")

		# Mark
		printf ("%g %g\n", x, y, >> temp)
		tvmark (frame, temp, logfile="", autolog=no,
		    outimage="", deletions="", commands="",
		    mark="circle", radii="10", lengths="0",
		    font="raster", color=icolor, label=no,
		    number=no, nxoffset=0, nyoffset=0, pointsize=1,
		    txsize=1, tolerance=1.5, interactive=no)
		delete (temp, verify-)

		xt = x1 + 10
		yt = y2 + 10
		printf ("%g %g '%.2H %.1h'\n", xt, yt, r, d, >> temp)
		tvmark (frame, temp, logfile="", autolog=no,
		    outimage="", deletions="", commands="",
		    mark="none", radii="0", lengths="0",
		    font="raster", color=icolor, label=yes,
		    number=no, nxoffset=0, nyoffset=0, pointsize=1,
		    txsize=2, tolerance=1.5, interactive=no)
		delete (temp, verify-)

		frame = frame - 1
	    }

	    # Display.
	    if (nooverlay) {
		printf ("%s[%d:%d,%d:%d]\n", im, x1, x2, y1, y2) | scan (imsec)
		acedisplay (imsec, frame, bpmask="", bpdisplay="none",
		    bpcolors="red", overlay="", ocolors=ocolors,
		    erase=yes, border_erase=no, select_frame=yes,
		    repeat=no, fill=no, zscale=yes, contrast=0.25,
		    zrange=yes, zmask="", nsample=1000, xcenter=0.5,
		    ycenter=0.5, xsize=1., ysize=1., xmag=2., ymag=2.,
		    order=0, z1=0., z2=0., ztrans="linear", lutfile="",
		    >> "dev$null")

		# Mark
		printf ("%g %g\n", x, y, >> temp)
		tvmark (frame, temp, logfile="", autolog=no,
		    outimage="", deletions="", commands="",
		    mark="circle", radii="10", lengths="0",
		    font="raster", color=icolor, label=no,
		    number=no, nxoffset=0, nyoffset=0, pointsize=1,
		    txsize=1, tolerance=1.5, interactive=no)
		delete (temp, verify-)

		xt = x1 + 10
		yt = y2 + 10
		printf ("%g %g '%.2H %.1h'\n", xt, yt, r, d, >> temp)
		tvmark (frame, temp, logfile="", autolog=no,
		    outimage="", deletions="", commands="",
		    mark="none", radii="0", lengths="0",
		    font="raster", color=icolor, label=yes,
		    number=no, nxoffset=0, nyoffset=0, pointsize=1,
		    txsize=2, tolerance=1.5, interactive=no)
		delete (temp, verify-)

		frame = frame - 1
	    }

	    pause = YES
	}
	fd = ""; delete (coords, verify-)
end
