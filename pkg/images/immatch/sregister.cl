# SREGISTER  -- Compute the geometric transformation required to register an
# input image to a reference image using celestial coordinate WCS information
# in the input and reference image headers, and perform the registration.
# SREGISTER is a simple script task which calls the SKYXYMATCH task to compute
# the control points, the GEOMAP task to compute the transformation, and the
# GEOTRAN task to do the registration.

procedure sregister (input, reference, output)

file	input		{prompt="The input images"}
file	reference	{prompt="Input reference images"}
file	output		{prompt="The output registered images"}
real	xmin		{INDEF,
			prompt="Minimum logical x reference coordinate value"}
real	xmax		{INDEF,
			prompt="Maximum logical x reference coordinate value"}
real	ymin		{INDEF,
			prompt="Minimum logical y reference coordinate value"}
real	ymax		{INDEF,
			prompt="Maximum logical y reference coordinate value"}
int	nx		{10, prompt="Number of grid points in x"}
int	ny		{10, prompt="Number of grid points in y"}
string	wcs		{"world", prompt="The default world coordinate system",
			enum="physical|world"}
string	xformat		{"%10.3f", prompt="Output logical x coordinate format"}
string	yformat		{"%10.3f", prompt="Output logical y coordinate format"}
string	rwxformat	{"",
			prompt="Output reference world x coordinate format"}
string	rwyformat	{"",
			prompt="Output reference world y coordinate format"}
string	wxformat	{"", prompt="Output world x coordinate format"}
string	wyformat	{"", prompt="Output world y coordinate format"}

string	fitgeometry	{"general",
			prompt="Fitting geometry",
			enum="shift|xyscale|rotate|rscale|rxyscale|general"}
string	function	{"polynomial",
			prompt="Type of coordinate surface to be computed",
			enum="legendre|chebyshev|polynomial"}
int	xxorder		{2, prompt="Order of x fit in x"}
int	xyorder		{2, prompt="Order of x fit in y"}
string	xxterms		{"half", enum="none|half|full",
                          prompt="X fit cross terms type"}
int	yxorder		{2, prompt="Order of y fit in x"}
int	yyorder		{2, prompt="Order of y fit in y"}
string	yxterms		{"half", enum="none|half|full",
                          prompt="Y fit cross terms type"}
real	reject		{INDEF, prompt="The rejection limit in units of sigma"}
string	calctype	{"real", prompt="Transformation computation type",
			enum="real|double"}

string	geometry	{"geometric", prompt="Transformation geometry",
			enum="linear|geometric"}
real	xsample		{1.0,prompt="X coordinate sampling interval"}
real	ysample		{1.0,prompt="Y coordinate sampling interval"}
string	interpolant	{"linear", prompt="The interpolant type"}
string	boundary	{"nearest", prompt="Boundary extensiontype",
			enum="nearest|constant|reflect|wrap"}
real	constant	{0.0, prompt="Constant for constant boundary extension"}
bool	fluxconserve	{yes, prompt="Preserve image flux ?"}
int	nxblock		{512, prompt="X dimension blocking factor"}
int	nyblock		{512, prompt="Y dimension blocking factor"}

bool	wcsinherit	{yes, prompt="Inherit wcs of the reference image ?"}

bool	verbose		{yes, prompt="Print messages about progress of task?"}
bool	interactive	{no, prompt="Compute transformation interactively? "}
string	graphics	{"stdgraph", prompt="The standard graphics device"}
gcur	gcommands	{"", prompt="The graphics cursor"}


begin
	# Declare local variables.
	int nimages
	string tinput, treference, tcoords, tcname, tdatabase, toutput
	string	tsections1, tsections2

	# Get the query parameters.
	tinput = input
	treference = reference
	toutput = output

	# Cache the sections task.
	cache sections

	# Get the coordinates file list.
	tsections1 = mktemp ("tmps1")
	tsections2 = mktemp ("tmps2")
	if (access ("imxymatch.1")) {
	    tcoords = mktemp ("imxymatch")
	} else {
	    tcoords = "imxymatch"
	}
	sections (tinput, option="fullname", > tsections1)
	nimages = sections.nimages
	for (i = 1; i <= nimages; i = i + 1) {
	    printf ("%s\n", tcoords // "." // i, >> tsections2)
	}
	delete (tsections1, go_ahead+, verify-, default_action+,
	    allversions+, subfiles+, > "dev$null")
	tcname = "@"//tsections2

	# Get the output database file name.
	if (access ("sregister.db")) {
	    tdatabase = mktemp ("tmpdb") 
	} else {
	    tdatabase = "sregister.db"
	}

	# Compute the control points.
	skyxymatch (tinput, treference, tcname, coords="grid", xmin=xmin,
	    xmax=xmax, ymin=ymin, ymax=ymax, nx=nx, ny=ny, wcs=wcs,
	    xcolumn=1, ycolumn=1, xunits="", yunits="", xformat=xformat,
	    yformat=yformat, rwxformat=rwxformat, rwyformat=rwyformat,
	    wxformat=wxformat, wyformat=wyformat, min_sigdigits=7, verbose=no)

	# Compute the transformation.
	geomap (tcname, tdatabase, xmin, xmax, ymin, ymax, transforms=tinput,
	    results="", fitgeometry=fitgeometry, function=function,
	    xxorder=xxorder, xyorder=xyorder, xxterms=xxterms, yxorder=yxorder,
	    yyorder=yyorder, yxterms=yxterms, reject=reject, calctype=calctype,
	    verbose=verbose, interactive=interactive, graphics=graphics,
	    cursor=gcommands)

	# Register the images.
	geotran (tinput, toutput, database=tdatabase, transforms=tinput,
            geometry=geometry, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax,
            xscale=1.0, yscale=1.0, ncols=INDEF, nlines=INDEF,
            interpolant=interpolant, boundary=boundary, constant=constant,
            fluxconserve=fluxconserve, xsample=xsample, ysample=ysample,
            nxblock=nxblock, nyblock=nyblock, xin=INDEF, yin=INDEF, xout=INDEF,
            yout=INDEF, xshift=INDEF, yshift=INDEF, xmag=INDEF, ymag=INDEF,
            xrotation=INDEF, yrotation=INDEF, verbose=verbose)

	# Copy the reference wcs to the input images.
	if (wcsinherit) {
	    wcscopy (toutput, treference, verbose-)
	}

	# Delete the coordinates files.
	delete (tcname, go_ahead+, verify-, default_action+,
	    allversions+, subfiles+, > "dev$null")

	# Delete the coordinates file list.
	delete (tsections2, go_ahead+, verify-, default_action+,
	    allversions+, subfiles+, > "dev$null")

	# Delete the database file.
	delete (tdatabase, go_ahead+, verify-, default_action+,
	    allversions+, subfiles+, > "dev$null")
end
