# WCSMAP -- Compute the geometric transformation required to register an
# input image to a reference image using WCS information in the input and
# reference image headers. WCSMAP is a simple script task which calls the
# WCSXYMATCH task to compute the control points followed by the GEOMAP
# task to compute the transformation.


procedure wcsmap (input, reference, database)

file	input		{prompt="The input images"}
file	reference	{prompt="The input reference images"}
file	database	{prompt="The output database file"}
string	transforms	{"", prompt="The database transform names"}
string	results		{"", prompt="The optional results summary files"}
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
bool	transpose	{no, prompt="Force a world coordinate tranpose ?"}
string	xformat		{"%10.3f", prompt="Output logical x coordinate format"}
string	yformat		{"%10.3f", prompt="Output logical y coordinate format"}
string	wxformat	{"", prompt="Output world x coordinate format"}
string	wyformat	{"", prompt="Output world y coordinate format"}
string	fitgeometry	{"general",
			prompt="Fitting geometry",
			enum="shift|xyscale|rotate|rscale|rxyscale|general"}
string	function	{"polynomial", prompt="Surface type",
			enum="legendre|chebyshev|polynomial"}
int	xxorder		{2, prompt="Order of x fit in x"}
int	xyorder		{2, prompt="Order of x fit in y"}
string	xxterms		{"half", enum="none|half|full",
			 prompt="X fit cross terms type"}
int	yxorder		{2, prompt="Order of y fit in x"}
int	yyorder		{2, prompt="Order of y fit in y"}
string	yxterms		{"half", enum="none|half|full",
			  prompt="Y fit cross terms type"}
real	reject		{INDEF, prompt="Rejection limit in sigma units"}
string	calctype	{"real", prompt="Computation precision",
			enum="real|double"}
bool	verbose		{yes, prompt="Print messages about progress of task ?"}
bool	interactive	{yes, prompt="Compute transformation interactively ? "}
string	graphics	{"stdgraph", prompt="Default graphics device"}
gcur	gcommands	{"", prompt="Graphics cursor"}


begin
	# Declare local variables.
	int nimages
	string tinput, treference, toutput, ttransforms, tresults, tcoords
	string	tsections1, tsections2, tcname

	# Cache the sections task.
	cache sections

	# Get the query parameters.
	tinput = input
	treference = reference
	toutput = database
	if (transforms == "") {
	    ttransforms = tinput
	} else {
	    ttransforms = transforms
	}
	tresults = results

	# Get the temporary coordinates file list.
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

	# Compute the control points.
	wcsxymatch (tinput, treference, tcname, coords="grid", xmin=xmin,
	    xmax=xmax, ymin=ymin, ymax=ymax, nx=nx, ny=ny, wcs=wcs,
	    transpose=transpose, xcolumn=1, ycolumn=1, xunits="", yunits="",
	    xformat=xformat, yformat=yformat, wxformat=wxformat,
	    wyformat=wyformat, min_sigdigits=7, verbose=no)

	# Compute the transformation.
	geomap (tcname, toutput, xmin, xmax, ymin, ymax, transforms=ttransforms,
	    results = tresults, fitgeometry=fitgeometry, function=function,
	    xxorder=xxorder, xyorder=xyorder, xxterms=xxterms, yxorder=yxorder,
	    yyorder=yyorder, yxterms=yxterms, reject=reject, calctype=calctype,
	    verbose=verbose, interactive=interactive, graphics=graphics,
	    cursor=gcommands)

	# Cleanup.
	delete (tcname, go_ahead+, verify-, default_action+,
	    allversions+, subfiles+, > "dev$null")
	delete (tsections2, go_ahead+, verify-, default_action+,
	    allversions+, subfiles+, > "dev$null")
end
