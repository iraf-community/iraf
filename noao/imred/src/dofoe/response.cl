# RESPONSE -- Make a fiber response spectrum using a flat field and sky flat.

procedure response (flat, apreference, response)

string	flat			{prompt="Flat field spectrum"}
string	apreference		{prompt="Aperture reference spectrum"}
string	response		{prompt="Response spectrum"}

bool	recenter = no		{prompt="Recenter sky apertures?"}
bool	edit = no		{prompt="Edit/review sky apertures?"}
bool	trace = no		{prompt="Trace sky spectra?"}
bool	clean = no		{prompt="Detect and replace bad pixels?"}
bool	fitflat = no		{prompt="Fit and ratio flat field spectrum?"}
bool	interactive = yes	{prompt="Fit flat field interactively?"}
string	function = "spline3"	{prompt="Fitting function",
				 enum="spline3|legendre|chebyshev|spline1"}
int	order = 20		{prompt="Fitting function order", min=1}

begin
	string	imtype
	file	log1, log2, flat2d, flatec, resp
	int	i, n
	struct	err

	imtype = "." // envget ("imtype")
	i = stridx (",", imtype)
	if (i > 0)
	    imtype = substr (imtype, 1, i-1)
	n = strlen (imtype)

	flat2d = flat
	resp = response

	if (flat2d == "")
	    error (1, "No flat field defined")
	if (flat2d != "") {
	    i = strlen (flat2d)
	    if (i > n && substr (flat2d, i-n+1, i) == imtype)
		flat2d = substr (flat2d, 1, i-n)
	    flatec = flat2d // ".ec"
	    if (!access (flat2d // imtype)) {
		printf ("Flat field spectrum not found - %s%s\n",
		    flat2d, imtype) | scan (err)
		error (1, err // "\nCheck setting of imtype")
	    }
	}

	tee.append = yes
	if (logfile == "")
	    log1 = "dev$null"
	else
	    log1 = logfile
	if (verbose)
	    log2 = "STDOUT"
	else
	    log2 = "dev$null"

	# Initialize APALL
	apscript.references = apreference
	if (recenter)
	    apscript.ansrecenter = "YES"
	else
	    apscript.ansrecenter = "NO"
	apscript.ansresize = "NO"
	if (edit)
	    apscript.ansedit = "yes"
	else
	    apscript.ansedit = "NO"
	if (trace)
	    apscript.anstrace = "YES"
	else
	    apscript.anstrace = "NO"
	apscript.ansextract = "YES"

	print ("Extract flat field ", flat2d) | tee (log1)
	if (flat2d == apscript.references)
	    apscript (flat2d, ansrecenter="NO", ansedit="NO", anstrace="NO",
		background="none", clean=clean, extras=no)
	else
	    apscript (flat2d, clean=clean, extras=no)

	if (fitflat) {
	    print ("Fit and ratio flat field ", flat2d) | tee (log1)
	    fit1d (flatec, resp, "fit", axis=1, interactive=interactive,
		sample="*", naverage=1, function=function, order=order,
		low_reject=0., high_reject=0., niterate=1, grow=0.,
		graphics="stdgraph")
	    sarith (flatec, "/", resp, resp, w1=INDEF, w2=INDEF, apertures="",
		bands="", beams="", apmodulus=0, reverse=no, ignoreaps=yes,
		format="multispec", renumber=no, offset=0, clobber=yes,
		merge=no, errval=0, verbose=no)
	    imdelete (flatec, verify=no)
	} else
	    imrename (flatec, resp, verbose=no)

	print ("Create the normalized response ", resp) | tee (log1)
	bscale (resp, resp, bzero="0.", bscale="mean", section="",
	    step=1, upper=INDEF, lower=INDEF, verbose=yes) | tee (log1, >log2)
end
