# SHUTCOR - calculate the shutter correction for a detector given
# a sequence of overscan corrected flats of varying durations.  The
# shutter correction is the intercept on a plot of exposure duration
# versus exposure level.  Notion courtesy Phil Massey.

procedure shutcor (images)

string	images			{prompt="Overscan corrected images"}
string	section	= "[*,*]"	{prompt="Image section for statistics"}
string	center	= "mode"	{prompt="Central statistical measure",
				    enum="mean|midpt|mode"}
int	nclip = 3		{prompt="Number of clipping iterations"}
real	lsigma = 4		{prompt="Lower clipping sigma factor"}
real	usigma = 4		{prompt="Upper clipping sigma factor"}
string	exposure = "exptime"	{prompt="Header keyword for the exposure time"}
bool	verbose	= yes		{prompt="Verbose output?"}

string	*list

begin
	string	limages, img, imglist, statlist, explist, tmplist
	real	exp, shutcorr, shutcorr_err
	real	slope, slope_err, intercept, intercept_err
	int	nstat, nexp, junk
	struct	tmp

	cache sections

	limages = images

	imglist = mktemp ("tmp$tmp")
	statlist = mktemp ("tmp$tmp")
	explist = mktemp ("tmp$tmp")
	tmplist = mktemp ("tmp$tmp")

	sections (limages, option="fullname", > imglist)
	if (sections.nimages < 4) {
	    printf ("You need a minimum of four images!\n")
	    delete (imglist, ver-, >& "dev$null")
	    return
	}

	hselect ("@"//imglist, "$I,"//exposure//",overscan", yes, > tmplist)
	list = tmplist
	while (fscan (list, img, exp, tmp) != EOF) {
	    if (strlen (tmp) == 0) {
		printf ("%s is not overscan corrected! (Check with ccdlist)\n",
		    img)
		delete (imglist, ver-, >& "dev$null")
		delete (tmplist, ver-, >& "dev$null")
		return
	    }
	    if (exp <= 0) {
		printf ("%s has zero exposure time!\n",
		    img)
		delete (imglist, ver-, >& "dev$null")
		delete (tmplist, ver-, >& "dev$null")
		return
	    }
	}

	list = ""; delete (tmplist, ver-, >& "dev$null")

	hselect ("@"//imglist, "$I,flatcor", yes, > tmplist)
	list = tmplist
	while (fscan (list, img, tmp) != EOF)
	    if (strlen (tmp) != 0)
		printf ("%s is flat fielded\n", img)

	list = ""; delete (tmplist, ver-, >& "dev$null")

	imstatistics ("@"//imglist, fields=center,
	    lower=INDEF, upper=INDEF, nclip=nclip, lsigma=lsigma,
	    usigma=usigma, binwidth=0.1, format-, > statlist)

	hselect ("@"//imglist, exposure, yes, > explist)
	delete (imglist, ver-, >& "dev$null")

	count (statlist) | scan (nstat)
	count (explist) | scan (nexp)

	if (nstat != nexp) {
	    printf ("Problem matching statistics with exposure times!\n")
	    delete (statlist, ver-, >& "dev$null")
	    delete (explist, ver-, >& "dev$null")
	    return
	}

	join (explist, statlist, output="STDOUT", delim=" ", missing="INDEF",
	    shortest+, verbose-) | polyfit ("STDIN", 1, weighting="uniform",
	    verbose=verbose, listdata-, > tmplist)

	delete (explist, ver-, >& "dev$null")
	delete (statlist, ver-, >& "dev$null")

	list = tmplist
	junk = fscan (list, intercept, slope)
	junk = fscan (list, intercept_err, slope_err)
	list = ""

	shutcorr = intercept / slope
	shutcorr_err = abs (shutcorr) *
	    sqrt ((intercept_err/intercept)**2 + (slope_err/slope)**2)

	if (verbose)
	    printf ("\n")

	printf ("Shutter correction = %.3f +/- %.3f seconds\n",
	    shutcorr, shutcorr_err)

	if (verbose) {
	    printf ("\nInformation about the %s versus %s fit:\n\n",
		center, exposure)
	    printf ("       intercept        slope     (and errors)\n")
	    printf ("!sed 's+^+    +' %s\n", osfn(tmplist)) | cl
	    printf ("\n")
	}

	delete (tmplist, ver-, >& "dev$null")
end
