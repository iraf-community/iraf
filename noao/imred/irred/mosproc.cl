# MOSPROC - Sky subtract, flat field and transpose images before mosaicing.

procedure mosproc (input, output, nxsub, nysub)

string  input           {prompt="Input images"}
string  output		{prompt="Output image"}
int     nxsub           {8, prompt="Number of subrasters in x"}
int     nysub           {8, prompt="Number of subrasters in y"}

bool	skysubtract	{yes, prompt="Sky subtract images before mosaicing"}
string	sky		{"", prompt="Sky image to subtract"}
string  exclude         {"", prompt="Input images excluded from sky frame"}
string	expname		{"EXPTIME", prompt="Image exposure time keywords"} 

bool	flatten		{yes, prompt="Flatten images before mosaicing"}
string	flat		{"", prompt="Flat field image"}
bool	transpose	{no,  prompt="Transpose images before mosaicing?"}

string  trim_section    {"[*,*]", prompt="Input image section to be extracted"}
string	corner          {"lr", prompt="Starting corner for the mosaic"}
string  direction       {"row", prompt="Starting direction for the mosaic"}
bool	raster          {no, prompt="Raster scan?"}
string  median_section  {"", prompt="Input subraster section for median ?"}
bool    subtract        {no, prompt="Substract median from each subraster?"}
real	oval		{-1.0, prompt="Mosaic border pixel values"}

bool    delete		{yes, prompt="Delete temporary images?"}
file    logfile		{"STDOUT", prompt="Log file name"}

struct	*list1, *list2
 
begin
	file	tmpimg, tmptmp, tmpred, tmpexc
	int	nx, ny, i, nin, lo, hi
	string	skyframe, normframe, in, out, img, delim, junk

	tmpimg = mktemp ("MOS")
	tmptmp = mktemp ("MOS")
	tmpred = mktemp ("MOS")
	tmpexc = mktemp ("tmp$MOS")

	# Get positional parameters
	in  = input
	out = output
	nx  = nxsub
	ny  = nysub

	# Expand input file name list removing the ".imh" extensions.
	sections (in, option="fullname", > tmptmp)
	list1 = tmptmp
	for (nin = 0; fscan (list1, img) != EOF; nin += 1) {
	    i = strlen (img)
	    if (substr (img, i-3, i) == ".imh")
		img = substr (img, 1, i-4)
	    print (img, >> tmpimg)
	    print (img // ".red", >> tmpred)
	}
	list1 = ""; delete (tmptmp, ver-, >& "dev$null")
 
	# Expand the range of images to skip.
	if (skysubtract && sky != "") {

	    skyframe = sky
	    imarith ("@"//tmpimg, "-", skyframe, "@"//tmpred, title="",
	        divzero=0., hparams="", pixtype="", calctype="", verbose+,
		noact-, >> logfile)

	} else if (skysubtract) {

	    print (exclude, ",") | translit ("", "^-,0-9", del+) |
	        translit ("", "-", "!", del-) | tokens (new-) |
	        translit ("", "\n,", " \n", del-, > tmpexc)

	    type (tmpexc, >> logfile)

	    list1 = tmpexc
	    while (fscan (list1, lo, delim, hi, junk) != EOF) {
		if (nscan() == 0)
		    next
	        else if (nscan() == 1 && lo >= 1)
		    print (lo, >> tmptmp)
	        else if (nscan() == 3) {
		    lo = min (max (lo, 1), nin); hi = min (max (hi, 1), nin)
		    for (i = lo; i <= hi; i += 1)
		        print (i, >> tmptmp)
	        }
	    }
	    list1 = ""; delete (tmpexc, ver-, >& "dev$null")

	    if (access (tmptmp)) {
	        sort (tmptmp, col=0, ign+, num+, rev-) | unique (> tmpexc)
	        delete (tmptmp, ver-, >& "dev$null")

	        list1 = tmpimg; list2 = tmpexc; junk = fscan (list2, nin)
	        for (i = 1; fscan (list1, img) != EOF; i += 1) {
		    if (i == nin) {
		        junk = fscan (list2, nin)
		        next
		    }
		    print (img, >> tmptmp)
	        }
	        list1 = ""; list2 = ""; delete (tmpexc, ver-, >& "dev$null")
	    } else
	        tmptmp = tmpimg
 
	    skyframe = out // ".sky"

	    imcombine ("@"//tmptmp, skyframe, rejmask="", plfile="", sigma="",
	        logfile=logfile, combine="median", reject="none", project=no,
		outtype="real", offsets="none", masktype="none", maskvalue=0.0,
		blank=-1.0, scale="exposure", zero="none", weight="exposure",
		statsec="", expname=expname, lthreshold=INDEF,
		hthreshold=INDEF, nlow=1, nhigh=1, nkeep=1, mclip=yes,
		lsigma=3.0, hsigma=3.0, rdnoise="0.0", gain="1.0", snoise="0.0",
		sigscale=0.1, pclip=-0.5, grow=0)
	    print ("\n", >> logfile)
	    imarith ("@"//tmpimg, "-", skyframe, "@"//tmpred, title="",
	        divzero=0., hparams="", pixtype="", calctype="", verbose+,
		noact-, >> logfile)

	} else {

	    skyframe = ""
	    imcopy ("@"//tmpimg, "@"//tmpred, verbose-)
	}

	if (flatten) {
	    if (flat != "") {
	        print ("\n", >> logfile)
	        flatten ("@"//tmpred, flat, minflat=INDEF, pixtype="",
		    keeplog=yes, logfile=logfile)
	    } else if (skyframe != "")  {
	        print ("\n", >> logfile)
		normframe = out // ".norm"
		imcopy (skyframe, normframe, verbose-)
		bscale (normframe, normframe, bzero="0.0", bscale="mode",
		    section="", step=10, lower=INDEF, upper=INDEF,
		    verbose+, >>logfile)
	        print ("\n", >> logfile)
	        flatten ("@"//tmpred, normframe, minflat=INDEF, pixtype="",
		    keeplog=yes, logfile=logfile)
	    }
	}

	if (transpose) {
	    print ("\nTRANSPOSE: Transpose images", >> logfile)
	    time (, >> logfile)
	    imtrans ("@"//tmpred, "@"//tmpred)
	    time (, >> logfile)
	    print ("TRANSPOSE: done", >> logfile)
	}

	print ("\nIRMOSAIC: Mosaic images", >> logfile)
	time (, >> logfile)
	irmosaic ("@"//tmpred, out, "db"//out, nx, ny,
	    trim_section=trim_section, null_input="", corner=corner,
	    direction=direction, raster=raster, nxover=-1, nyover=-1,
	    nimcols=INDEF, nimrows=INDEF, oval=oval,
	    median_section=median_section, sub=subtract, opixtype="r",
	    verbose+, >> logfile)
	time (, >> logfile)
	print ("IRMOSAIC: done", >> logfile)
 
	if (delete) {
	    if (access (tmpred))
		imdelete ("@"//tmpred, ver-, >& "dev$null")
	}

	delete (tmpimg, ver-, >& "dev$null")
	delete (tmptmp, ver-, >& "dev$null")
	delete (tmpred, ver-, >& "dev$null")
end
