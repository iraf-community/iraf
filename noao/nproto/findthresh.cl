# FINDTHRESH - estimate the expected random error per pixel (in ADU) of
# the background, given the gain and read noise (in electrons) of a CCD.
#
#	random error in 1 pixel = sqrt (sky*p(N) + r(N)**2) / p(N)
#
# r(N) is the effective read noise (electrons), corrected for N frames
# p(N) is the effective gain (electrons/ADU), corrected for N frames
#
# In our implementation, the `mean' used to estimate the sky may actually
# be any of `mean', `midpt', or `mode' as in the IMSTATISTICS task.


procedure findthresh (data)

real	data				{prompt="Sky level (ADU)"}

string	images		= ""		{prompt="List of images"}
string	section		= "[*,*]"	{prompt="Selected image section"}
string	center		= "mean"	{prompt="Central statistical measure",
					    enum="mean|midpt|mode"}
real	binwidth	= 0.1	{prompt="Bin width of histogram in sigma\n"}

real	gain				{prompt="CCD gain in electrons/ADU"}
real	readnoise			{prompt="CCD read noise in electrons"}
int	nframes		= 1		{prompt="Number of coadded frames",
					    min=1}
string	coaddtype	= "average"	{prompt="Type of coaddition",
					   enum="average|sum"}

bool	verbose		= yes		{prompt="Verbose output?\n"}

string	*list1
string	*list2

begin
	string	img, tmpfile, statsfile
	real	reff, peff, mean, stddev, random

	peff = gain
	reff = readnoise

	if (nframes > 1) {
	    reff *= sqrt (nframes)

	    if (coaddtype == "average")
		peff *= nframes

	    if (verbose) {
		print ("effective gain      = ", peff, " (electrons/ADU)")
		print ("effective readnoise = ", reff, " (electrons)\n")
	    }
	}

	if (images != "" && $nargs == 0) {
	    statsfile = mktemp ("tmp$junk")
	    tmpfile = mktemp ("tmp$junk")
	    sections (images, > tmpfile)

	    list1 = tmpfile
	    while (fscan (list1, img) != EOF) {
		imstatistics (img//section, fields=center//",stddev",
		    lower=INDEF, upper=INDEF, binwidth=binwidth, format-,
		    > statsfile)

		list2 = statsfile
		if (fscan (list2, mean, stddev) != 2) 
		    break
		list2 = ""; delete (statsfile, ver-, >& "dev$null")

		random = sqrt (mean*peff + reff**2) / peff

		# round to three decimal places
		stddev = real (nint (stddev * 1000.)) / 1000.
		random = real (nint (random * 1000.)) / 1000.

		if (verbose) {
		    print ("   sigma (computed) = ", random, " (ADU)")
		    print ("         (measured) = ", stddev, " (ADU)\n")
		} else
		    print (random, "\t", stddev)
	    }

   	    list1 = ""; delete (tmpfile, ver-, >& "dev$null")
	    list2 = ""; delete (statsfile, ver-, >& "dev$null")

	} else {
	    mean = data
	    random = sqrt (mean*peff + reff**2) / peff

	    # round to three decimal places
	    random = real (nint (random * 1000.)) / 1000.

	    if (verbose)
		print ("   sigma (computed) = ", random, " (ADU)")
	    else
		print (random)
	}
end
