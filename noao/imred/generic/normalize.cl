# NORMALIZE -- Compute the average of a sample region and normalize.

procedure normalize (images)

string	images			{prompt="Images to be normalized"}
real	norm = INDEF		{prompt="Normalization value"}
string	sample_section = "[]"	{prompt="Sample section"}
real	lower = INDEF	{prompt="Lower limit of data values for sampling"}
real	upper = INDEF	{prompt="Upper limit of data values for sampling"}
bool	keeplog = ")_.keeplog"	{prompt="Keep log of processing?"}
file	logfile = ")_.logfile"	{prompt="Log file"}

struct	*imfd

begin
	file	imlist, input, tmp
	real	mean
	int	stat
	bool	mef

	mef = no

	# Query parameters.
	input = images

	# Set temporary files.
	imlist = mktemp ("tmp$ims")
	tmp = mktemp ("tmp")

	# Startup message.
	if (keeplog) {
	    time (>> logfile)
	    print ("  NORMALIZE: Normalize images.", >> logfile)
	}

	# Generate image list.
	sections (input, option="fullname", >imlist)

	# Process list.
	imfd = imlist
	while (fscan (imfd, input) != EOF) {

	    # Determine normalization.
	    if (norm == INDEF) {
	        # Determine the mean of the sample region.
	        imstatistics (input // sample_section, fields="mean",
		    lower=lower, upper=upper, format=no) | scan (mean)
	    } else
		mean = norm

	    # Print output.
	    if (keeplog) {
		time (>> logfile)
	        print ("  Normalization  for ", input, " = ", mean, >> logfile)
	    }

	    if (mean != 0.) {
	        # Normalize the image by the mean.
		if (mef) {
		    imarith (input, "/", mean, tmp, pixtype="real",
			calctype="real")
		    imcopy (tmp, input//"[]", verbose-)
		    imdelete (tmp, verify-)
		} else
		    imarith (input, "/", mean, input, pixtype="real",
			calctype="real")
		hedit (input, "ccdmean", 1., add=yes, verify=no, show=no,
		    update=yes)
	    } else
		print ("  WARNING: Cannot normalize ", input, ".")
	}
	imfd = ""; delete (imlist, verify=no)

	# Ending message.
	if (keeplog) {
	    time (>> logfile)
	    print ("  NORMALIZE: Done.", >> logfile)
	}
end
