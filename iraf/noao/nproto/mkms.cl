# MKMS -- Simple script to make a multispec file from separate 1D spectra.
# The task SCOPY will make a multispec file but it does not handle associated
# arrays such as background and errors.  This task builds on SCOPY by adding
# the associated arrays in the proper format.
# 
# This task does very little error checking.  The various lists are assumed
# to be null (i.e. "") or have the same number of spectra.  The associated
# spectra are also assumed to have matching dispersions with their primary
# spectrum.
# 
# To install MKMS copy the script to your home or other directory.  Load the
# onedspec package.  Add the task with the command "task mkms=home$mkms.cl".
# Note you can replace the home$ with the full or logical path to another
# directory if the script is not in your home directory.  The steps of
# loading onedspec and defining the script task may be done in the login.cl
# or loginuser.cl file for permanent use.

procedure mkms (output, spectra, raw, background, sigma)

file	output		{prompt="Output multispec file"}
string	spectra		{prompt="List of primary spectra"}
string	raw		{prompt="List of raw (secondary) spectra"}
string	background	{prompt="List of background spectra"}
string	sigma		{prompt="List of sigma spectra"}

begin
	file	out, temp1, temp2, temp3, temp4, temp5
	string	in, outlist, bandid
	int	nspec, nbands

	# Temporary files in the current directory.
	temp1 = mktemp ("temp")
	temp2 = mktemp ("temp")
	temp3 = mktemp ("temp")
	temp4 = mktemp ("temp")
	temp5 = mktemp ("temp")

	# Get query parameters once and do a simple check for input.
	out = output
	in = spectra
	if (in == "")
	    error (1, "No primary spectra specified")

	# Load ONEDSPEC if not already loaded.
	if (!defpac ("onedspec"))
	    onedspec

	# Create the primary multispec format from 1D spectra using SCOPY.
	scopy (in, temp1, w1=INDEF, w2=INDEF, apertures="", bands="",
	    beams="", apmodulus=0, format="multispec", renumber=yes,
	    offset=0, clobber=no, merge=no, rebin=yes, verbose=no)
	hedit (temp1, "bandid1", "spectrum", add+, verify-, show-, update+)

	# Determine the number of spectra and initialize the bands accumulators.
	nspec = 1
	hselect (temp1, "naxis2", yes) | scan (nspec)
	nbands = 1
	outlist = temp1

	# Create bands if specified. Don't worry about headers since the
	# header of the primary multispec format will be inherited.

	in = raw
	if (in != "") {
	    imstack (in, temp2, title="*", pixtype="*")
	    outlist = outlist // "," // temp2
	    nbands = nbands + 1
	    printf ("bandid%d\n", nbands) | scan (bandid)
	    hedit (temp1, bandid, "raw", add+, verify-, show-, update+)
	}
	in = background
	if (in != "") {
	    imstack (in, temp3, title="*", pixtype="*")
	    outlist = outlist // "," // temp3
	    nbands = nbands + 1
	    printf ("bandid%d\n", nbands) | scan (bandid)
	    hedit (temp1, bandid, "background", add+, verify-, show-, update+)
	}
	in = sigma
	if (in != "") {
	    imstack (in, temp4, title="*", pixtype="*")
	    outlist = outlist // "," // temp4
	    nbands = nbands + 1
	    printf ("bandid%d\n", nbands) | scan (bandid)
	    hedit (temp1, bandid, "sigma", add+, verify-, show-, update+)
	}

	# Make the final output format.  Adjust dimensions are needed.
	# A multispec file maybe 1D, 2D, or 3D depending on the content.

	if (nbands == 1)
	    imcopy (temp1, out, verbose-)
	else {
	    if (nspec == 1) {
		imrename (temp1, temp5, verbose-)
		imstack (temp5, temp1, title="*", pixtype="*")
		imdelete (temp5, verify-)
	    }
	    imstack (outlist, out, title="*", pixtype="*")
	}

	# Finish up.
	imdelete (outlist, verify-)
end
