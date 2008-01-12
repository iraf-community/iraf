# MKNOBSFILE -- Create a catalog of observations suitable for input to
# FITPARAMS or the evaluation routines EVALFIT and INVERTFIT from a list of
# APPHOT/DAOPHOT text or ST table databases. This script is optimized for
# creating an observations file from a large number of observations of
# single stars. MKNOBSFILE is generally the preprocessor of choice for standard
# star observations.

procedure mknobsfile (photfiles, idfilters, imsets, observations)

file	photfiles    {prompt="The input list of APPHOT/DAOPHOT databases"}
string	idfilters    {prompt="The list of filter ids"}
file	imsets       {prompt="The input image set file"}
file	observations {prompt="The output observations file"}
bool    wrap         {yes, prompt="Format output file for easy reading ?"}
file	obsparams    {"", prompt="The input observing parameters file"}
string	obscolumns   {"2 3 4 5", prompt="The format of obsparams"}
real	minmagerr    {0.001, min=0.0, prompt="The minimum error magnitude"}
file	shifts	     {"", prompt="The input x and y coordinate shifts file"}
file	apercors     {"", prompt="The input aperture corrections file"}
int	aperture     {1,
		     prompt="The aperture number of the extracted magnitude"}
real	tolerance    {5.0,
                     prompt="The tolerance in pixels for position matching"}
bool	allfilters   {no, prompt="Output only objects matched in all filters"}
bool	verify	     {no, prompt="Verify interactive user input ?"}
bool	verbose	     {yes, prompt="Print status, warning and error messages ?"}

begin
	# Declare local variables.
	string tfiles, timsets, tidfilters, tobsfile, tformat, tobscolumns
	string tdatafile, tinfields

	# Get the parameters and test for the existence of various files.

	tfiles = photfiles
	tidfilters = idfilters

	timsets = imsets
	if (! access (timsets))
	    error (0, "The image set file " // timsets // " does not exist")

	tobsfile = observations
	if (access (tobsfile))
	    error (0,
	    "The output catalog file " // tobsfile // " already exists")
	tformat = "f" // tobsfile // ".dat"
	if (access (tformat))
	    delete (tformat, go_ahead+, verify-, default_action+,
	        allversions+, subfiles+, > "dev$null")

	if (obsparams == "") {
	    tobscolumns = ""
	} else {
	    tobscolumns = obscolumns
	    if (! access (obsparams))
	        error (0,
		    "The obsparams file " // obsparams // " does not exist")
	}

	if (shifts != "") {
	    if (! access (shifts))
	        error (0,
		    "The shifts file " // shifts // " does not exist")
	}

	if (apercors != "") {
	    if (! access (apercors))
	        error (0,
		    "The apercors file " // apercors // " does not exist")
	}

	# Create temporary file names to store the intermediate image list.
	tdatafile = mktemp ("tmp$")

	# Change columns named "MAG" and "MERR" to "MAG[1]" and "MERR[1]"
	# in any ST tables files. Non-ST format files are skipped.

	tbcrename (tfiles, "MAG,MERR", "MAG\[1],MERR\[1]")

	# Add the image, ifilter, itime and xairmass columns to any
	# files in ST tables format. Non-ST format files are skipped.

	tbkeycol (tfiles, "IMAGE,IFILTER,ITIME,XAIRMASS,OTIME")

	# Construct the string describing the fields to be extracted 
	# making sure to specify the correct aperture number. Extract
	# the data and store the results in the temporary file tdatafile.

	tinfields = ",IMAGE,XCENTER,YCENTER," //
	    "MAG[" // aperture // "]" // ",MERR[" // aperture // "]," //
	    "MAG\[" // aperture // "]" // ",MERR\[" // aperture // "]," //
	    "IFILTER,XAIRMASS,OTIME,ITIME"
	pdump (tfiles, tinfields, "yes", headers=no, parameters=yes,
	    > tdatafile)

	# Create the observations file.

	obsfile (tdatafile, "1,2,3,6,9,7,8,4,5,0", tidfilters, timsets,
	    tobsfile, wrap=wrap, obsparams=obsparams, minmagerr=minmagerr,
	    normtime=no, tolerance=tolerance, allfilters=allfilters,
	    obscolumns="1," // tobscolumns, shifts=shifts, apercors=apercors,
	    verify=verify, verbose=verbose)

	# Delete the temporary file.

	delete (tdatafile, go_ahead+, verify-, default_action+,
	    allversions+, subfiles+, > "dev$null")
end
