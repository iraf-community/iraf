# MKOBSFILE -- Create a catalog of observations suitable for input to
# FITPARAMS or the evaluation routines EVALFIT and INVERTFIT from a list of
# APPHOT/DAOPHOT text databases.

procedure mkobsfile (photfiles, idfilters, observations)

file	photfiles    {prompt="The input list of APPHOT/DAOPHOT databases"}
string	idfilters    {prompt="The list of filter ids"}
file	observations {prompt="The output observations file"}
bool    wrap         {yes, prompt="Format the output file for easy reading ?"}
file	imsets       {"STDIN", prompt="The input image set file"}
file	obsparams    {"", prompt="The observing parameters file"}
string	obscolumns   {"2 3 4 5", prompt="The format of obsparams"}
real	minmagerr    {0.001, min=0.0, prompt="The minimum magnitude error"}
file	shifts	     {"STDIN", prompt="The x and y coordinate shifts file"}
file	apercors     {"STDIN", prompt="The aperture corrections file"}
int	aperture     {1,
		     prompt="The aperture number of the extracted magnitude"}
real	tolerance    {5.0,
		     prompt="The tolerance in pixels for position matching"}
bool	allfilters   {no, prompt="Output only objects matched in all filters"}
bool	verify	     {no, prompt="Verify interactive user input ?"}
bool	verbose	     {yes, prompt="Print status, warning and error messages ?"}

begin
	# Declare local variables.
	string tfiles, tidfilters, tobsfile, tformat
	string iimsets, iobsparams, ishifts, iapercors
	string timsets, tobsparams, tobscolumns, tshifts, tapercors
	string tdatafile, tinfields

	# Get the parameters.
	tfiles = photfiles
	tidfilters = idfilters

	tobsfile = observations
	if (access (tobsfile))
	    error (0,
	    "The output catalog file " // tobsfile // " already exists")
	tformat = "f" // tobsfile // ".dat"
	if (access (tformat))
	    delete (tformat, go_ahead+, verify-, default_action+,
	        allversions+, subfiles+)

	iimsets = imsets
	if (iimsets != "") {
	    if (iimsets == "STDIN")
		timsets = mktemp ("tmp$")
	    else if (! access (iimsets))
	        error (0, "The image set file " // timsets // " does not exist")
	    else
		timsets = iimsets
	} else
	    timsets = ""

	iobsparams = obsparams
	if (iobsparams != "") {
	    if (iobsparams == "STDIN") {
		tobsparams = mktemp ("tmp$")
		tobscolumns = "2 3 4 5"
	    } else if (! access (iobsparams)) {
	        error (0, "The obsparams file " // iobsparams //
		    " does not exist")
	    } else {
		tobsparams = iobsparams
		tobscolumns = obscolumns
	    }
	} else {
	    tobsparams = ""
	    tobscolumns = "2 3 4 5"
	}

	ishifts = shifts
	if (ishifts != "") {
	    if (ishifts == "STDIN")
		tshifts = mktemp ("tmp$")
	    else if (! access (ishifts))
		error (0,
		    "The shifts file " // ishifts // " does not exist")
	    else
		tshifts = ishifts
	} else
	    tshifts = ""

	iapercors = apercors
	if (iapercors != "") {
	    if (iapercors == "STDIN")
		tapercors = mktemp ("tmp$")
	    else if (! access (iapercors))
		error (0,
		    "The apercors file " // iapercors // " does not exist")
	    else
		tapercors = iapercors 
	} else
	    tapercors = ""

	# Query the user for input.
	mkphotcors (timsets, tidfilters, tobsparams, tshifts, tapercors,
	    obscolumns=tobscolumns, verify=verify, verbose=verbose)

	# Create temporary file names to store the intermediate image list.
	tdatafile = mktemp ("tmp$")

	# Change columns named "MAG" and "MERR" to "MAG[1]" and "MERR[1]"
	# in any ST tables databases.

	tbcrename (tfiles, "MAG,MERR", "MAG\[1],MERR\[1]")

	# Add the image, ifilter, itime, and xairmass columns to any files
	# in ST tables format.

	tbkeycol (tfiles, "IMAGE,IFILTER,ITIME,XAIRMASS,OTIME")

	# Construct the string describing the fields to be extracted 
	# making sure to specify the correct aperture number. Extract
	# the data, sort on the image name which is in the first column,
	# remove any duplicate records, and store the results in the
	# temporary file tdatafile.

	tinfields = ",IMAGE,XCENTER,YCENTER," //
	      "MAG[" // aperture // "]" // ",MERR[" // aperture // "]," //
	      "MAG\[" // aperture // "]" // ",MERR\[" // aperture // "]," //
	      "IFILTER,XAIRMASS,OTIME,ITIME"

	pdump (tfiles, tinfields, "yes", headers=no, parameters=yes,
	    > tdatafile)

	# Create the output catalog.

	obsfile (tdatafile, "1,2,3,6,9,7,8,4,5,0", tidfilters, timsets,
	    tobsfile, wrap=wrap, obsparams=tobsparams, minmagerr=minmagerr,
	    normtime=no, tolerance=tolerance, allfilters=allfilters,
	    obscolumns="1," // tobscolumns, shifts=tshifts,
	    apercors=tapercors, verify-, verbose=verbose)

	# Delete the temporary files.

	delete (tdatafile, go_ahead+, verify-, default_action+, allversions+,
	    subfiles+)
	if (iimsets == "STDIN")
	    delete (timsets, go_ahead+, verify-, default_action+,
	        allversions+, subfiles+)
	if (iobsparams == "STDIN")
	    delete (tobsparams, go_ahead+, verify-, default_action+,
	        allversions+, subfiles+)
	if (ishifts == "STDIN")
	    delete (tshifts, go_ahead+, verify-, default_action+,
	        allversions+, subfiles+)
	if (iapercors == "STDIN")
	    delete (tapercors, go_ahead+, verify-, default_action+,
	        allversions+, subfiles+)
end
