# MKAPFILE -- Create an aperture corrections file suitable for input to the
# PHOTCAL preprocessor tasks MKNOBSFILE, MKOBSFILE, and OBSFILE from a list of
# APPHOT/DAOPHOT text or ST table photometry files.

procedure mkapfile (photfiles, naperts, apercors)

file	photfiles    {prompt="The input list of APPHOT/DAOPHOT databases"}
int	naperts	     {prompt="The number of apertures to extract"}
file	apercors     {prompt="The output aperture corrections file"}
int	smallap	     {1, prompt="The first aperture for the correction"}
int	largeap	     {0, prompt="The last aperture for the correction"}
file	magfile      {"", prompt="The optional output best magnitudes file"}
file	logfile	     {"", prompt="The optional output log file"}
file	plotfile     {"", prompt="The optional output plot file"}
file	obsparams    {"", prompt="The observing parameters file"}
string	obscolumns   {"2 3 4 5", prompt="The observing parameters file format"}
bool	append       {no, prompt="Open log and plot files in append mode"}
real	maglim       {.10, prompt="The maximum permitted magnitude error"}
int	nparams	     {3, prompt="Number of cog model parameters to fit"}
real	swings	     {1.2, prompt="The power law slope of the stellar wings"}
real	pwings       {0.1,
prompt="The fraction of the total power in the stellar wings"}
real	pgauss	     {0.5,
prompt="The fraction of the core power in the gaussian core"}
real	rgescale     {0.9, prompt="The exponential / gaussian radial scales"}
real	xwings       {0.0, prompt="The extinction coefficient"}
bool	interactive  {yes, prompt="Do the fit interactively ?"}
bool	verify	     {no,  prompt="Verify interactive user input ?"}
gcur	gcommands    {"", prompt="The graphics cursor"}
string	graphics     {"stdgraph", prompt="The graphics device"}

begin
	# Declare local variables.

	bool tappend
	int tnaperts, tsmallap, tlargeap, td2, tr2, tpwidth, tpagwidth
	string tfiles, tapercors, tmagfile, tlogfile, tplotfile
	string	tobsparams, tobscolumns, tmplist, tdatafile
	string tainfields, tsinfields, trstr, tmstr, testr, fname

	# Get the parameters and test for the existence of various files.

	tfiles = photfiles

	tnaperts = naperts
	if (tnaperts < 2) {
	    error (0, "Naperts must be > 1")
	}

	tapercors = apercors
	if (access (tapercors)) {
	    error (0,
	    "The output aperture corrections file " // tapercors //
	        " already exists")
	}

	tsmallap = smallap
	tlargeap = largeap
	tsmallap = max (1, min (tsmallap, tnaperts))
	if (tlargeap == 0) {
	    tlargeap = tnaperts
	} else {
	    tlargeap = max (1, min (tlargeap, tnaperts))
	}
	if (tsmallap > tlargeap) {
	    error (0, "Lastap must be > tsmallap")
	}

	tmagfile = magfile
	if (tmagfile != "" && access (tmagfile)) {
	    error (0,
	    "The output magfile file " // tmagfile // " already exists")
	}

	tappend = append
	tlogfile = logfile
	if (tlogfile != "" && ! tappend) {
	    if (access (tlogfile))
	        error (0,
		    "The output logfile " // tlogfile // " already exists")
	}
	tplotfile = plotfile
	if (tplotfile != "" && ! tappend) {
	    if (access (tplotfile))
	        error (0,
		    "The output plotfile " // tplotfile // " already exists")
	}

	tobsparams = obsparams
	if (tobsparams == "") {
	    tobscolumns = ""
	} else {
	    tobscolumns = obscolumns
	    if (! access (tobsparams))
	        error (0,
		    "The obsparmas file " // tobsparams // " does not exist")
	}


	# Change columns named "RAPERT", "MAG" and "MERR" to "RAPERT[1]",
	# "MAG[1]" and "MERR[1]" in any ST tables files. Non-ST format files
	# are skipped.

	tbcrename (tfiles, "RAPERT,MAG,MERR", "RAPERT\[1],MAG\[1],MERR\[1]")

	# Add the image, scale, ifilter, itime and xairmass columns to any
	# files in ST tables format. Non-ST format files are skipped.

	tbkeycol (tfiles, "IMAGE,XAIRMASS")

	# Construct the string describing the fields to be extracted 
	# making sure to specify the correct aperture number. Extract
	# the data and store the results in the temporary file tdatafile.

	tainfields = "IM,XC,YC,IF,IT,XA,OT,RAPERT[1-" // tnaperts // "]" //
	    ",MAG[1-"  // tnaperts // "]" // ",MERR[1-" // tnaperts // "]"

	tsinfields = "IM*,XC*,YC*,IF*,IT*,XA*,OT*"
	td2 = tnaperts / 10
	tr2 = mod (tnaperts, 10)
	for (i = 0; i <= td2; i = i + 1) {
	    if (td2 == 0) {
		trstr = ",RA*\[[1-" // tr2 // "]]"
		tmstr = ",MAG\[[1-" // tr2 // "]]"
		testr = ",MERR\[[1-" // tr2 // "]]"
	    } else if (i == 0) {
		trstr = ",RA*\[[1-9]]"
		tmstr = ",MAG\[[1-9]]"
		testr = ",MERR\[[1-9]]"
	    } else if (i == td2) {
		trstr = trstr // ",RA*\[" // td2 // "[0-" // tr2 // "]]"
		tmstr = tmstr // ",MAG\[" // td2 // "[0-" // tr2 // "]]"
		testr = testr // ",MERR\[" // td2 // "[0-" // tr2 // "]]"
	    } else if (i == 1) {
		trstr = trstr // ",RA*\[[1-" // td2-1 // "][1-9]]"
		tmstr = tmstr // ",MAG\[[1-" // td2-1 // "][1-9]]"
		testr = testr // ",MERR\[[1-" // td2-1 // "][1-9]]"
	    }
	}
	tsinfields = tsinfields // trstr // "," // tmstr // "," // testr 

	# Create input file list.

	tmplist = mktemp ("tmp$")
	files (tfiles, sort=no, > tmplist)

	# Create temporary file names to store the intermediate image list.

	tdatafile = mktemp ("tmp$")
	list = tmplist
	while (fscan (list, fname) != EOF) {
	    istable (fname)
	    if (istable.table) {
		tpagwidth = tbdump.pagwidth
	        #tpwidth = tdump.pwidth.p_max
		tdump.pwidth.p_max = 5000
		tbdump (fname, tsinfields, "yes", cdfile="", pfile="",
		    datafile="STDOUT", rows="-", pagwidth=5000,
		    >> tdatafile)
		#tdump.pwidth.p_max = tpwidth
		tbdump.pagwidth = tpagwidth
	    } else if (istable.text) {
		txdump (fname, tainfields, "yes", headers=no, parameters=yes,
		    >> tdatafile)
	    } else {
		print ("ERROR: Cannot run MKAPFILE on file: " // fname)
	    }
	}

	# Run APFILE.

	tainfields = "1,2,3,4,5,6,7,8," // 8+tnaperts // "," // 8+2*tnaperts
	apfile (tdatafile, tainfields, tnaperts, tapercors,
	    smallap=tsmallap, largeap=tlargeap, magfile=tmagfile, 
	    logfile=tlogfile, plotfile=tplotfile, append=tappend,
	    obsparams=tobsparams,obscolumns=tobscolumns,maglim=maglim,
	    nparams=nparams,swings=swings,pwings=pwings, pgauss=pgauss,
	    rgescale=rgescale, xwings=xwings, interactive=interactive,
	    verify=verify, gcommands=gcommands, graphics=graphics)

	# Delete the temporary files.

	delete (tdatafile, go_ahead+, verify-, default_action+,
	    allversions+, subfiles+, > "dev$null")
	delete (tmplist, go_ahead+, verify-, default_action+,
	    allversions+, subfiles+, > "dev$null")
end
