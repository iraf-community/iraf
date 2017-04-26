# ASTTEST - Self testing procedure for the ASTCAT package.

procedure asttest (rootname)

string	rootname	{prompt="Root name of the output test files"}
string	astlogfile	{"", prompt="Name of the output log file"}

struct	*catlist
struct	*svlist

begin
	# Declare local variables.
	string	troot, tastlog, tcatlist, tcatalog, tcatfile
	string	tsvlist, tsurvey, timage, ttemp1
	int tfirst, tlast, tindex

	# Check that the user truly wants to proceed.
	ttemp1 = ""
	print ("")
	print ("ASTTEST initializes the ASTCAT task parameters")
	print ("Type 'q' or 'Q' to quit, any other key to proceed")
	if (scan (ttemp1) != EOF) {
	    if (ttemp1 == "q" || ttemp1 == "Q") {
		print ("Terminating the ASTTEST task")
		bye
	    }
	}
	print ("")

	# Define the plot file name.

	troot = rootname
	tastlog = astlogfile
	if (tastlog == "") {
	    tastlog = troot // ".log"
	}

	# Check for the existence of the test files.

	if (access (tastlog)) {
	    error (0, "Error: The log file already exists on disk")
	}

	# Create some temporary files.

	tcatlist = mktemp (troot)
	tsvlist = mktemp (troot)

	# Initialize the ASTCAT package.

	print ("Initialize the ASTCAT package", >> tastlog)
	print ("", >> tastlog)
	print ("")
	print ("Initialize the ASTCAT package")
	print ("")

	astcat.catalogs = "usno2@noao"
	astcat.catdb = "astcat$lib/catdb.dat"
	astcat.imsurveys = "dss2@cadc"
	astcat.imdb = "astcat$lib/imdb.dat"

	# Unlearning tasks and psets. Ran into a problem with doing
	# unlearn on astcat which I will have to work around.

	# unlearn ("astcat")
	unlearn ("acatpars")
	unlearn ("aclist")
	unlearn ("adumpcat")
	unlearn ("adumpim")
	unlearn ("afiltcat")
	unlearn ("afiltpars")
	unlearn ("agetcat")
	unlearn ("agetim")
	unlearn ("ahedit")
	unlearn ("aimfind")
	unlearn ("aimpars")
	unlearn ("aregpars")
	unlearn ("aslist")
	unlearn ("awcspars")

	# Do the tests.

	# Test the ACLIST task.

	print ("", >> tastlog)
	print ("Testing the ACLIST task", >> tastlog)
	print ("Testing the ACLIST task")
	print ("", >> tastlog)

	aclist ("*", verbose+, catdb=")_.catdb", >> tastlog)
	aclist ("*", verbose-, catdb=")_.catdb", > tcatlist)
	print ("", >> tastlog)

	# Test the ADUMPCAT task.

	print ("", >> tastlog)
	print ("Testing the ADUMPCAT task", >> tastlog)
	print ("Testing the ADUMPCAT task")
	print ("", >> tastlog)

	catlist = tcatlist
	while (fscan (catlist, tcatalog) != EOF) {
	    print ("", >> tastlog)
	    print (tcatalog, >> tastlog)
	    print ("", >> tastlog)
	    print ("    ", tcatalog)
	    if (tcatalog == "tmass@ipac") {
		adumpcat (tcatalog, "STDOUT", ra="00:00:00.0", dec="00:00:00",
		    size="0.17", catdb=")_.catdb", >> tastlog)
	    } else {
		adumpcat (tcatalog, "STDOUT", ra="00:00:00.0", dec="00:00:00",
		    size="10.0", catdb=")_.catdb", >> tastlog)
	    }
	    print ("", >> tastlog)
	}

	# Test the AGETCAT and AFILTCAT tasks.

	print ("", >> tastlog)
	print ("Testing the AGETCAT and AFILTCAT tasks", >> tastlog)
	print ("Testing the AGETCAT and AFILTCAT tasks")
	print ("", >> tastlog)

	catlist = tcatlist 
	while (fscan (catlist, tcatalog) != EOF) {
	    print ("", >> tastlog)
	    print (tcatalog, >> tastlog)
	    print ("", >> tastlog)
	    print ("    ", tcatalog)
	    tfirst = 1
	    tlast = strlen (tcatalog)
	    tindex = stridx ("@", tcatalog)
	    tcatfile = substr (tcatalog, tfirst, tindex - 1) // "." //
		substr (tcatalog, tindex + 1, tlast) // ".cat"
	    if (access (tcatfile)) {
		delete (tcatfile, verify-)
	    }
	    agetcat ("pars", tcatfile, rcra="00:00:00.00", rcdec="+00:00:00.0",
	        rrawidth=20.0, rdecwidth=20.0, catalogs=tcatalog, standard+,
		filter-, update-, verbose+, catdb=")_.catdb", >> tastlog)
	    print ("", >> tastlog)
	    type (tcatfile, map_cc+, device="terminal", >> tastlog)
	    print ("", >> tastlog)
	    afiltcat (tcatfile, tcatfile, catalogs="filename@noao", standard+,
	        filter+, fsort="mag1", update-, verbose+, catdb=")_.catdb",
		>> tastlog)
	    print ("", >> tastlog)
	    type (tcatfile, map_cc+, device="terminal", >> tastlog)
	    print ("", >> tastlog)
	    delete (tcatfile, verify-)
	}

	# Test the ASLIST task.

	print ("", >> tastlog)
	print ("Testing the ASLIST task", >> tastlog)
	print ("Testing the ASLIST task")
	print ("", >> tastlog)

	aslist ("*", verbose+, imdb=")_.imdb", >> tastlog)
	aslist ("*", verbose-, imdb=")_.imdb", > tsvlist)
	print ("", >> tastlog)

	# Test the ADUMPIM task.

	print ("", >> tastlog)
	print ("Testing the ADUMPIM task", >> tastlog)
	print ("Testing the ADUMPIM task")
	print ("", >> tastlog)

	svlist = tsvlist
	while (fscan (svlist, tsurvey) != EOF) {
	    print ("", >> tastlog)
	    print (tsurvey, >> tastlog)
	    print ("", >> tastlog)
	    print ("    ", tsurvey)
	    tfirst = 1
	    tlast = strlen (tsurvey)
	    tindex = stridx ("@", tsurvey)
	    timage = substr (tsurvey, tfirst, tindex - 1) // "." //
		substr (tsurvey, tindex + 1, tlast) // ".fits"
	    if (imaccess (timage)) {
	        imdelete (timage, verify-)
	    }
	    adumpim (tsurvey, timage, ra="14:28:07.0", dec="+34:55:00",
		    size="10.0", imdb=")_.imdb", >> tastlog)
	    printf ("    ")
	    imheader (timage, longheader-, userfields+)
	    print ("", >> tastlog)
	    imheader (timage, longheader+, userfields+, >> tastlog)
	    print ("", >> tastlog)
	    imdelete (timage, verify-)
	}

	# Test the AGETIM and AHEDIT tasks.

	print ("", >> tastlog)
	print ("Testing the AGETIM and AHEDIT tasks", >> tastlog)
	print ("Testing the AGETIM and AHEDIT tasks")
	print ("", >> tastlog)

	svlist = tsvlist
	while (fscan (svlist, tsurvey) != EOF) {
	    print ("", >> tastlog)
	    print (tsurvey, >> tastlog)
	    print ("", >> tastlog)
	    tfirst = 1
	    tlast = strlen (tsurvey)
	    tindex = stridx ("@", tsurvey)
	    timage = substr (tsurvey, tfirst, tindex - 1) // "." //
		substr (tsurvey, tindex + 1, tlast) // ".fits"
	    if (imaccess (timage)) {
	        imdelete (timage, verify-)
	    }
	    agetim ("pars", timage, rcra="14:28:07.00", rcdec="+34:55:00.0",
	        rrawidth=10.0, rdecwidth=10.0, imsurveys=tsurvey, wcsedit-,
		hdredit-, update-, verbose+, imdb=")_.imdb", >> tastlog)
	    printf ("    ")
	    imheader (timage, longheader-, userfields+)
	    print ("", >> tastlog)
	    imheader (timage, longheader+, userfields+, >> tastlog)
	    print ("", >> tastlog)
	    ahedit (timage, tsurvey, hupdate+, wcsedit+, wcs="none", hdredit+,
	        update-, verbose+, imdb=")_.imdb", >> tastlog)
	    print ("", >> tastlog)
	    imheader (timage, longheader+, userfields+, >> tastlog)
	    #imdelete (timage, verify-)
	}

	# Test the AIMFIND task.

	print ("", >> tastlog)
	print ("Testing the AIMFIND task", >> tastlog)
	print ("Testing the AIMFIND task")
	print ("", >> tastlog)

	# Test the aimfind task using the USNO2 survey

	svlist = tsvlist
	while (fscan (svlist, tsurvey) != EOF) {
	    tfirst = 1
	    tlast = strlen (tsurvey)
	    tindex = stridx ("@", tsurvey)
	    timage = substr (tsurvey, tfirst, tindex - 1) // "." //
		substr (tsurvey, tindex + 1, tlast) // ".fits"
	    tcatfile = substr (tsurvey, tfirst, tindex - 1) // "." //
		substr (tsurvey, tindex + 1, tlast) // ".cat"
	    if (access (tcatfile)) {
		delete (tcatfile, verify-)
	    }
	    aimfind (timage, tcatfile, imfile="", catalogs="usno2@noao",
	        standard+, filter-, append-, update-, verbose+,
		catdb=")_.catdb", >> tastlog)
	}

	# Reinitialize the astcat package.

	# unlearn ("astcat")
	unlearn ("acatpars")
	unlearn ("aclist")
	unlearn ("adumpcat")
	unlearn ("adumpim")
	unlearn ("afiltcat")
	unlearn ("afiltpars")
	unlearn ("agetcat")
	unlearn ("agetim")
	unlearn ("ahedit")
	unlearn ("aimfind")
	unlearn ("aimpars")
	unlearn ("aregpars")
	unlearn ("aslist")
	unlearn ("awcspars")

	# Delete some temporary files.

	delete (tsvlist, verify-)
	delete (tcatlist, verify-)
	svlist = ""
	catlist = ""

        print ("", >> tastlog)
        print ("ASTCAT package tests completed", >> tastlog)
        print ("", >> tastlog)
        print ("")
        print ("ASTCAT package tests completed")
        print ("")

	bye
end
