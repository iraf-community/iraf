# SETIMPARS -- Initialize the DAOPHOT task and pset parameters.

procedure setimpars (image, restore, update)

string	image	   	{prompt="Image name"}
bool	restore		{yes, prompt="Restore the last saved parameter set ?"}
bool	update		{yes, prompt="Update the last saved parameter set ?"}
bool	review		{no,
    prompt="Review the parameters before saving ?"}
file	parfile		{"", prompt="Input algorithm parameters file"}
file	datapars	{"", prompt="The input data dependent parameters file"}
file	findpars	{"",
    prompt="The input object detection parameters file"}
file	centerpars	{"", prompt="The input centering parameters file"}
file	fitskypars	{"", prompt="The input sky fitting parameters file"}
file	photpars	{"",
    prompt="The input aperture photometry parameters file"}
file	daopars		{"", prompt="The input psf fitting parameters file"}
bool	unlearn		{no,
    prompt="Unlearn the current algorithm parameters ?"}

begin
	# Define some temporary variables.
	bool	trestore, tupdate
	string  timage, tinparfile, toutparfile, tpars, tfile1, tfile2

	# Read in the image name.
	timage = image
	trestore = restore
	tupdate = update
	print ("Setting parameters for image ", timage, " ...")

	# Set the input image name.
	addstar.image = timage
	allstar.image = timage
	daoedit.image = timage
	daofind.image = timage
	group.image = timage
	nstar.image = timage
	peak.image = timage
	pexamine.image = timage
	phot.image = timage
	psf.image = timage
	pstselect.image = timage
	substar.image = timage

	# Set the input coordinate / sky files back to their defaults.
	phot.skyfile = ""

	# Set the input photometry files back to their defaults.
	addstar.photfile = ""
	    addstar.simple_text = no
	allstar.photfile = "default"
	group.photfile = "default"
	grpselect.ingroupfile = ""
	nstar.groupfile = "default"
	peak.photfile = "default"
	phot.coords = "default"
	psf.photfile = "default"
	psf.pstfile = ""
	pstselect.photfile = "default"
	substar.photfile = "default"

	# Set the psfimage back to the default.
	addstar.psfimage = "default"
	allstar.psfimage = "default"
	group.psfimage = "default"
	nstar.psfimage = "default"
	peak.psfimage = "default"
	seepsf.psfimage = ""
	substar.psfimage = "default"

	# Set the output photometry file names to the default.
	allstar.allstarfile = "default"
	    allstar.rejfile = "default"
	daofind.output = "default"
	group.groupfile = "default"
	grpselect.outgroupfile = ""
	nstar.nstarfile = "default"
	    nstar.rejfile = "default"
	peak.peakfile = "default"
	    peak.rejfile = "default"
	phot.output = "default"
	psf.groupfile = "default"
	psf.opstfile = "default"
	pstselect.pstfile = "default"

	# Set the output images back to the default.
	addstar.addimage = "default"
	allstar.subimage = "default"
	daofind.starmap = ""
	daofind.skymap = ""
	psf.psfimage = "default"
	seepsf.image = ""
	substar.subimage = "default"

	# Set any output plot files back to the default.
	phot.plotfile = ""
	pstselect.plotfile = ""
	psf.plotfile = ""

	# Get the input parameter file name.
	tinparfile = parfile
	if (tinparfile == "") {
	    if (access (timage // ".pars") && trestore)
		tinparfile = timage // ".pars"
	} else if (! access (tinparfile)) {
	    print ("File ", tinparfile, " does not exist ...")
	    return
	}

	# Read in the input parameters.

	if (tinparfile != "") {

	    print ("Reading algorithm parameters from file ",
	        tinparfile, " ...")
	    cl (< tinparfile)

	} else {

	    tpars = datapars
	    if (access (tpars)) {
	        print ("Reading datapars parameters from file ", tpars, " ...")
		tfile1 = mktemp ("tmp$pars")
		tfile2 = mktemp ("tmp$pars")
		dparam (tpars, > tfile1)
		list = tfile1
		while (fscan (list, line) != EOF) {
		    if (substr (line, 1, 5) != "# EOF")
		        print ("datapars.", line, >> tfile2)
		}
		cl (< tfile2)
		delete (tfile1 // "," // tfile2, verify-, default_action+,
		    allversions+, subfiles+, go_ahead+, >& "dev$null")
	    } else if (unlearn) {
	        print ("Reading default datapars parameters from disk ...")
		unlearn ("daophot.datapars")
	    } else {
	        print ("Reading current datapars parameters from disk ...")
	    }

	    tpars = findpars
	    if (access (tpars)) {
	        print ("Reading findpars parameters from file ", tpars, " ...")
		tfile1 = mktemp ("tmp$pars")
		tfile2 = mktemp ("tmp$pars")
		dparam (tpars, > tfile1)
		list = tfile1
		while (fscan (list, line) != EOF) {
		    if (substr (line, 1, 5) != "# EOF")
		        print ("findpars.", line, >> tfile2)
		}
		cl (< tfile2)
		delete (tfile1 // "," // tfile2, verify-, default_action+,
		    allversions+, subfiles+, go_ahead+, >& "dev$null")
	    } else if (unlearn) {
	        print ("Reading default findpars parameters from disk ...")
		unlearn ("daophot.findpars")
	    } else {
	        print ("Reading current findpars parameters from disk ...")
	    }

	    tpars = centerpars
	    if (access (tpars)) {
	        print ("Reading centerpars parameters from file ", tpars,
		    " ...")
		tfile1 = mktemp ("tmp$pars")
		tfile2 = mktemp ("tmp$pars")
		dparam (tpars, > tfile1)
		list = tfile1
		while (fscan (list, line) != EOF) {
		    if (substr (line, 1, 5) != "# EOF")
		        print ("centerpars.", line, >> tfile2)
		}
		cl (< tfile2)
		delete (tfile1 // "," // tfile2, verify-, default_action+,
		    allversions+, subfiles+, go_ahead+, >& "dev$null")
	    } else if (unlearn) {
		unlearn ("daophot.centerpars")
	        print ("Reading default centerpars parameters from disk ...")
	    } else {
	        print ("Reading current centerpars parameters from disk ...")
	    }

	    tpars = fitskypars
	    if (access (tpars)) {
	        print ("Reading fitskypars parameters from file ", tpars,
		    " ...")
		tfile1 = mktemp ("tmp$pars")
		tfile2 = mktemp ("tmp$pars")
		dparam (tpars, > tfile1)
		list = tfile1
		while (fscan (list, line) != EOF) {
		    if (substr (line, 1, 5) != "# EOF")
		        print ("fitskypars.", line, >> tfile2)
		}
		cl (< tfile2)
		delete (tfile1 // "," // tfile2, verify-, default_action+,
		    allversions+, subfiles+, go_ahead+, >& "dev$null")
	    } else if (unlearn) {
		unlearn ("daophot.fitskypars")
	        print ("Reading default fitskypars parameters from disk ...")
	    } else {
	        print ("Reading current fitskypars parameters from disk ...")
	    }

	    tpars = photpars
	    if (access (tpars)) {
	        print ("Reading photpars parameters from file ", tpars,
		    " ...")
		tfile1 = mktemp ("tmp$pars")
		tfile2 = mktemp ("tmp$pars")
		dparam (tpars, > tfile1)
		list = tfile1
		while (fscan (list, line) != EOF) {
		    if (substr (line, 1, 5) != "# EOF")
		        print ("photpars.", line, >> tfile2)
		}
		cl (< tfile2)
		delete (tfile1 // "," // tfile2, verify-, default_action+,
		    allversions+, subfiles+, go_ahead+, >& "dev$null")
	    } else if (unlearn) {
		unlearn ("daophot.photpars")
	        print ("Reading default photpars parameters from disk ...")
	    } else {
	        print ("Reading current photpars parameters from disk ...")
	    }

	    tpars = daopars
	    if (access (tpars)) {
	        print ("Reading psf fitting parameters from file ", tpars,
		    " ...")
		tfile1 = mktemp ("tmp$pars")
		tfile2 = mktemp ("tmp$pars")
		dparam (tpars, > tfile1)
		list = tfile1
		while (fscan (list, line) != EOF) {
		    if (substr (line, 1, 5) != "# EOF")
		        print ("daopars.", line, >> tfile2)
		}
		cl (< tfile2)
		delete (tfile1 // "," // tfile2, verify-, default_action+,
		    allversions+, subfiles+, go_ahead+, >& "dev$null")
	    } else if (unlearn) {
		unlearn ("daophot.daopars")
	        print ("Reading default daopars parameters from disk ...")
	    } else {
	        print ("Reading current daopars parameters from disk ...")
	    }
	}

	# Review the current values for the algorithm parameters.
	if (review) {
	    eparam ("datapars")
	    eparam ("findpars")
	    eparam ("centerpars")
	    eparam ("fitskypars")
	    eparam ("photpars")
	    eparam ("daopars")
	}

	# Update the output parameter file.
	toutparfile = timage // ".pars"
	if (tupdate) {
	    if (access (toutparfile)) {
	        print ("Updating image parameter file ", toutparfile, " ...")
	        delete (toutparfile, verify-, default_action+, allversions+,
	            subfiles+, go_ahead+, >& "dev$null")
	    } else {
	        print ("Creating image parameter file ", toutparfile, " ...")
	    }
	    dparam ("datapars", "findpars", "centerpars", "fitskypars",
	        "photpars", "daopars", > toutparfile)
	}
end
