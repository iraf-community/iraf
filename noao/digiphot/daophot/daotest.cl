# DAOTEST - Self testing procedure for the DAOPHOT package.

procedure daotest (imname)

string 	imname		{prompt="Name of the output test image"}
string	daologfile	{"", prompt="Name of the output log file"}
string	daoplotfile	{"", prompt="Name of the output plot file"}

begin
	# Declare local variables.
	string	im, daolog, daoplot

	# Check that the user truly wants to proceed.
	s1 = ""
	print ("")
	print ("DAOTEST INITIALIZES THE DAOPHOT TASK PARAMETERS")
	print ("TYPE 'q' or 'Q' TO QUIT, ANY OTHER KEY TO PROCEED")
	if (scan (s1) != EOF) {
	    if (s1 == "q" || s1 == "Q") {
		print ("TERMINATING THE DAOTEST TASK")
		bye
	    }
	}
	print ("")

	# Define the image name and the log and plot file names.
	im = imname
	daolog = daologfile
	if (daolog == "") {
	    daolog = im // ".log"
	}
	daoplot = daoplotfile
	if (daoplot == "") {
	    daoplot = im // ".plot"
	}

	# Read in the FITS file and check for the existance of the log and
	# plot files.

	if (! access (im // ".imh") && ! access (im // ".hhh")) {
	    rfits ("daophot$test/fits3.fits", "0", im, make_image=yes,
		    long_header=no, short_header=yes, datatype="",blank=0,
		    scale=yes,oldirafname=no,offset=0, >& "dev$null")
	} else {
	    error (0, "Error: The image already exists on disk")
	}
	if (access (daolog)) {
	    error (0, "Error: The log file already exists on disk")
	}
	if (access (daoplot)) {
	    error (0, "Error: The plot file already exists on disk")
	}

	# Initialize the DAOPHOT package.

	print ("INITIALIZE THE DAOPHOT PACKAGE", >> daolog)
	print ("", >> daolog)
	print ("")
	print ("INITIALIZE THE DAOPHOT PACKAGE")
	print ("")

	daophot.text=yes

	unlearn ("addstar")
	unlearn ("allstar")
	unlearn ("centerpars")
	unlearn ("cntrplot")
	unlearn ("daofind")
	unlearn ("daopars")
	unlearn ("datapars")
	unlearn ("findpars")
	unlearn ("fitskypars")
	unlearn ("group")
	unlearn ("grpselect")
	unlearn ("histplot")
	unlearn ("nstar")
	unlearn ("pconcat")
	unlearn ("pconvert")
	unlearn ("pdump")
	unlearn ("peak")
	unlearn ("pexamine")
	unlearn ("pfmerge")
	unlearn ("phot")
	unlearn ("photpars")
	unlearn ("prenumber")
	unlearn ("pselect")
	unlearn ("psf")
	unlearn ("pstselect")
	unlearn ("psort")
	unlearn ("radplot")
	unlearn ("seepsf")
	unlearn ("substar")
	unlearn ("surfplot")
	unlearn ("xyplot")

	# Test the DAOFIND task.

	print ("TESTING THE DAOFIND TASK", >> daolog) 
	print ("TESTING THE DAOFIND TASK") 
	print ("", >> daolog)

	datapars.fwhmpsf=2.0
	datapars.sigma=10.0
	findpars.threshold=3.0

	daofind (im, "default", interactive-, verify-, verbose-)
	concat (im // ".coo.1", daolog, append=yes)

	# Test the PHOT task.

	print ("", >> daolog)
	print ("TESTING THE PHOT TASK", >> daolog) 
	print ("TESTING THE PHOT TASK") 
	print ("", >> daolog)

	fitskypars.annulus=6.0
	fitskypars.dannulus=12.0
	photpars.apertures="3.0,5.0"

	phot (im, "default", "default", interactive-, verify-, verbose-)
	concat (im // ".mag.1", daolog, append=yes)

	# Test the PSTSELECT task.

	print ("", >> daolog)
	print ("TESTING THE PSTSELECT TASK", >> daolog) 
	print ("TESTING THE PSTSELECT TASK") 
	print ("", >> daolog)

	pstselect (im, im // ".mag.1", "default", 1, verify-, verbose-)
	concat (im // ".pst.1", daolog, append=yes)

	# Test the PSF task.

	print ("", >> daolog)
	print ("TESTING THE PSF TASK", >> daolog) 
	print ("TESTING THE PSF TASK") 
	print ("", >> daolog)

	daopars.psfrad=5.0
	daopars.fitrad=3.0

	psf (im, "default", "", "default", "default", "default",
	    plotfile=daoplot, icommands="daophot$test/cmds.dat", verify-,
	    verbose-, >> daolog)
	imheader (im //".psf.1", longheader+, userfields+, >> daolog)
	print ("", >> daolog)
	concat (im // ".psg.1", daolog, append=yes)
	print ("", >> daolog)
	concat (im // ".pst.2", daolog, append=yes)

	# Test the PEAK task.

	print ("", >> daolog)
	print ("TESTING THE PEAK TASK", >> daolog) 
	print ("TESTING THE PEAK TASK") 
	print ("", >> daolog)

	peak (im, "default", "default", "default", "", verify-, verbose-,
	    >> daolog)
	print ("", >> daolog)
	concat (im // ".pk.1", daolog, append=yes)

	# Test the GROUP task.

	daopars.critsnratio = 0.2

	print ("", >> daolog)
	print ("TESTING THE GROUP TASK", >> daolog) 
	print ("TESTING THE GROUP TASK") 
	print ("", >> daolog)

	group (im, "default", "default", "default", verify-, verbose+,
	    >> daolog)
	print ("", >> daolog)
	concat (im // ".grp.1", daolog, append=yes)

	# Test the GRPSELECT task.

	print ("", >> daolog)
	print ("TESTING THE GRPSELECT TASK", >> daolog) 
	print ("TESTING THE GRPSELECT TASK") 
	print ("", >> daolog)

	grpselect (im // ".grp.1", im // ".grp.2", 1, 1, verbose+, >> daolog)
	print ("", >> daolog)
	concat (im // ".grp.2", daolog, append=yes)
	delete (im // ".grp.2", ver-, >& "dev$null")

	# Test the NSTAR task.

	print ("", >> daolog)
	print ("TESTING THE NSTAR TASK", >> daolog) 
	print ("TESTING THE NSTAR TASK") 
	print ("", >> daolog)

	nstar (im, "default", "default", "default", "", verify-, verbose-,
	    >> daolog)
	print ("", >> daolog)
	concat (im // ".nst.1", daolog, append=yes)

	# Test the ALLSTAR task with cache set to yes.

	print ("", >> daolog)
	print ("TESTING THE ALLSTAR TASK (CACHE=YES)", >> daolog) 
	print ("TESTING THE ALLSTAR TASK (CACHE=YES)") 
	print ("", >> daolog)

	daopars.fitrad=3.0

	allstar (im, "default", "default", "default", "", "default", verify-,
	    verbose-, cache=yes, >> daolog)
	print ("", >> daolog)
	concat (im // ".als.1", daolog, append=yes)
	imdelete (im // ".sub.1", go+, verify-, def+, >& "dev$null")

	# Test the ALLSTAR task with cache set to no.

	print ("", >> daolog)
	print ("TESTING THE ALLSTAR TASK (CACHE=NO)", >> daolog) 
	print ("TESTING THE ALLSTAR TASK (CACHE=NO)") 
	print ("", >> daolog)

	allstar (im, "default", "default", "default", "", "default", verify-,
	    verbose-, cache=no, >> daolog)
	print ("", >> daolog)
	concat (im // ".als.2", daolog, append=yes)
	imdelete (im // ".sub.1", go+, verify-, def+, >& "dev$null")

	# Test the SUBSTAR task.

	print ("", >> daolog)
	print ("TESTING THE SUBSTAR TASK", >> daolog) 
	print ("TESTING THE SUBSTAR TASK") 
	print ("", >> daolog)

	substar (im, "default", "", "default", "default", verify-, verbose-,
	    >> daolog)
	imheader (im //".sub.1", longheader+, userfields+, >> daolog)
	print ("", >> daolog)

	# Test the ADDSTAR task.

	print ("", >> daolog)
	print ("TESTING THE ADDSTAR TASK", >> daolog) 
	print ("TESTING THE ADDSTAR TASK") 
	print ("", >> daolog)

	addstar (im, "", "default", "default", minmag=16.0, maxmag=18.0,
	    nstar=3, verify-, verbose-, >> daolog)
	imheader (im //".add.1", longheader+, userfields+, >> daolog)
	print ("", >> daolog)
	concat (im // ".art.1", daolog, append=yes)
	print ("", >> daolog)

	# Clean up.
	delete (im // ".coo.1", ver-, >& "dev$null")
	delete (im // ".mag.1", ver-, >& "dev$null")
	delete (im // ".pst.1", ver-, >& "dev$null")
	delete (im // ".psg.1", ver-, >& "dev$null")
	delete (im // ".pst.2", ver-, >& "dev$null")
	delete (im // ".pk.1", ver-, >& "dev$null")
	delete (im // ".grp.1", ver-, >& "dev$null")
	delete (im // ".nst.1", ver-, >& "dev$null")
	delete (im // ".als.1", ver-, >& "dev$null")
	delete (im // ".als.2", ver-, >& "dev$null")
	delete (im // ".art.1", ver-, >& "dev$null")

	unlearn ("addstar")
	unlearn ("allstar")
	unlearn ("centerpars")
	unlearn ("cntrplot")
	unlearn ("daofind")
	unlearn ("daopars")
	unlearn ("datapars")
	unlearn ("findpars")
	unlearn ("fitskypars")
	unlearn ("group")
	unlearn ("grpselect")
	unlearn ("histplot")
	unlearn ("nstar")
	unlearn ("pconcat")
	unlearn ("pconvert")
	unlearn ("pdump")
	unlearn ("peak")
	unlearn ("pexamine")
	unlearn ("pfmerge")
	unlearn ("phot")
	unlearn ("photpars")
	unlearn ("prenumber")
	unlearn ("pselect")
	unlearn ("psf")
	unlearn ("pstselect")
	unlearn ("psort")
	unlearn ("radplot")
	unlearn ("seepsf")
	unlearn ("substar")
	unlearn ("surfplot")
	unlearn ("xyplot")

	print ("DAOPHOT PACKAGE TESTS COMPLETED", >> daolog)
	print ("", >> daolog)
	print ("")
	print ("DAOPHOT PACKAGE TESTS COMPLETED")
	print ("")

	bye
end
