# APTEST - Self testing procedure for the APPHOT package.

procedure aptest (imname)

string 	imname		{prompt="Name of the output test image"}
string	aplogfile	{"", prompt="Name of the output log file"}
string	applotfile	{"", prompt="Name of the output plot file"}

begin
	# Declare local variables.
	string	im, aplog, applot, apcoords

	# Check that the user truly wants to proceed.
	s1 = ""
	print ("")
	print ("APTEST INITIALIZES THE APPHOT TASK PARAMETERS")
	print ("TYPE q or Q TO QUIT, ANY OTHER KEY TO PROCEED")
	if (scan (s1) != EOF) {
	    if (s1 == "q" || s1 == "Q") {
		print ("TERMINATING THE APTEST TASK")
		bye
	    }
	}
	print ("")

	# Define some local variables.
	im = imname
	aplog = aplogfile
	if (aplog == "") {
	    aplog = im // ".log"
	}
	applot = applotfile
	if (applot == "") {
	    applot = im // ".plot"
	}

	# Read in the fits file and check for existance of the log and
	# plot files.
	if (! access (im // ".imh") && ! access (im // ".hhh")) {
	    rfits ("apphot$test/fits3.fits", "0", im, make_image=yes,
		    long_header=no, short_header=yes, datatype="",blank=0,
		    scale=yes,oldirafname=no,offset=0, >& "dev$null")
	} else {
	    error (0, "Error: The image already exists on disk")
	}
	if (access (aplog)) {
	    error (0, "Error: The log file already exists on disk")
	}
	if (access (applot)) {
	    error (0, "Error: The plot file already exists on disk")
	}

	# Initialize the APPHOT package.
	print ("INITIALIZE THE APPHOT PACKAGE", >> aplog)
	print ("", >> aplog)
	print ("")
	print ("INITIALIZE THE APPHOT PACKAGE")
	print ("")

	unlearn ("txdump")
	unlearn ("center")
	unlearn ("centerpars")
	unlearn ("daofind")
	unlearn ("datapars")
	unlearn ("findpars")
	unlearn ("fitpsf")
	unlearn ("fitsky")
	unlearn ("fitskypars")
	unlearn ("phot")
	unlearn ("photpars")
	unlearn ("polymark")
	unlearn ("polyphot")
	unlearn ("polypars")
	unlearn ("qphot")
	unlearn ("radprof")
	unlearn ("wphot")

	# Test the DAOFIND task.

	print ("TESTING THE DAOFIND TASK", >> aplog) 
	print ("TESTING THE DAOFIND TASK") 
	print ("", >> aplog)

	datapars.fwhmpsf=2.354820
	datapars.sigma=10.0
	findpars.threshold=3.0

	apcoords = im // ".coo.1"
	daofind (im, output=apcoords, interactive-, verify-)
	concat (apcoords, aplog, append=yes)

	# Test the CENTER task.

	print ("", >> aplog)
	print ("TESTING THE CENTER TASK", >> aplog) 
	print ("TESTING THE CENTER TASK") 
	print ("", >> aplog)

	center (im, coords=apcoords, interactive-, verify-)
	concat (im // ".ctr.1", aplog, append=yes)
	delete (im // ".ctr.1", ver-, >& "dev$null")

	# Test the FITSKY task.

	print ("", >> aplog)
	print ("TESTING THE FITSKY TASK", >> aplog) 
	print ("TESTING THE FITSKY TASK") 
	print ("", >> aplog)

	fitskypars.annulus=6.0
	fitskypars.dannulus=7.0

	fitsky (im, coords=apcoords, interactive-, verify-)
	concat (im // ".sky.1", aplog, append=yes)
	delete (im // ".sky.1", ver-, >& "dev$null")

	# Test the QPHOT task.

	print ("", >> aplog)
	print ("TESTING THE QPHOT TASK", >> aplog) 
	print ("TESTING THE QPHOT TASK") 
	print ("", >> aplog)

	qphot (im, 5.0, 6.0, 7.0, "3.0,5.0", coords=apcoords, interactive-)
	concat (im // ".mag.1", aplog, append=yes)
	delete (im // ".mag.1", ver-, >& "dev$null")

	# Test the PHOT task.

	print ("", >> aplog)
	print ("TESTING THE PHOT TASK", >> aplog) 
	print ("TESTING THE PHOT TASK") 
	print ("", >> aplog)

	photpars.apertures="3.0,5.0"

	phot (im, coords=apcoords, interactive-, verify-)
	concat (im // ".mag.1", aplog, append=yes)
	delete (im // ".mag.1", ver-, >& "dev$null")

	# Test the WPHOT task.

	print ("", >> aplog)
	print ("TESTING THE WPHOT TASK", >> aplog) 
	print ("TESTING THE WPHOT TASK") 
	print ("", >> aplog)

	photpars.weighting="gauss"

	wphot (im, coords=apcoords, interactive-, verify-)
	concat (im // ".omag.1", aplog, append=yes)
	delete (im // ".omag.1", ver-, >& "dev$null")

	# Test the POLYPHOT task.

	print ("", >> aplog)
	print ("TESTING THE POLYPHOT TASK", >> aplog) 
	print ("TESTING THE POLYPHOT TASK")
	print ("", >> aplog)
	print ("COPY OF THE POLYGONS FILE", >> aplog) 
	print ("", >> aplog)
	concat ("apphot$test/polygons.dat", aplog, append=yes)
	print ("", >> aplog)

	polyphot (im, coords=apcoords, polygons="apphot$test/polygons.dat",
	    interactive-, verify-)
	concat (im // ".ply.1", aplog, append=yes)
	delete (im // ".ply.1", ver-, >& "dev$null")

	# Test the RADPROF task.

	print ("", >> aplog)
	print ("TESTING THE RADPROF TASK", >> aplog) 
	print ("TESTING THE RADPROF TASK") 
	print ("", >> aplog)

	radprof (im, 8.0, 0.25, coords=apcoords, output="default",
	    plotfile=applot, order=4, nreject=3, kreject=3.0, interactive-,
	    verify-)
	concat (im // ".prf.1", aplog, append=yes)
	delete (im // ".prf.1", ver-, >& "dev$null")

	# Test the FITPSF task.

	print ("", >> aplog)
	print ("TESTING THE FITPSF TASK", >> aplog) 
	print ("TESTING THE FITPSF TASK") 
	print ("", >> aplog)

	fitpsf (im, 7.0, coords=apcoords, interactive-, verify-)
	concat (im // ".psf.1", aplog, append=yes)
	delete (im // ".psf.1", ver-, >& "dev$null")

	print ("", >> aplog)
	print ("APPHOT PACKAGE TESTS COMPLETED", >> aplog) 
	print ("")
	print ("APPHOT PACKAGE TESTS COMPLETED") 
	print ("", >> aplog)

	# Clean up.
	delete (apcoords, ver-, >& "dev$null")
	unlearn ("txdump")
	unlearn ("center")
	unlearn ("centerpars")
	unlearn ("daofind")
	unlearn ("datapars")
	unlearn ("findpars")
	unlearn ("fitpsf")
	unlearn ("fitsky")
	unlearn ("fitskypars")
	unlearn ("phot")
	unlearn ("photpars")
	unlearn ("polymark")
	unlearn ("polyphot")
	unlearn ("polypars")
	unlearn ("qphot")
	unlearn ("radprof")
	unlearn ("wphot")
	bye
end
