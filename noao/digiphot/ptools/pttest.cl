# PTTEST - Self testing procedure for the PTOOLS package.

procedure pttest (rootname)

string	rootname	{prompt="Root name of the output test files"}
string	ptlogfile	{"", prompt="Name of the output log file"}
string	ptplotfile	{"", prompt="Name of the output plot file"}

begin
	# Declare local variables.
	string	root, txtfile1, txtfile2, tblfile1, ptlog, ptplot

	# Check that the user truly wants to proceed.
	s1 = ""
	print ("")
	print ("PTTEST INITIALIZES THE PTOOLS TASK PARAMETERS")
	print ("TYPE 'q' or 'Q' TO QUIT, ANY OTHER KEY TO PROCEED")
	if (scan (s1) != EOF) {
	    if (s1 == "q" || s1 == "Q") {
		print ("TERMINATING THE PTTEST TASK")
		bye
	    }
	}
	print ("")

	# Define the image name and the log and plot file names.
	root = rootname
	ptlog = ptlogfile
	if (ptlog == "") {
	    ptlog = root // ".log"
	}
	ptplot = ptplotfile
	if (ptplot == "") {
	    ptplot = root // ".plot"
	}

	# Read in the FITS file and check for the existance of the log and
	# plot files.

	txtfile1 = root // ".txt.1"
	if (! access (txtfile1)) {
	    copy ("ptools$test/test1.dat", txtfile1, verbose-)
	} else {
	    error (0, "Error: The first test text file already exists on disk")
	}
	txtfile2 = root // ".txt.2"
	if (! access (txtfile2)) {
	    copy ("ptools$test/test2.dat", txtfile2, verbose-)
	} else {
	    error (0, "Error: The second test text file already exists on disk")
	}
	tblfile1 = root // ".tbl.1"
	if (! access (tblfile1)) {
	    ;
	} else {
	    error (0, "Error: The test tables file already exists on disk")
	}
	if (access (ptlog)) {
	    error (0, "Error: The log file already exists on disk")
	}
	if (access (ptplot)) {
	    error (0, "Error: The plot file already exists on disk")
	}

	# Initialize the PTOOLS package.

	print ("INITIALIZE THE PTOOLS PACKAGE", >> ptlog)
	print ("", >> ptlog)
	print ("")
	print ("INITIALIZE THE PTOOLS PACKAGE")
	print ("")

	unlearn ("txconcat")
	unlearn ("txdump")
	unlearn ("txrenumber")
	unlearn ("txselect")
	unlearn ("txsort")
	unlearn ("pconvert")

	unlearn ("tbconcat")
	unlearn ("tbdump")
	unlearn ("tbrenumber")
	unlearn ("tbselect")
	unlearn ("tbsort")
	unlearn ("tbkeycol")
	unlearn ("tbcrename")

	unlearn ("pconcat")
	unlearn ("pdump")
	unlearn ("prenumber")
	unlearn ("pselect")
	unlearn ("psort")
	unlearn ("pexamine")
	unlearn ("xyplot")
	unlearn ("histplot")
	unlearn ("radplot")
	unlearn ("surfplot")
	unlearn ("cntrplot")
	unlearn ("istable")

	# Copy the first test file to the log file.

	print ("COPY THE FIRST TEST FILE TO THE LOG FILE", >> ptlog) 
	print ("COPY THE FIRST TEST FILE TO THE LOG FILE")
	print ("", >> ptlog)

	concatenate (txtfile1, ptlog, append=yes)

	# Testing the TXCONCAT task.

	print ("", >> ptlog)
	print ("TESTING THE TXCONCAT TASK (FIRST TEXT FILE)", >> ptlog) 
	print ("    APPENDING THE FIRST TEXT FILE TO ITSELF", >> ptlog) 
	print ("TESTING THE TXCONCAT TASK (FIRST TEXT FILE)") 
	print ("", >> ptlog)

	txconcat (txtfile1 // "," // txtfile1, root // ".app.1", task="TASK")
	concatenate (root // ".app.1", ptlog, append=yes)
	delete (root // ".app.1", ver-, >& "dev$null")

	# Testing the PCONCAT task.

	print ("", >> ptlog)
	print ("TESTING THE PCONCAT TASK (FIRST TEXT FILE)", >> ptlog) 
	print ("    APPENDING THE FIRST TEXT FILE TO ITSELF", >> ptlog) 
	print ("TESTING THE PCONCAT TASK (FIRST TEXT FILE)") 
	print ("", >> ptlog)

	pconcat (txtfile1 // "," // txtfile1, root // ".app.1", task="TASK")
	concatenate (root // ".app.1", ptlog, append=yes)
	delete (root // ".app.1", ver-, >& "dev$null")

	# Testing the TXDUMP task.

	print ("", >> ptlog)
	print ("TESTING THE TXDUMP TASK (FIRST TEXT FILE)", >> ptlog) 
	print ("DUMPING FIELDS ID, XCENTER, YCENTER, MSKY AND MAG[1]",
	    >> ptlog) 
	print ("TESTING THE TXDUMP TASK (FIRST TEXT FILE)") 
	print ("", >> ptlog)

	txdump (txtfile1, "id,xcenter,ycenter,msky,mag[1]", "yes", headers-,
	    parameters+, >> ptlog)

	# Testing the PDUMP task.

	print ("", >> ptlog)
	print ("TESTING THE PDUMP TASK (FIRST TEXT FILE)", >> ptlog) 
	print ("DUMPING FIELDS ID, XCENTER, YCENTER, MSKY AND MAG[1]",
	    >> ptlog) 
	print ("TESTING THE PDUMP TASK (FIRST TEXT FILE)") 
	print ("", >> ptlog)

	pdump (txtfile1, "id,xcenter,ycenter,msky,mag[1]", yes, headers-,
	    parameters+, >> ptlog)

	# Testing the TXSELECT task.

	print ("", >> ptlog)
	print ("TESTING THE TXSELECT TASK (FIRST TEXT FILE)", >> ptlog) 
	print ("SELECTING RECORDS WITH MAG[1] <= 18.0", >> ptlog) 
	print ("TESTING THE TXSELECT TASK (FIRST TEXT FILE)") 
	print ("", >> ptlog)

	txselect (txtfile1, root // ".sel.1", "mag[1] <= 18.0")
	concatenate (root // ".sel.1", ptlog, append=yes)
	delete (root // ".sel.1", ver-, >& "dev$null")

	# Testing the PSELECT task.

	print ("", >> ptlog)
	print ("TESTING THE PSELECT TASK (FIRST TEXT FILE)", >> ptlog) 
	print ("SELECTING RECORDS WITH MAG[1] <= 18.0", >> ptlog) 
	print ("TESTING THE PSELECT TASK (FIRST TEXT FILE)") 
	print ("", >> ptlog)

	pselect (txtfile1, root // ".sel.1", "mag[1] <= 18.0")
	concatenate (root // ".sel.1", ptlog, append=yes)
	delete (root // ".sel.1", ver-, >& "dev$null")

	# Testing the TXSORT task.

	print ("", >> ptlog)
	print ("TESTING THE TXSORT TASK (FIRST TEXT FILE)", >> ptlog) 
	print ("SORTING ON COLUMN MAG[1]", >> ptlog) 
	print ("TESTING THE TXSORT TASK (FIRST TEXT FILE)") 
	print ("", >> ptlog)

	copy (txtfile1, root // ".srt.1", verbose-)
	txsort (root // ".srt.1", "MAG[1]", ascend+)
	concatenate (root // ".srt.1", ptlog, append=yes)

	# Testing the PSORT task.

	print ("", >> ptlog)
	print ("TESTING THE PSORT TASK (FIRST TEXT FILE)", >> ptlog) 
	print ("SORTING ON COLUMN MAG[1]", >> ptlog) 
	print ("TESTING THE PSORT TASK (FIRST TEXT FILE)") 
	print ("", >> ptlog)

	copy (txtfile1, root // ".srt.2", verbose-)
	psort (root // ".srt.2", "MAG[1]", ascend+)
	concatenate (root // ".srt.1", ptlog, append=yes)

	# Testing the TXRENUMBER task.

	print ("", >> ptlog)
	print ("TESTING THE TXRENUMBER TASK (FIRST TEXT FILE)", >> ptlog) 
	print ("RENUMBERING ON COLUMN ID", >> ptlog) 
	print ("TESTING THE TXRENUMBER TASK (FIRST TEXT FILE)") 
	print ("", >> ptlog)

	txrenumber (root // ".srt.1", idoffset=0, id="ID")
	concatenate (root // ".srt.1", ptlog, append=yes)

	# Testing the PRENUMBER task.

	print ("", >> ptlog)
	print ("TESTING THE PRENUMBER TASK (FIRST TEXT FILE)", >> ptlog) 
	print ("RENUMBERING ON COLUMN ID", >> ptlog) 
	print ("TESTING THE PRENUMBER TASK (FIRST TEXT FILE)") 
	print ("", >> ptlog)

	prenumber (root // ".srt.2", idoffset=0, id="ID")
	concatenate (root // ".srt.2", ptlog, append=yes)

	delete (root // ".srt.1", ver-, >& "dev$null")
	delete (root // ".srt.2", ver-, >& "dev$null")

	# Copy the second test file to the log file.

	print ("")
	print ("", >> ptlog)
	print ("COPY THE SECOND TEST FILE TO THE LOG FILE", >> ptlog) 
	print ("COPY THE SECOND TEST FILE TO THE LOG FILE")
	print ("", >> ptlog)

	concatenate (txtfile2, ptlog, append=yes)

	# Testing the TXDUMP task.

	print ("", >> ptlog)
	print ("TESTING THE TXDUMP TASK (SECOND TEXT FILE)", >> ptlog) 
	print ("DUMPING FIELDS ID, XCENTER, YCENTER, MSKY AND MAG[1]",
	    >> ptlog) 
	print ("TESTING THE TXDUMP TASK (SECOND TEXT FILE)") 
	print ("", >> ptlog)

	txdump (txtfile2, "id,xcenter,ycenter,msky,mag[1]", "yes", headers-,
	    parameters+, >> ptlog)

	# Testing the PDUMP task.

	print ("", >> ptlog)
	print ("TESTING THE PDUMP TASK (SECOND TEXT FILE)", >> ptlog) 
	print ("DUMPING FIELDS ID, XCENTER, YCENTER, MSKY AND MAG[1]",
	    >> ptlog) 
	print ("TESTING THE PDUMP TASK (SECOND TEXT FILE)") 
	print ("", >> ptlog)

	pdump (txtfile2, "id,xcenter,ycenter,msky,mag[1]", yes, headers-,
	    parameters+, >> ptlog)

	# Testing the TXSELECT task.

	print ("", >> ptlog)
	print ("TESTING THE TXSELECT TASK (SECOND TEXT FILE)", >> ptlog) 
	print ("SELECTING RECORDS WITH MAG[1] >= 11.5", >> ptlog) 
	print ("TESTING THE TXSELECT TASK (SECOND TEXT FILE)") 
	print ("", >> ptlog)

	txselect (txtfile2, root // ".sel.1", "mag[1] >= 11.5")
	concatenate (root // ".sel.1", ptlog, append=yes)
	delete (root // ".sel.1", ver-, >& "dev$null")

	# Testing the PSELECT task.

	print ("", >> ptlog)
	print ("TESTING THE PSELECT TASK (SECOND TEXT FILE)", >> ptlog) 
	print ("SELECTING RECORDS WITH MAG[1] >= 11.5", >> ptlog) 
	print ("TESTING THE PSELECT TASK (SECOND TEXT FILE)") 
	print ("", >> ptlog)

	pselect (txtfile2, root // ".sel.1", "mag[1] >= 11.5")
	concatenate (root // ".sel.1", ptlog, append=yes)
	delete (root // ".sel.1", ver-, >& "dev$null")

	# Testing the TXSORT task.

	print ("", >> ptlog)
	print ("TESTING THE TXSORT TASK (SECOND TEXT FILE)", >> ptlog) 
	print ("SORTING ON COLUMN MAG[1]", >> ptlog) 
	print ("TESTING THE TXSORT TASK (SECOND TEXT FILE)") 
	print ("", >> ptlog)

	copy (txtfile2, root // ".srt.1", verbose-)
	txsort (root // ".srt.1", "MAG[1]", ascend+)
	concatenate (root // ".srt.1", ptlog, append=yes)

	# Testing the PSORT task.

	print ("", >> ptlog)
	print ("TESTING THE PSORT TASK (SECOND TEXT FILE)", >> ptlog) 
	print ("SORTING ON COLUMN MAG[1]", >> ptlog) 
	print ("TESTING THE PSORT TASK (SECOND TEXT FILE)") 
	print ("", >> ptlog)

	copy (txtfile2, root // ".srt.2", verbose-)
	psort (root // ".srt.2", "MAG[1]", ascend+)
	concatenate (root // ".srt.1", ptlog, append=yes)

	# Testing the TXRENUMBER task.

	print ("", >> ptlog)
	print ("TESTING THE TXRENUMBER TASK (SECOND TEXT FILE)", >> ptlog) 
	print ("RENUMBERING ON COLUMN ID", >> ptlog) 
	print ("TESTING THE TXRENUMBER TASK (SECOND TEXT FILE)") 
	print ("", >> ptlog)

	txrenumber (root // ".srt.1", id="ID")
	concatenate (root // ".srt.1", ptlog, append=yes)

	# Testing the PRENUMBER task.

	print ("", >> ptlog)
	print ("TESTING THE PRENUMBER TASK (SECOND TEXT FILE)", >> ptlog) 
	print ("RENUMBERING ON COLUMN ID", >> ptlog) 
	print ("TESTING THE PRENUMBER TASK (SECOND TEXT FILE)") 
	print ("", >> ptlog)

	prenumber (root // ".srt.2", id="ID")
	concatenate (root // ".srt.2", ptlog, append=yes)

	delete (root // ".srt.1", ver-, >& "dev$null")
	delete (root // ".srt.2", ver-, >& "dev$null")

	# Testing the PCONVERT task.

	print ("", >> ptlog)
	print ("TESTING THE PCONVERT TASK (FIRST TEXT FILE)", >> ptlog) 
	print ("CREATING TABLE FILE BY CONVERTING ALL COLUMNS", >> ptlog) 
	print ("")
	print ("TESTING THE PCONVERT TASK (FIRST TEXT FILE)") 
	print ("", >> ptlog)

	pconvert (txtfile1, tblfile1, "*", expr+, append-)

	# Check to see if the tables package is loaded.

	if (! defpac ("nttools")) {

	    print ("THE NTTOOLS PACKAGE IS NOT LOADED: TERMINATING PTTEST")

	} else {

	    # Testing the TBCONCAT task.

	    print ("", >> ptlog)
	    print ("TESTING THE TBCONCAT/TBDUMP TASKS", >> ptlog) 
	    print ("APPENDING TABLE FILE TO ITSELF", >> ptlog)
	    print ("DUMPING COLUMNS ID, XCENTER, YCENTER, MKSKY, and MAG[1]",
	        >> ptlog) 
	    print ("TESTING THE TBCONCAT/TBDUMP TASKS") 
	    print ("", >> ptlog)

	    tbconcat (tblfile1 // "," // tblfile1, root // ".app.1",
	        task="TASK")
	    tbdump (root // ".app.1", "ID,XCENTER,YCENTER,MSKY,MAG\[1]",
	        "yes", datafile="STDOUT", cdfile="", pfile="", rows="-",
		pagwidth=158, >> ptlog)
	    delete (root // ".app.1", ver-, >& "dev$null")

	    # Testing the PCONCAT task.

	    print ("", >> ptlog)
	    print ("TESTING THE PCONCAT/PDUMP TASKS", >> ptlog) 
	    print ("APPENDING TABLE FILE TO ITSELF", >> ptlog)
	    print ("DUMPING COLUMNS ID, XCENTER, YCENTER, MKSKY, and MAG[1]",
	        >> ptlog) 
	    print ("TESTING THE PCONCAT/PDUMP TASKS") 
	    print ("", >> ptlog)

	    pconcat (tblfile1 // "," // tblfile1, root // ".app.1",
	        task="TASK")
	    pdump (root // ".app.1", "ID,XCENTER,YCENTER,MSKY,MAG\[1]",
		yes, headers-, parameters+, >> ptlog)
	    delete (root // ".app.1", ver-, >& "dev$null")

	    # Testing the TBSELECT task.

	    print ("", >> ptlog)
	    print ("TESTING THE TBSELECT/TBDUMP TASKS", >> ptlog) 
	    print ("SELECTING RECORDS WITH MAG[1] <= 18.0", >> ptlog) 
	    print ("DUMPING COLUMNS ID, XCENTER, YCENTER, MKSKY, and MAG[1]",
	        >> ptlog) 
	    print ("TESTING THE TBSELECT/TBDUMP TASKS") 
	    print ("", >> ptlog)

	    tbselect (tblfile1, root // ".sel.1", "mag[1] <= 18.0")
	    tbdump (root // ".sel.1", "ID,XCENTER,YCENTER,MSKY,MAG\[1]",
	        "yes", datafile="STDOUT", cdfile="", pfile="", rows="-",
		pagwidth=158, >> ptlog)
	    delete (root // ".sel.1", ver-, >& "dev$null")

	    # Testing the PSELECT task.

	    print ("", >> ptlog)
	    print ("TESTING THE PSELECT/PDUMP TASKS", >> ptlog) 
	    print ("SELECTING RECORDS WITH MAG[1] <= 18.0", >> ptlog) 
	    print ("DUMPING COLUMNS ID, XCENTER, YCENTER, MKSKY, and MAG[1]",
	        >> ptlog) 
	    print ("TESTING THE PSELECT/PDUMP TASKS") 
	    print ("", >> ptlog)

	    pselect (tblfile1, root // ".sel.1", "mag[1] <= 18.0")
	    pdump (root // ".sel.1", "ID,XCENTER,YCENTER,MSKY,MAG\[1]",
		yes, headers-, parameters+, >> ptlog)
	    delete (root // ".sel.1", ver-, >& "dev$null")

	    # Testing the TBSORT task.

	    print ("", >> ptlog)
	    print ("TESTING THE TBSORT/TBDUMP TASKS", >> ptlog) 
	    print ("SORTING ON COLUMN MAG[1]", >> ptlog) 
	    print ("DUMPING COLUMNS ID, XCENTER, YCENTER, MKSKY, and MAG[1]",
	        >> ptlog) 
	    print ("TESTING THE TBSORT/TBDUMP TASKS") 
	    print ("", >> ptlog)

	    copy (tblfile1, root // ".srt.1", verbose-)
	    tbsort (root // ".srt.1", "MAG\[1]", ascend+, casesens+)
	    tbdump (root // ".srt.1", "ID,XCENTER,YCENTER,MSKY,MAG\[1]",
	        "yes", datafile="STDOUT", cdfile="", pfile="", rows="-",
		pagwidth=158, >> ptlog)

	    # Testing the PSORT task.

	    print ("", >> ptlog)
	    print ("TESTING THE PSORT/PDUMP TASKS", >> ptlog) 
	    print ("SORTING ON COLUMN MAG[1]", >> ptlog) 
	    print ("DUMPING COLUMNS ID, XCENTER, YCENTER, MKSKY, and MAG[1]",
	        >> ptlog) 
	    print ("TESTING THE PSORT/PDUMP TASK") 
	    print ("", >> ptlog)

	    copy (tblfile1, root // ".srt.2", verbose-)
	    psort (root // ".srt.2", "MAG\[1]", ascend+)
	    pdump (root // ".srt.2", "ID,XCENTER,YCENTER,MSKY,MAG\[1]",
		yes, headers-, parameters+, >> ptlog)

	    # Testing the TBRENUMBER task.

	    print ("", >> ptlog)
	    print ("TESTING THE TBRENUMBER/TBDUMP TASKS", >> ptlog) 
	    print ("RENUMBERING ON COLUMN ID", >> ptlog) 
	    print ("DUMPING COLUMNS ID, XCENTER, YCENTER, MKSKY, and MAG[1]",
	        >> ptlog) 
	    print ("TESTING THE TBRENUMBER/PDUMP TASKS") 
	    print ("", >> ptlog)

	    tbrenumber (root // ".srt.1", idoffset=0, id="ID")
	    tbdump (root // ".srt.1", "ID,XCENTER,YCENTER,MSKY,MAG\[1]",
	        "yes", datafile="STDOUT", cdfile="", pfile="", rows="-",
		pagwidth=158, >> ptlog)

	    # Testing the PRENUMBER task.

	    print ("", >> ptlog)
	    print ("TESTING THE PRENUMBER/PDUMP TASKS", >> ptlog) 
	    print ("RENUMBERING ON COLUMN ID", >> ptlog) 
	    print ("DUMPING COLUMNS ID, XCENTER, YCENTER, MKSKY, and MAG[1]",
	        >> ptlog) 
	    print ("TESTING THE PRENUMBER/PDUMP TASKS") 
	    print ("", >> ptlog)

	    prenumber (root // ".srt.2", id="ID")
	    pdump (root // ".srt.2", "ID,XCENTER,YCENTER,MSKY,MAG\[1]",
		yes, headers-, parameters+, >> ptlog)

	    delete (root // ".srt.1", ver-, >& "dev$null")
	    delete (root // ".srt.2", ver-, >& "dev$null")
	}

	# Testing the PEXAMINE task.

	xyplot.x1=16.0
	xyplot.x2=20.0
	xyplot.y1=0.0
	xyplot.y2=0.5

	histplot.z1=16.0
	histplot.z2=20.0
	histplot.nbins=8

	print ("")
	print ("", >> ptlog)
	print ("TESTING THE PEXAMINE TASK (FIRST TEXT FILE)", >> ptlog) 
	print ("DUMPING THE RESULTS TO THE PLOT FILE", >> ptlog) 
	print ("TESTING THE PEXAMINE TASK (FIRST TEXT FILE)") 
	print ("", >> ptlog)

	pexamine (txtfile1, "", "", gcommands="ptools$test/gcommands.dat",
	    icommands="ptools$test/icommands.dat", >>G ptplot, >& "dev$null")

	print ("", >> ptlog)
	print ("TESTING THE PEXAMINE TASK (TABLE FILE)", >> ptlog) 
	print ("DUMPING THE RESULTS TO THE PLOT FILE", >> ptlog) 
	print ("TESTING THE PEXAMINE TASK (TABLE FILE)") 
	print ("", >> ptlog)

	pexamine (tblfile1, "", "", gcommands="ptools$test/gcommands.dat",
	    icommands="ptools$test/icommands.dat", >>G ptplot, >& "dev$null")

	# Clean up.
	delete (txtfile1, ver-, >& "dev$null")
	delete (txtfile2, ver-, >& "dev$null")
	delete (tblfile1, ver-, >& "dev$null")

	unlearn ("txconcat")
	unlearn ("txdump")
	unlearn ("txrenumber")
	unlearn ("txselect")
	unlearn ("txsort")
	unlearn ("pconvert")

	unlearn ("tbconcat")
	unlearn ("tbdump")
	unlearn ("tbrenumber")
	unlearn ("tbselect")
	unlearn ("tbsort")
	unlearn ("tbkeycol")
	unlearn ("tbcrename")

	unlearn ("pconcat")
	unlearn ("pdump")
	unlearn ("prenumber")
	unlearn ("pselect")
	unlearn ("psort")
	unlearn ("pexamine")

	unlearn ("xyplot")
	unlearn ("histplot")
	unlearn ("radplot")
	unlearn ("surfplot")
	unlearn ("cntrplot")

	unlearn ("istable")

	bye
end
