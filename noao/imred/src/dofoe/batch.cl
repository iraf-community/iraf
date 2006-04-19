# BATCH -- Process spectra in batch.
# This task is called in batch mode.  It only processes objects
# not previously processed unless the update or redo flags are set.

procedure batch ()

string	objects		{prompt="Object spectra"}
real	datamax 	{prompt="Max data value / cosmic ray threshold"}

file	response	{prompt="Response spectrum"}
string	arcs		{prompt="List of arc spectra"}
file	arcref		{prompt="Arc reference for dispersion solution"}
string	arcrefs		{prompt="Arc references"}

string	objaps		{prompt="Object apertures"}
string	arcaps		{prompt="Arc apertures"}
string	objbeams	{prompt="Object beam numbers"}
string	arcbeams	{prompt="Arc beam numbers\n"}

file	done		{prompt="File of spectra already done"}
file	logfile		{prompt="Logfile"}

bool	redo		{prompt="Redo operations?"}
bool	update		{prompt="Update spectra?"}
bool	scattered	{prompt="Subtract scattered light?"}
bool	arcap		{prompt="Use object apertures for arcs?"}
bool	dispcor		{prompt="Dispersion correct spectra?"}

bool	newaps, newresp, newdisp, newarcs

struct	*fd1, *fd2

begin
	file	temp1, temp2, spec, specec, arc, arcec
	bool	reextract, extract, scat, disp, log
	string	imtype, ectype, str
	int	i, n

	imtype = "." // envget ("imtype")
	i = stridx (",", imtype)
	if (i > 0)
	    imtype = substr (imtype, 1, i-1)
	ectype = ".ec" // imtype
	n = strlen (imtype)

	temp1 = mktemp ("tmp$iraf")
	temp2 = mktemp ("tmp$iraf")

	# Initialize extraction to be noninteractive.
	if (apscript.ansrecenter == "yes")
	    apscript.ansrecenter = "YES"
	else if (apscript.ansrecenter == "no")
	    apscript.ansrecenter = "NO"
	apscript.ansedit = "NO"
	if (apscript.anstrace == "yes") {
	    apscript.anstrace = "YES"
	    apscript.ansfittrace = "NO"
	} else if (apscript.anstrace == "no")
	    apscript.anstrace = "NO"

	reextract = redo || (update && (newaps || newresp || newdisp))

	hselect (objects, "$I", yes, > temp1)
	#sections (objects, option="fullname", > temp1)
	fd1 = temp1
	while (fscan (fd1, spec) != EOF) {
	    i = strlen (spec)
	    if (i > n && substr (spec, i-n+1, i) == imtype)
		spec = substr (spec, 1, i-n)
	    
	    if (access (done)) {
	        fd2 = done
	        while (fscan (fd2, specec) != EOF)
		    if (spec == specec)
		        break
	        if (spec == specec)
		    next
	        fd2 = ""
	    }
	    if (!access (spec // imtype)) {
		printf ("Object spectrum not found - %s%s\nCheck setting of imtype\n", spec, imtype) | tee (log1)
		next
	    }
	    specec = spec // ectype

	    scat = no
	    extract = no
	    disp = no
	    if (scattered) {
		hselect (spec, "apscatte", yes, > temp2)
		fd2 = temp2
		if (fscan (fd2, str) < 1)
		    scat = yes
		fd2 = ""; delete (temp2, verify=no)
	    }
	    if (reextract || !access (specec) || (update && scat))
		extract = yes
	    else {
		hselect (specec, "dc-flag", yes, > temp2)
		fd2 = temp2
		if (fscan (fd2, str) == 1) {
		    extract = update && newdisp
		    if (update && !newdisp)
		        # We really should check if REFSPEC will assign
			# different reference spectra.
			;
		} else
		    disp = dispcor
		fd2 = ""; delete (temp2, verify=no)
	    }
		
	    if (extract)
		disp = dispcor

	    if (extract) {
		if (access (specec))
		    imdelete (specec, verify=no)
		if (scat) {
		    print ("Subtract scattered light in ", spec, >> logfile)
		    apscript (spec, output="", ansextract="NO",
			ansscat="YES", anssmooth="YES", verbose=no)
		}
		print ("Extract object spectrum ", spec, >> logfile)
		setjd (spec, observatory=observatory, date="date-obs",
		    time="ut", exposure="exptime", jd="jd", hjd="",
		    ljd="ljd", utdate=yes, uttime=yes, listonly=no,
		    >> logfile)
	        setairmass (spec, intype="beginning",
		    outtype="effective", exposure="exptime",
		    observatory=observatory, show=no, update=yes,
		    override=yes, >> logfile)
		apscript (spec, saturation=datamax, verbose=no)
		if (response != "")
		    sarith (specec, "/", response, specec, w1=INDEF, w2=INDEF,
			apertures="", bands="", beams="", apmodulus=0,
			reverse=no, ignoreaps=no, format="multispec",
			renumber=no, offset=0, clobber=yes, merge=no,
			errval=0, verbose=no)
	    }

	    if (disp) {
		# Fix arc headers if necessary.
		if (newarcs) {
	    	    sections (arcs, option="fullname", >temp2)
		    setjd ("@"//temp2, observatory=observatory, date="date-obs",
			time="ut", exposure="exptime", jd="jd", hjd="",
			ljd="ljd", utdate=yes, uttime=yes, listonly=no,
			>> logfile)
	    	    setairmass ("@"//temp2, intype="beginning",
			outtype="effective", exposure="exptime",
			observatory=observatory, show=no, update=yes,
			override=yes, >> logfile)
		    delete (temp2, verify=no)
		    hselect (arcs, "$I", yes, >temp2)
	    	    fd2 = temp2
	    	    while (fscan (fd2, arc) != EOF) {
	        	i = strlen (arc)
	        	if (i > n && substr (arc, i-n+1, i) == imtype)
	            	    arc = substr (arc, 1, i-n)
	        	hedit (arc, "refspec1", arc, add=yes, verify=no,
		    	    show=no, update=yes)
	        	hedit (arc, "arctype", "henear", add=yes, verify=no,
		    	    show=no, update=yes)
	    	    }
	    	    fd2 = ""; delete (temp2, verify=no)
		    newarcs = no
		}

		print ("Assign arc spectra for ", spec, >> logfile)
		refspectra (spec, references=arcrefs,
		    apertures="", refaps="", ignoreaps=no,
		    select=params.select, sort=params.sort,
		    group=params.group, time=params.time,
		    timewrap=params.timewrap, override=yes, confirm=no,
		    assign=yes, logfiles="STDOUT", verbose=no,
		    >> logfile)

		doarcs (spec, response, arcref, arcaps, arcbeams, reextract,
		    arcap, logfile, yes)

	        hselect (specec, "refspec1", yes, > temp2)
	        fd2 = temp2
	        i = fscan (fd2, arc)
	        fd2 = ""; delete (temp2, verify=no)
		if (i < 1)
		    print ("No arc reference assigned for ", spec, >> logfile)
		else {
	            print ("Dispersion correct ", spec, >> logfile)
		    dispcor (specec, "", linearize=params.linearize,
			database=database, table=arcref//ectype,
			w1=INDEF, w2=INDEF, dw=INDEF, nw=INDEF,
			log=params.log, samedisp=no, flux=params.flux,
			global=no, ignoreaps=no, confirm=no, listonly=no,
			verbose=no, logfile=logfile)
		    hedit (specec, "dc-flag", 0, add=yes, verify=no,
			show=no, update=yes)
		    disp = no
		}
	    }
	}
	fd1 = ""; delete (temp1, verify=no)

	if (access (done))
	    delete (done, verify=no)

	flprcache (0)
end
