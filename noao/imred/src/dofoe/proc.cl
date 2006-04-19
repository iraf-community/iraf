# PROC -- Process echelle fiber spectra
# This program combines the operations of extraction, flat fielding, and
# dispersion correction in as simple and noninteractive way as possible.
# It supports a second simultaneous arc fiber.  The data must all share
# the same position on the 2D image and the same dispersion solution
# apart from small instrumental changes which can be tracked
# automatically.  The apertures must be identified sequentially and must
# be properly paired if a arc fiber is used.
# 
# If every needed on could add sky subtraction (with a sky fiber) and
# fluxing following the model of the multifiber packages.

procedure proc (objects, apref, flat, arcs, arctable, naps, objaps, arcaps,
	objbeams, arcbeams, fitflat, recenter, scattered, edit, trace, arcap,
	clean, dispcor, splot, redo, update, batch, listonly)

string	objects			{prompt="List of object spectra"}

file	apref			{prompt="Aperture reference spectrum"}
file	flat			{prompt="Flat field spectrum"}
string	arcs			{prompt="List of arc spectra"}
file	arctable		{prompt="Arc assignment table (optional)\n"}

int	naps			{prompt="Number of apertures"}
string	objaps			{prompt="Object apertures"}
string	arcaps			{prompt="Arc apertures"}
string	objbeams		{prompt="Object beam numbers"}
string	arcbeams		{prompt="Arc beam numbers\n"}

bool	fitflat			{prompt="Fit and ratio flat field spectrum?"}
bool	recenter		{prompt="Recenter object apertures?"}
bool	scattered		{prompt="Subtract scattered light?"}
bool	edit			{prompt="Edit/review object apertures?"}
bool	trace			{prompt="Trace object spectra?"}
bool	arcap			{prompt="Use object apertures for arcs?"}
bool	clean			{prompt="Detect and replace bad pixels?"}
bool	dispcor			{prompt="Dispersion correct spectra?"}
bool	splot			{prompt="Plot the final spectrum?"}
bool	redo			{prompt="Redo operations if previously done?"}
bool	update			{prompt="Update spectra if cal data changes?"}
bool	batch			{prompt="Extract objects in batch?"}
bool	listonly		{prompt="List steps but don't process?\n"}

real	datamax = INDEF		{prompt="Max data value / cosmic ray threshold"}

bool	newaps, newresp, newdisp, newarcs, dobatch

string	anssplot = "yes"	{prompt="Splot spectrum?", mode="q",
				 enum="no|yes|NO|YES"}

struct	*fd1, *fd2

begin
	string	imtype, ectype
	string	arcref, spec, arc
	string	arcrefec, specec, arcec, response
	string	temp1, temp2, done
	string	str1, objs, arcrefs, log1, log2
	bool	reextract, extract, scat, disp, disperr, log
	bool	splot1, splot2
	int	i, j, n, nspec
	struct	err

	# Call a separate task to do the listing to minimize the size of
	# this script and improve it's readability.

	dobatch = no
	if (listonly) {
	    listonly (objects, apref, flat, arcs, scattered, dispcor,
		redo, update)
	    bye
	}

	imtype = "." // envget ("imtype")
	i = stridx (",", imtype)
	if (i > 0)
	    imtype = substr (imtype, 1, i-1)
	ectype = ".ec" // imtype
	n = strlen (imtype)

	# Get query parameter.
	objs = objects
	if (arctable == "")
	    arcrefs = arcs
	else
	    arcrefs = arctable
	arcref = ""

	# Temporary files used repeatedly in this script.  Under some
	# abort circumstances these files may be left behind.

	temp1 = mktemp ("tmp$iraf")
	temp2 = mktemp ("tmp$iraf")
	done = mktemp ("tmp$iraf")

	# Rather than always have switches on the logfile and verbose flags
	# we use TEE and set a file to "dev$null" if output is not desired.
	# We must check for the null string to signify no logfile.

	tee.append = yes
	if (logfile == "")
	    log1 = "dev$null"
	else
	    log1 = logfile
	if (verbose)
	    log2 = "STDOUT"
	else
	    log2 = "dev$null"

	# If the update switch is used changes in the calibration data
	# can cause images to be reprocessed (if they are in the object
	# list).  Possible changes are in the aperture definitions,
	# response function, dispersion solution, and sensitivity
	# function.  The newarcs flag is used to only go through the arc
	# image headers once setting the reference spectrum, airmass, and
	# UT.

	newaps = no
	newresp = no
	newdisp = no
	newarcs = yes

	# Check if there are aperture definitions in the database and
	# define them if needed.  This is usually somewhat interactive.
	# Delete the database entry to start fresh if we enter this
	# because of a redo.  Set the newaps flag in case an update is
	# desired.

	i = strlen (apref)
	if (i > n && substr (apref, i-n+1, i) == imtype)
	    apref = substr (apref, 1, i-n)

	# Initialize
	apscript.saturation = INDEF
	apscript.references = apref
	apscript.profiles = ""
	apscript.nfind = naps
	apscript.clean = clean
	if (splot) {
	    splot1 = yes
	    splot2 = yes
	} else {
	    splot1 = no
	    splot2 = no
	}

	reextract = redo
	if (reextract || !access (database // "/ap" // apref)) {
	    if (!access (apref // imtype)) {
		printf ("Aperture reference spectrum not found - %s%s\n",
		    apref, imtype) | scan (err)
		error (1, err // "\nCheck setting of imtype")
	    }
	    print ("Set reference apertures for ", apref) | tee (log1)
	    if (access (database // "/ap" // apref))
		delete (database // "/ap" // apref, verify=no)
	    apscript.ansresize = "yes"
	    apscript.ansedit = "YES"
	    apscript.ansfittrace = "yes"
	    apscript (apref, references="", ansfind="YES", ansrecenter="NO",
		anstrace="YES", ansextract="NO")
	    newaps = yes
	}

	if (recenter)
	    apscript.ansrecenter = "YES"
	else
	    apscript.ansrecenter = "NO"
	apscript.ansresize = "NO"
	if (edit)
	    apscript.ansedit = "yes"
	else
	    apscript.ansedit = "NO"
	if (trace)
	    apscript.anstrace = "YES"
	else
	    apscript.anstrace = "NO"
	apscript.ansfittrace = "NO"
	apscript.ansextract = "YES"
	apscript.ansreview = "NO"

	# The next step is to setup the scattered light correction if needed.
	# We use the flat field image for the interactive setting unless
	# one is not used an then we use the aperture reference.
	# If these images have been  scattered light corrected we assume the
	# scattered light functions parameters are correctly set.

	i = strlen (flat)
	if (i > n && substr (flat, i-n+1, i) == imtype)
	    flat = substr (flat, 1, i-n)

	if (flat != "")
	    spec = flat
	else
	    spec = apref

	scat = no
	if (scattered) {
	    hselect (spec, "apscatte", yes, > temp1)
	    fd1 = temp1
	    if (fscan (fd1, str1) < 1)
		scat = yes
	    fd1 = ""; delete (temp1, verify=no)
	}
	if (scat) {
	    print ("Subtract scattered light in ", spec) | tee (log1)
	    apscript.ansfitscatter = "yes"
	    apscript.ansfitsmooth = "yes"
	    apscript (spec, output="", ansextract="NO", ansscat="YES",
		anssmooth="YES")
	    apscript.ansfitscatter = "NO"
	    apscript.ansfitsmooth = "NO"
	}

	response = ""
	if (flat != "") {
	    response = flat // "norm.ec"
	    reextract = redo || (update && newaps)
	    if (reextract || !access (response // imtype) || (update && scat)) {
	        print ("Create response function ", response) | tee (log1)

	        if (access (response // imtype))
		    imdelete (response, verify=no)
	        if (access (flat //ectype))
		    imdelete (flat//ectype, verify=no)

	        response (flat, apref, response, recenter=recenter,
		    edit=edit, trace=trace, clean=clean, fitflat=fitflat,
		    interactive=params.f_interactive,
		    function=params.f_function, order=params.f_order)

	        newresp = yes
	    }
	}

	# If not dispersion correcting we can go directly to extracting
	# the object spectra.  The reference arcs are the first on
	# the arc lists.  The processing of the reference arcs is done
	# by the task ARCREFS.

	if (dispcor) {
	    hselect (arcs, "$I", yes, >temp1)
	    fd1 = temp1
	    i = fscan (fd1, arcref)
	    if (i < 1)
		error (1, "No reference arcs")
	    fd1 = ""; delete (temp1, verify=no)
	    i = strlen (arcref)
	    if (i > n && substr (arcref, i-n+1, i) == imtype)
	        arcref = substr (arcref, 1, i-n)
	    if (!access (arcref // imtype)) {
		printf ("Arc reference spectrum not found - %s%s\n",
		    arcref, imtype) | scan (err)
		error (1, err // "\nCheck setting of imtype")
	    }
	    arcrefec = arcref // ectype
	    reextract = redo || (update && newaps)
	    if (reextract && access (arcrefec))
	        imdelete (arcrefec, verify=no)

	    arcrefs (arcref, arcaps, arcbeams, response, done, log1, log2)
	}

	# Now we are ready to process the object spectra.

	reextract = redo || (update && (newaps || newresp || newdisp))
	hselect (objs, "$I", yes, > temp1)
	fd1 = temp1
	while (fscan (fd1, spec) != EOF) {
	    i = strlen (spec)
	    if (i > n && substr (spec, i-n+1, i) == imtype)
		spec = substr (spec, 1, i-n)
	    
	    # Check if previously done; i.e. arc.
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
		printf ("Object spectrum not found - %s%s\n",
		    spec, imtype) | scan (err)
		print (err) | tee (log1)
		print ("Check setting of imtype")
		next
	    }
	    specec = spec // ectype

	    # Determine required operations from the flags and image header.
	    scat = no
	    extract = no
	    disp = no
	    if (scattered) {
		hselect (spec, "apscatte", yes, > temp2)
		fd2 = temp2
		if (fscan (fd2, str1) < 1)
		    scat = yes
		fd2 = ""; delete (temp2, verify=no)
	    }
	    if (reextract || !access (specec) || (update && scat))
		extract = yes
	    else {
		hselect (specec, "dc-flag", yes, > temp2)
		fd2 = temp2
		if (fscan (fd2, str1) == 1) {
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

	    # If fully processed go to the next object.
	    if (!extract && !disp)
		next

	    # If not interactive and the batch flag is set submit rest to batch.
	    if (batch && !splot1 && !splot2 && apscript.ansedit == "NO") {
		fd1 = ""; delete (temp1, verify=no)
		flprcache
		batch.objects = objs
		batch.datamax = datamax
		batch.response = response
		batch.arcs = arcs
		batch.arcref = arcref
		batch.arcrefs = arcrefs
		batch.objaps = objaps
		batch.arcaps = arcaps
		batch.objbeams = objbeams
		batch.arcbeams = arcbeams
		batch.done = done
		batch.logfile = log1
		batch.redo = reextract
		batch.update = update
		batch.scattered = scattered
		batch.arcap = arcap
		batch.dispcor = dispcor
		batch.newaps = newaps
		batch.newresp = newresp
		batch.newdisp = newdisp
		batch.newarcs = newarcs
		dobatch = yes
		return
	    }

	    # Process the spectrum in foreground.
	    if (extract) {
		if (access (specec))
		    imdelete (specec, verify=no)

		if (scat) {
		    print ("Subtract scattered light in ", spec) | tee (log1)
		    apscript (spec, output="", ansextract="NO",
			ansscat="YES", anssmooth="YES")
		}

		print ("Extract object spectrum ", spec) | tee (log1)
		setjd (spec, observatory=observatory, date="date-obs",
		    time="ut", exposure="exptime", jd="jd", hjd="",
		    ljd="ljd", utdate=yes, uttime=yes, listonly=no,
		    >> log1)
	        setairmass (spec, intype="beginning",
		    outtype="effective", exposure="exptime",
		    observatory=observatory, show=no, update=yes,
		    override=yes, >> log1)
		apscript (spec, saturation=datamax)
		if (response != "")
		    imarith (specec, "/", response, specec)
	    }

	    disperr = no
	    if (disp) {
		# Fix arc headers if necessary.
		if (newarcs) {
	    	    sections (arcs, option="fullname", >temp2)
		    setjd ("@"//temp2, observatory=observatory, date="date-obs",
			time="ut", exposure="exptime", jd="jd", hjd="",
			ljd="ljd", utdate=yes, uttime=yes, listonly=no,
			>> log1)
	    	    setairmass ("@"//temp2, intype="beginning",
			outtype="effective", exposure="exptime",
			observatory=observatory, show=no, update=yes,
			override=yes, >> log1)
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

		print ("Assign arc spectra for ", spec) | tee (log1)
		refspectra (spec, references=arcrefs,
		    apertures="", refaps="", ignoreaps=no,
		    select=params.select, sort=params.sort,
		    group=params.group, time=params.time,
		    timewrap=params.timewrap, override=yes, confirm=no,
		    assign=yes, logfiles="STDOUT", verbose=no) |
		    tee (log1, > log2)

		doarcs (spec, response, arcref, arcaps, arcbeams, reextract,
		    arcap, log1, no)

	        hselect (specec, "refspec1", yes, > temp2)
	        fd2 = temp2
	        i = fscan (fd2, arc)
	        fd2 = ""; delete (temp2, verify=no)
		if (i < 1) {
		    print ("No arc reference assigned for ", spec) | tee (log1)
		    disperr = yes
		} else {
	            print ("Dispersion correct ", spec) | tee (log1)
		    dispcor (specec, "", linearize=params.linearize,
			database=database, table=arcref//ectype,
			w1=INDEF, w2=INDEF, dw=INDEF, nw=INDEF,
			log=params.log, samedisp=no, flux=params.flux,
			global=no, ignoreaps=no, confirm=no, listonly=no,
			verbose=verbose, logfile=logfile)
		    hedit (specec, "dc-flag", 0, add=yes, verify=no,
			show=no, update=yes)
		}
	    }

	    if (!disperr && (extract || disp)) {
		if (splot1) {
		    print (specec, ":")
		    str1 = anssplot
		    if (str1 == "NO" || str1 == "YES")
			splot1 = no
		    if (str1 == "no" || str1 == "NO")
			splot2 = no
		    else
			splot2 = yes
		}
		if (splot2)
		    splot (specec)
	    }

	    print (spec, >> done)
	}
	fd1 = ""; delete (temp1, verify=no)

	if (access (done))
	    delete (done, verify=no)
end
