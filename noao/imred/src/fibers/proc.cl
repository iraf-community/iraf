# PROC -- Process spectra from 2D to wavelength calibrated 1D.
# This program combines the operations of extraction, flat fielding,
# fiber throughput correction, dispersion correction, and sky subtraction
# in as simple and noninteractive way as possible.  Certain assumptions
# are made about the data and the output.  A blank sky image, called a
# sky flat, may be used to determine the instrument throughput.  The data
# must all share the same position on the 2D image and the same
# dispersion solution apart from small instrumental changes which can be
# tracked automatically.

procedure proc (objects, apref, flat, throughput, arcs1, arcs2, arcreplace,
	arctable, fibers, apidtable, crval, cdelt, objaps, skyaps, arcaps,
	objbeams, skybeams, arcbeams, scattered, fitflat, recenter, edit,
	trace, arcap, clean, dispcor, savearcs, skyalign, skysubtract,
	skyedit, saveskys, splot, redo, update, batch, listonly)

string	objects			{prompt="List of object spectra"}

file	apref			{prompt="Aperture reference spectrum"}
file	flat			{prompt="Flat field spectrum"}
file	throughput		{prompt="Throughput file or image (optional)"}
string	arcs1			{prompt="List of arc spectra"}
string	arcs2			{prompt="List of shift arc spectra"}
file	arcreplace		{prompt="Special aperture replacements"}
file	arctable		{prompt="Arc assignment table (optional)\n"}

int	fibers			{prompt="Number of fibers"}
file	apidtable		{prompt="Aperture identifications"}
string	crval = "INDEF"		{prompt="Approximate wavelength"}
string	cdelt = "INDEF"		{prompt="Approximate dispersion"}
string	objaps			{prompt="Object apertures"}
string	skyaps			{prompt="Sky apertures"}
string	arcaps			{prompt="Arc apertures"}
string	objbeams		{prompt="Object beam numbers"}
string	skybeams		{prompt="Sky beam numbers"}
string	arcbeams		{prompt="Arc beam numbers\n"}

bool	scattered		{prompt="Subtract scattered light?"}
bool	fitflat			{prompt="Fit and ratio flat field spectrum?"}
bool	recenter		{prompt="Recenter object apertures?"}
bool	edit			{prompt="Edit/review object apertures?"}
bool	trace			{prompt="Trace object spectra?"}
bool	arcap			{prompt="Use object apertures for arcs?"}
bool	clean			{prompt="Detect and replace bad pixels?"}
bool	dispcor			{prompt="Dispersion correct spectra?"}
bool	savearcs		{prompt="Save internal arcs?"}
bool	skyalign		{prompt="Align sky lines?"}
bool	skysubtract		{prompt="Subtract sky?"}
bool	skyedit			{prompt="Edit the sky spectra?"}
bool	saveskys		{prompt="Save sky spectra?"}
bool	splot			{prompt="Plot the final spectrum?"}
bool	redo			{prompt="Redo operations if previously done?"}
bool	update			{prompt="Update spectra if cal data changes?"}
bool	batch			{prompt="Extract objects in batch?"}
bool	listonly		{prompt="List steps but don't process?\n"}

real	datamax = INDEF		{prompt="Max data value / cosmic ray threshold"}

string	ansskyedit = "yes"	{prompt="Edit the sky spectra?", mode="q"}
bool	newaps, newresp, newdisp, newarcs, dobatch

string	anssplot = "yes"	{prompt="Splot spectrum?", mode="q",
				 enum="no|yes|NO|YES"}

string	extn = ".ms"		{prompt="Extraction extension"}
struct	*fd1, *fd2, *fd3

begin
	string	imtype, mstype
	string	arcref1, arcref2, spec, arc, align=""
	string	arcref1ms, arcref2ms, specms, arcms, response
	string	objs, temp, temp1, done
	string	str1, str2, str3, str4, arcrefs, log1, log2
	bool	scat, reextract, extract, disp, disperr, sky, log
	bool	skyedit1, skyedit2, splot1, splot2
	int	i, j, n, nspec
	struct	err

	# Call a separate task to do the listing to minimize the size of
	# this script and improve it's readability.

	dobatch = no
	if (listonly) {
	    listonly (objects, apidtable, apref, flat, throughput, arcs1, arcs2,
		scattered, dispcor, skysubtract, redo, update)
	    bye
	}

	imtype = "." // envget ("imtype")
	i = stridx (",", imtype)
	if (i > 0)
	    imtype = substr (imtype, 1, i-1)
	mstype = ".ms" // imtype
	n = strlen (imtype)

	# Temporary files used repeatedly in this script.  Under some
	# abort circumstances these files may be left behind.

	objs = mktemp ("tmp$iraf")
	temp = mktemp ("tmp$iraf")
	temp1 = mktemp ("tmp$iraf")
	done = mktemp ("tmp$iraf")

	if (apidtable != "") {
	    i = strlen (apidtable)
	    if (i > n && substr (apidtable, i-n+1, i) == imtype) {
		apidtable = substr (apidtable, 1, i-n)
		i = strlen (apidtable)
	    }
	    for (j=1; j<=i && substr(apidtable,j,j)==" "; j+=1);
	    apidtable = substr (apidtable, j, i)
	}
	i = strlen (apidtable)
	if (i == 0)
	    extn = ".ms"
	else {
	    extn = apidtable
	    while (yes) {
		i = stridx ("/$]", extn)
		if (i == 0)
		    break
		j = strlen (extn)
		extn = substr (extn, i+1, j)
	    }
	    extn = extn // ".ms"
	}

	# Get query parameter.
	getspec (objects, objs)
	if (arctable == "" || arctable == " ") {
	    if (arcs2 == "" || arcs2 == " ")
	        arcrefs = arcs1
	    else
		arcrefs = arcs2
	} else
	    arcrefs = arctable
	arcref1 = ""
	arcref2 = ""

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

	getspec (apref, temp)
	fd1 = temp
	if (fscan (fd1, apref) == EOF)
	    error (1, "No aperture reference")
	fd1 = ""; delete (temp, verify=no)

	# Initialize
	apscript.saturation = INDEF
	apscript.references = apref
	apscript.profiles = ""
	apscript.apidtable = apidtable
	apscript.nfind = fibers
	apscript.clean = clean
	if (splot) {
	    splot1 = yes
	    splot2 = yes
	} else {
	    splot1 = no
	    splot2 = no
	}

	reextract = redo
	if (reextract || !access (database // "/ap" // apref // extn)) {
	    if (!access (apref // imtype)) {
		printf ("Aperture reference spectrum not found - %s%s\n",
		    apref, imtype) | scan (err)
		error (1, err // "\nCheck settting of imtype")
	    }
	    print ("Set reference apertures for ", apref) | tee (log1)
	    if (access (database // "/ap" // apref))
		delete (database // "/ap" // apref, verify=no)
	    if (access (database // "/ap" // apref//extn))
		delete (database // "/ap" // apref // extn, verify=no)
	    apscript.ansresize = "yes"
	    apscript.ansedit = "yes"
	    apscript.ansfittrace = "yes"
	    apscript (apref, references="", ansfind="YES", ansrecenter="NO",
		anstrace="YES", ansextract="NO")
	    newaps = yes
	    copy (database//"/ap"//apref, database//"/ap"//apref//extn,
		verbose=no)
	} else {
	    if (access (database // "/ap" // apref))
		delete (database // "/ap" // apref, verify=no)
	    copy (database//"/ap"//apref//extn, database//"/ap"//apref,
		verbose=no)
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
	if (scattered) {
	    apscript.ansfitscatter = "yes"
	    apscript.ansfitsmooth = "yes"
	}
	apscript.ansfittrace = "NO"
	apscript.ansextract = "YES"
	apscript.ansreview = "NO"
	if (skyedit) {
	    skyedit1 = yes
	    skyedit2 = yes
	} else {
	    skyedit1 = no
	    skyedit2 = no
	}

	# The next step is to setup the scattered light correction if needed.
	# We use the flat field image for the interactive setting unless
	# one is not used and then we use the aperture reference.
	# If these images have been  scattered light corrected we assume the
	# scattered light functions parameters are correctly set.

	i = strlen (flat)
	if (i > n && substr (flat, i-n+1, i) == imtype)
	    flat = substr (flat, 1, i-n)

	if (flat != "")
	    spec = flat
	else
	    spec = apref

	getspec (spec, temp)
	fd1 = temp
	if (fscan (fd1, spec) == EOF)
	    error (1, "No flat field")
	fd1 = ""; delete (temp, verify=no)

	scat = no
	if (scattered) {
	    if (redo && access (spec//"noscat"//imtype)) {
		imdelete (spec, verify=no)
		imrename (spec//"noscat", spec)
	    }
	    hselect (spec, "apscatte", yes) | scan (str1)
	    if (nscan() == 0)
		scat = yes
	}
	if (scat) {
	    print ("Subtract scattered light from ", spec) | tee (log1)
	    #apscript.ansfitscatter = "yes"
	    #apscript.ansfitsmooth = "yes"
	    imrename (spec, spec//"noscat")
	    apscript (spec//"noscat", output=spec, ansextract="NO",
		ansscat="YES", anssmooth="YES")
	    #apscript.ansfitscatter = "NO"
	    #apscript.ansfitsmooth = "NO"
	}

	# The next step is to process the flat field image which is used
	# as a flat field and a throughput correction.

	spec = throughput
	i = strlen (spec)
	if (i > n && substr (spec, i-n+1, i) == imtype)
	    spec = substr (spec, 1, i-n)
	specms = spec // mstype

	if (spec != "" && access (spec//imtype)) {
	    getspec (spec, temp)
	    fd1 = temp
	    if (fscan (fd1, spec) == EOF)
		error (1, "No flat field")
	    fd1 = ""; delete (temp, verify=no)

	    scat = no
	    if (scattered) {
		if (redo && access (spec//"noscat"//imtype)) {
		    imdelete (spec, verify=no)
		    imrename (spec//"noscat", spec)
		}
		hselect (spec, "apscatte", yes) | scan (str1)
		if (nscan() == 0)
		    scat = yes
	    }
	    if (scat) {
		print ("Subtract scattered light from ", spec) | tee (log1)
		imrename (spec, spec//"noscat")
		apscript (spec//"noscat", output=spec, ansextract="NO",
		    ansscat="YES", anssmooth="YES")
	    }
	}

	response = ""
	if (flat != "" || spec != "") {
	    if (extn == ".ms")
		response = flat // spec // "norm.ms"
	    else
		response = flat // spec // extn
	    reextract = redo || (update && newaps)
	    if (reextract || !access (response // imtype) || (redo && scat)) {
	        print ("Create response function ", response) | tee (log1)

	        if (access (response // imtype))
		    imdelete (response, verify=no)
	        if (access (flat //mstype))
		    imdelete (flat//mstype, verify=no)
	        if (access (specms))
		    imdelete (specms, verify=no)

	        fibresponse (flat, spec, apref, response, recenter=recenter,
		    edit=edit, trace=trace, clean=clean,
		    fitflat=fitflat, interactive=params.f_interactive,
		    function=params.f_function, order=params.f_order)

	        newresp = yes
	    }
	}

	# If not dispersion correcting we can go directly to extracting
	# the object spectra.  The reference arcs are the first on
	# the arc lists.  The processing of the reference arcs is done
	# by the task ARCREFS.

	if (dispcor) {
	    getspec (arcs1, temp)
	    fd1 = temp
	    if (fscan (fd1, arcref1) == EOF)
		error (1, "No reference arcs")
	    fd1 = ""; delete (temp, verify=no)
	    if (!access (arcref1 // imtype)) {
		printf ("Arc reference spectrum not found - %s%s\n",
		    arcref1, imtype) | scan (err)
		error (1, err // "\nCheck settting of imtype")
	    }
	    arcref1ms = arcref1 // extn
	    reextract = redo || (update && newaps)
	    if (reextract && access (arcref1ms//imtype))
	        imdelete (arcref1ms, verify=no)

	    getspec (arcs2, temp)
	    fd1 = temp
	    if (fscan (fd1, arcref2) == EOF)
		arcref2 = ""
	    else {
	        if (!access (arcref2 // imtype)) {
		    printf ("Arc reference spectrum not found - %s%s\n",
			arcref2, imtype) | scan (err)
		    error (1, err // "\nCheck settting of imtype")
		}
	        arcref2ms = arcref2 // extn
	        if (reextract && access (arcref2ms//imtype))
	            imdelete (arcref2ms, verify=no)
	    }
	    fd1 = ""; delete (temp, verify=no)

	    arcrefs (arcref1, arcref2, extn, arcreplace, apidtable, response,
		crval, cdelt, done, log1, log2)

	    # Define alignment if needed.
	    if (skyalign) {
		align = "align" // extn // imtype
		if (reextract)
		    imdelete (align, verify=no, >& "dev$null")
	    }
	}

	# Now we are ready to process the object spectra.

	reextract = redo || (update && (newaps || newresp || newdisp))
	fd1 = objs
	while (fscan (fd1, spec) != EOF) {
	    # Check if previously done; i.e. arc.
	    if (access (done)) {
	        fd2 = done
	        while (fscan (fd2, specms) != EOF)
		    if (spec == specms)
		        break
	        if (spec == specms)
		    next
	        fd2 = ""
	    }
	    if (!access (spec // imtype)) {
		printf ("Object spectrum not found - %s%s\n",
		    spec, imtype) | tee (log1)
		print ("Check settting of imtype")
	    }
	    specms = spec // mstype

	    # Determine required operations from the flags and image header.
	    scat = no
	    extract = no
	    disp = no
	    sky = no
	    if (scattered) {
		if (redo && access (spec//"noscat"//imtype)) {
		    imdelete (spec, verify=no)
		    imrename (spec//"noscat", spec)
		}
		hselect (spec, "apscatte", yes) | scan (str1)
		if (nscan() == 0)
		    scat = yes
	    }
	    if (reextract || !access (specms) || (redo && scat))
		extract = yes
	    else {
		hselect (specms, "dclog1", yes) | scan (str1)
		if (nscan () == 1) {
		    extract = update && newdisp
		    if (update && !newdisp)
		        # We really should check if REFSPEC will assign
			# different reference spectra.
			;
		} else
		    disp = dispcor

		hselect (specms, "skysub", yes) | scan (str1)
		if (nscan() == 0)
		    sky = skysubtract
	    }
		
	    if (extract) {
		disp = dispcor
		sky = skysubtract
	    }

	    # If fully processed go to the next object.
	    if (!extract && !disp && !sky)
		next

	    # If not interactive and the batch flag is set submit rest to batch.
	    if (batch && !skyedit1 && !skyedit2 && !splot1 && !splot2 &&
		apscript.ansedit == "NO" && (!skyalign || access (align))) {
		fd1 = ""; delete (objs, verify=no)
		apscript.ansfitscatter = "NO"
		apscript.ansfitsmooth = "NO"

		flprcache
		batch.objects = objects
		batch.datamax = datamax
		batch.response = response
		batch.arcs1 = arcs1
		batch.arcs2 = arcs2
		batch.arcref1 = arcref1
		batch.arcref2 = arcref2
		batch.arcreplace = arcreplace
		batch.apidtable = apidtable
		batch.arcrefs = arcrefs
		batch.extn = extn
		batch.objaps = objaps
		batch.skyaps = skyaps
		batch.arcaps = arcaps
		batch.objbeams = objbeams
		batch.skybeams = skybeams
		batch.arcbeams = arcbeams
		batch.done = done
		batch.logfile = log1
		batch.redo = reextract
		batch.update = update
		batch.scattered = scattered
		batch.arcap = arcap
		batch.dispcor = dispcor
		batch.savearcs = savearcs
		batch.skyalign = skyalign
		batch.skysubtract = skysubtract
		batch.saveskys = saveskys
		batch.newaps = newaps
		batch.newresp = newresp
		batch.newdisp = newdisp
		batch.newarcs = newarcs
		dobatch = yes
		return
	    }

	    # Process the spectrum in foreground.
	    if (extract) {
		if (access (specms))
		    imdelete (specms, verify=no)

		if (scat) {
		    print ("Subtract scattered light from ", spec) | tee (log1)
		    imrename (spec, spec//"noscat")
		    apscript (spec//"noscat", output=spec, ansextract="NO",
			ansscat="YES", anssmooth="YES")
		}

		print ("Extract object spectrum ", spec) | tee (log1)
		hselect (spec, "date-obs,ut,exptime", yes, > temp1)
		hselect (spec, "ra,dec,epoch,st", yes, >> temp1)
		fd3 = temp1
		if (fscan (fd3, str1, str2, str3) == 3) {
		    setjd (spec, observatory=observatory, date="date-obs",
			time="ut", exposure="exptime", jd="jd", hjd="",
			ljd="ljd", utdate=yes, uttime=yes, listonly=no,
			>> log1)
		    if (fscan (fd3, str1, str2, str3, str4) == 4)
			setairmass (spec, intype="beginning",
			    outtype="effective", exposure="exptime",
			    observatory=observatory, show=no, update=yes,
			    override=yes, >> log1)
		}
		fd3 = ""; delete (temp1, verify=no)
		apscript (spec, nsubaps=params.nsubaps, saturation=datamax)
		sapertures (specms, apertures="", apidtable=apidtable,
		    wcsreset=no, verbose=no, beam=INDEF, dtype=INDEF, w1=INDEF,
		    dw=INDEF, z=INDEF, aplow=INDEF, aphigh=INDEF, title=INDEF)
		if (response != "") {
		    if (params.nsubaps == 1)
			sarith (specms, "/", response, specms, w1=INDEF,
			    w2=INDEF, apertures="", bands="", beams="",
			    apmodulus=0, reverse=no, ignoreaps=no,
			    format="multispec", renumber=no, offset=0,
			    clobber=yes, merge=no, errval=0, verbose=no)
		    else {
			blkrep (response, temp, 1, params.nsubaps)
			sarith (specms, "/", temp, specms, w1=INDEF,
			    w2=INDEF, apertures="", bands="", beams="",
			    apmodulus=0, reverse=no, ignoreaps=yes,
			    format="multispec", renumber=no, offset=0,
			    clobber=yes, merge=no, errval=0, verbose=no)
			imdelete (temp, verify=no)
		    }
		}
	    }

	    disperr = no
	    if (disp) {
		# Fix arc headers if necessary.
		if (newarcs) {
		    getspec (arcs1, temp)
	    	    fd2 = temp
	    	    while (fscan (fd2, arc) != EOF) {
			hselect (arc, "date-obs,ut,exptime", yes, > temp1)
			hselect (arc, "ra,dec,epoch,st", yes, >> temp1)
			fd3 = temp1
			if (fscan (fd3, str1, str2, str3) == 3) {
			    setjd (arc, observatory=observatory, date="date-obs",
				time="ut", exposure="exptime", jd="jd", hjd="",
				ljd="ljd", utdate=yes, uttime=yes, listonly=no,
				>> log1)
			    if (fscan (fd3, str1, str2, str3, str4) == 4)
				setairmass (arc, intype="beginning",
				    outtype="effective", exposure="exptime",
				    observatory=observatory, show=no, update=yes,
				    override=yes, >> log1)
			}
			fd3 = ""; delete (temp1, verify=no)
	        	hedit (arc, "refspec1", arc, add=yes, verify=no,
		    	    show=no, update=yes)
	        	hedit (arc, "arctype", "henear", add=yes, verify=no,
		    	    show=no, update=yes)
	    	    }
	    	    fd2 = ""; delete (temp, verify=no)
		    getspec (arcs2, temp)
	    	    fd2 = temp
	    	    while (fscan (fd2, arc) != EOF) {
			hselect (arc, "date-obs,ut,exptime", yes, > temp1)
			hselect (arc, "ra,dec,epoch,st", yes, >> temp1)
			fd3 = temp1
			if (fscan (fd3, str1, str2, str3) == 3) {
			    setjd (arc, observatory=observatory,
				date="date-obs", time="ut", exposure="exptime",
				jd="jd", hjd="", ljd="ljd", utdate=yes,
				uttime=yes, listonly=no, >> log1)
			    if (fscan (fd3, str1, str2, str3, str4) == 4)
				setairmass (arc, intype="beginning",
				    outtype="effective", exposure="exptime",
				    observatory=observatory, show=no,
				    update=yes, override=yes, >> log1)

			}
			fd3 = ""; delete (temp1, verify=no)
	        	hedit (arc, "refspec1", arc, add=yes, verify=no,
		    	    show=no, update=yes)
	        	hedit (arc, "arctype", "shift", add=yes, verify=no,
		    	    show=no, update=yes)
	    	    }
	    	    fd2 = ""; delete (temp, verify=no)
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

		doarcs (spec, response, arcref1, arcref2, extn, arcreplace,
		    apidtable, arcaps, arcbeams, savearcs, reextract, arcap,
		    log1, no, done)

	        hselect (specms, "refspec1", yes, > temp)
	        fd2 = temp
	        i = fscan (fd2, arc)
	        fd2 = ""; delete (temp, verify=no)
		if (i < 1) {
		    print ("No arc reference assigned for ", spec) | tee (log1)
		    disperr = yes
		} else {
		    if (skyalign)
			doalign (spec, specms, align, arcref1ms, log1, no)
	            print ("Dispersion correct ", spec) | tee (log1)
		    dispcor (specms, "", linearize=params.linearize,
			database=database, table=arcref1ms, w1=INDEF,
			w2=INDEF, dw=INDEF, nw=INDEF, log=params.log,
			flux=params.flux, samedisp=no, global=no,
			ignoreaps=no, confirm=no, listonly=no,
			verbose=verbose, logfile=logfile)
		    if (params.nsubaps > 1) {
			imrename (specms, temp, verbose=no)
			scopy (temp, specms, w1=INDEF, w2=INDEF,
			    apertures="1-999", bands="", beams="", apmodulus=0,
			    offset=0, format="multispec", clobber=no, merge=no,
			    renumber=no, verbose=no)
			blkavg (temp, temp, 1, params.nsubaps, option="sum")
			imcopy (temp, specms//"[*,*]", verbose=no)
			imdelete (temp, verify=no)
		    }
		}
	    }

	    if (sky && !disperr) {
		str1 = ""
		if (skyaps != "")
		    str1 = "skyaps=" // skyaps
		if (skybeams != "")
		    str1 = str1 // " skybeams=" // skybeams
	        print ("Sky subtract ", spec, ": ", str1) | tee (log1)
		if (skyedit1) {
		    str1 = substr (ansskyedit, 1, 1)
		    if (str1 == "N" || str1 == "Y")
			skyedit1 = no
		    if (str1 == "n" || str1 == "N")
			skyedit2 = no
		    else
			skyedit2 = yes
		}
		skysub.reject = params.reject
		skysub (specms, output="", objaps=objaps, skyaps=skyaps,
		    objbeams=objbeams, skybeams=skybeams, skyedit=skyedit2,
		    combine=params.combine, scale=params.scale,
		    saveskys=saveskys, logfile=logfile)
		params.reject = skysub.reject
	        hedit (specms, "skysub", yes, add=yes, show=no, verify=no,
		    update=yes)
	    }

	    if (!disperr && (extract || disp || sky)) {
		if (splot1) {
		    print (specms, ":")
		    str1 = anssplot
		    if (str1 == "NO" || str1 == "YES")
			splot1 = no
		    if (str1 == "no" || str1 == "NO")
			splot2 = no
		    else
			splot2 = yes
		}
		if (splot2)
		    splot (specms)
	    }

	    print (spec, >> done)
	}
	fd1 = ""; delete (objs, verify=no)

	if (access (done))
	    delete (done, verify=no)
end
