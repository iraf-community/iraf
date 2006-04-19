# SPROC -- Process echelle slit spectra
# This program combines all the operations of scattered light
# subtraction, extraction, dispersion correction, extinction correction,
# and flux calibration in as simple and noninteractive manner as
# possible.  The data must all share the same position on the 2D image
# and the same dispersion solution apart from small instrumental changes
# which can be followed automatically.

procedure sproc (objects, apref, arcs, arctable, standards, recenter,
	resize, quicklook, trace, scattered, arcap, dispcor, extcor,
	fluxcal, splot, redo, update, batch, listonly)

file	objects			{prompt="List of object spectra"}

file	apref			{prompt="Aperture reference spectrum"}
file	arcs			{prompt="List of arc spectra"}
file	arctable		{prompt="Arc assignment table (optional)"}
file	standards		{prompt="List of standard star spectra\n"}

bool	recenter		{prompt="Recenter object apertures?"}
bool	resize			{prompt="Resize object apertures?"}
bool	quicklook		{prompt="Edit/review object apertures?"}
bool	trace			{prompt="Trace object spectra?"}
bool	scattered		{prompt="Subtract scattered light?"}
bool	arcap			{prompt="Use object apertures for arcs?"}
bool	dispcor			{prompt="Dispersion correct spectra?"}
bool	extcor			{prompt="Extinction correct spectra?"}
bool	fluxcal			{prompt="Flux calibrate spectra?"}
bool	splot			{prompt="Plot the final spectrum?"}
bool	redo			{prompt="Redo operations if previously done?"}
bool	update			{prompt="Update spectra if cal data changes?"}
bool	batch			{prompt="Extract objects in batch?"}
bool	listonly		{prompt="List steps but don't process?\n"}

real	datamax = INDEF		{prompt="Max data value / cosmic ray threshold"}

string	anssplot = "yes"	{prompt="Splot spectrum?", mode="q",
				 enum="no|yes|NO|YES"}
bool	newaps, newdisp, newsens, newarcs
bool	fluxcal1, splot1, splot2
bool	dobatch

struct	*fd1, *fd2, *fd3

begin
	string	imtype, ectype
	string	arcref, spec, arc
	string	arcrefec, specec, arcec
	string	temp, done
	string	str1, str2, str3, str4, arcrefs, log1, log2
	bool	reextract, extract, scat, disp, ext, flux, log, disperr
	int	i, j, n
	struct	err
	str1 = ""

	# Call a separate task to do the listing to minimize the size of
	# this script and improve it's readability.

	dobatch = no
	if (listonly) {
	    slistonly (objects, apref, arcs, standards, scattered,
		dispcor, extcor, fluxcal, redo, update)
	    bye
	}

	imtype = "." // envget ("imtype")
	i = stridx (",", imtype)
	if (i > 0)
	    imtype = substr (imtype, 1, i-1)
	ectype = ".ec" // imtype
	n = strlen (imtype)

	# Temporary files used repeatedly in this script.  Under some
	# abort circumstances these files may be left behind.

	temp = mktemp ("tmp$iraf")
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
	# dispersion solution, and sensitivity function.  The newarcs
	# flag is used to only go through the arc image headers once
	# setting the reference spectrum, airmass, and UT.

	newaps = no
	newdisp = no
	newsens = no
	newarcs = yes
	fluxcal1 = fluxcal

	# Check if there are aperture definitions in the database and
	# define them if needed.  This is usually somewhat interactive.
	# Set the newaps flag in case an update is desired.

	# Initialize APSCRIPT for aperture reference.
	apslitproc.saturation = INDEF
	apslitproc.references = ""
	apslitproc.ansfind = "YES"
	if (recenter)
	    apslitproc.ansrecenter = "YES"
	else
	    apslitproc.ansrecenter = "NO"
	if (resize)
	    apslitproc.ansresize = "YES"
	else
	    apslitproc.ansresize = "NO"
	apslitproc.ansedit = "yes"
	apslitproc.anstrace = "YES"
	apslitproc.ansfittrace = "yes"
	apslitproc.ansextract = "NO"

	i = strlen (apref)
	if (i > n && substr (apref, i-n+1, i) == imtype)
	    apref = substr (apref, 1, i-n)

	reextract = redo
	if (reextract || !access (database // "/ap" // apref)) {
	    if (!access (apref // imtype)) {
		printf ("Aperture reference spectrum not found - %s%s\n",
		    apref, imtype) | scan (err)
		error (1, err // "\nCheck setting of imtype")
	    }
	    scat = no
	    if (scattered) {
		hselect (apref, "apscatte", yes, > temp)
		fd1 = temp
		if (fscan (fd1, str1) < 1)
		    scat = yes
		fd1 = ""; delete (temp, verify=no)
	    }

	    print ("Set reference aperture for ", apref) | tee (log1)
	    delete (database//"/ap"//apref, verify=no, >& "dev$null")
	    apslitproc (apref)
	    newaps = yes
	}

	# Initialize APSCRIPT for aperture definitions.
	if (quicklook) {
	    apslitproc.ansedit = "NO"
	    apslitproc.ansfittrace = "NO"
	}
	if (trace) {
	    apslitproc.anstrace = "yes"
	} else {
	    apslitproc.anstrace = "NO"
	}
	apslitproc.ansextract = "NO"
	apslitproc.ansscat = "NO"

	print ("Define object apertures", >> log1)
	if (redo)
	    apslitproc ("@"//objects, references=apref)
	else
	    apslitproc ("@"//objects, references="NEW"//apref)
	if (dispcor && fluxcal1) {
	    if (redo)
		apslitproc ("@"//standards, references=apref)
	    else
		apslitproc ("@"//standards, references="NEW"//apref)
	}

	# Initialize APSCRIPT for extraction and SPLOT.
	apslitproc.ansrecenter = "NO"
	apslitproc.ansresize = "NO"
	apslitproc.ansedit = "NO"
	apslitproc.anstrace = "NO"
	apslitproc.ansextract = "YES"
	apslitproc.ansreview = "NO"
	apslitproc.ansscat = "NO"
	apslitproc.anssmooth = "YES"

	if (splot && !quicklook) {
	    splot1 = yes
	    splot2 = yes
	} else {
	    splot1 = no
	    splot2 = no
	}

	# The next step is to setup the scattered light correction if needed.
	# We use the aperture reference image for the interactive setting.
	# If this image has been  scattered light corrected we assume the
	# scattered light functions parameters are correctly set.

	scat = no
	if (scattered) {
	    hselect (apref, "apscatte", yes, > temp)
	    fd1 = temp
	    if (fscan (fd1, str1) < 1)
		scat = yes
	    fd1 = ""; delete (temp, verify=no)
	}
	if (scat) {
	    print ("Setup and do scattered light subtraction in ", apref) |
		tee (log1)
	    apslitproc.ansfitscatter = "yes"
	    apslitproc.ansfitsmooth = "yes"
	    apslitproc (apref, ansextract="NO", ansscat="YES")
	    apslitproc.ansfitscatter = "NO"
	    apslitproc.ansfitsmooth = "NO"
	}

	# If not dispersion correcting we can go directly to extracting
	# the object spectra.  The reference arcs are the first on
	# the arc lists.  The processing of the reference arcs is done
	# by the task ARCREFS.

	arcref = ""
	arcrefs = ""
	if (dispcor) {
	    if (arctable == "")
		arcrefs = "@"//arcs
	    else
		arcrefs = arctable

	    fd1 = arcs
	    if (fscan (fd1, arcref) == EOF)
		error (1, "No reference arcs")
	    fd1 = ""
	    if (!access (arcref // imtype)) {
		printf ("Arc reference spectrum not found - %s%s\n",
		    arcref, imtype) | scan (err)
		error (1, err // "\nCheck setting of imtype")
	    }
	    arcrefec = arcref // ectype
	    reextract = redo || (update && newaps)
	    if (reextract && access (arcrefec))
	        imdelete (arcrefec, verify=no)

	    apslitproc.references = apref
	    sarcrefs (arcref, done, log1, log2)
	    apslitproc.references = ""

	    if (fluxcal1)
		sfluxcal (standards, arcs, arcref, arcrefs, redo, update,
		    scattered, arcap, extcor, done, log1, log2)
	}

	# Now we are ready to process the object spectra.

	reextract = redo || (update && (newaps || newdisp))
	fd1 = objects
	while (fscan (fd1, spec) != EOF) {
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
	    ext = no
	    flux = no
	    if (scattered) {
		hselect (spec, "apscatte", yes, > temp)
		fd2 = temp
		if (fscan (fd2, str1) < 1)
		    scat = yes
		fd2 = ""; delete (temp, verify=no)
	    }
	    if (reextract || !access (specec) || (update && scat))
		extract = yes
	    else {
		hselect (specec, "dc-flag", yes, > temp)
		hselect (specec, "ex-flag", yes, >> temp)
		hselect (specec, "ca-flag", yes, >> temp)
		fd2 = temp
		if (fscan (fd2, str1) == 1) {
		    extract = update && newdisp
		    if (update && !newdisp)
		        # We really should check if REFSPEC will assign
			# different reference spectra.
			;
		} else
		    disp = dispcor
		if (fscan (fd2, str1) == 1)
		    extract = update && !extcor
		else
		    ext = extcor
		if (fscan (fd2, str1) == 1)
		    extract = update && (!fluxcal1 || newsens)
		else
		    flux = fluxcal1
		fd2 = ""; delete (temp, verify=no)
	    }

	    if (extract) {
		disp = dispcor
		ext = extcor
		flux = fluxcal1
	    }

	    # If fully processed go to the next object.
	    if (!extract && !disp && !extcor && !flux)
		next

	    # If not interactive and the batch flag is set submit rest to batch.
	    if (batch && !splot1 && !splot2) {
		fd1 = ""
		flprcache
		sbatch.objects = objects
		sbatch.datamax = datamax
		sbatch.arcs = arcs
		sbatch.arcref = arcref
		sbatch.arcrefs = arcrefs
		sbatch.done = done
		sbatch.logfile = log1
		sbatch.redo = reextract
		sbatch.update = update
		sbatch.scattered = scattered
		sbatch.arcap = arcap
		sbatch.dispcor = dispcor
		sbatch.fluxcal1 = fluxcal1
		sbatch.extcor = extcor
		sbatch.newaps = newaps
		sbatch.newdisp = newdisp
		sbatch.newsens = newsens
		sbatch.newarcs = newarcs
		dobatch = yes
		return
	    }

	    # Process the spectrum in foreground.
	    if (extract) {
		if (access (specec))
		    imdelete (specec, verify=no)

		if (scat) {
		    print ("Subtract scattered light in ", spec) | tee (log1)
		    apslitproc (spec, ansextract="NO", ansscat="YES")
		}

		print ("Extract object spectrum ", spec) | tee (log1)
		hselect (spec, "date-obs,ut,exptime", yes, > temp)
		hselect (spec, "ra,dec,epoch,st", yes, >> temp)
		fd2 = temp
		if (fscan (fd2, str1, str2, str3) == 3) {
		    setjd (spec, observatory=observatory, date="date-obs",
			time="ut", exposure="exptime", jd="jd", hjd="",
			ljd="ljd", utdate=yes, uttime=yes, listonly=no,
			>> log1)
		    if (fscan (fd2, str1, str2, str3, str4) == 4)
			setairmass (spec, intype="beginning",
			    outtype="effective", exposure="exptime",
			    observatory=observatory, show=no, update=yes,
			    override=yes, >> log1)
		}
		fd2 = ""; delete (temp, verify=no)
		apslitproc (spec, saturation=datamax)
	    }

	    disperr = no
	    if (disp) {
		# Fix arc headers if necessary.
		if (newarcs) {
	    	    fd2 = arcs
	    	    while (fscan (fd2, arc) != EOF) {
			hselect (arc, "date-obs,ut,exptime", yes, > temp)
			hselect (arc, "ra,dec,epoch,st", yes, >> temp)
			fd3 = temp
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
			fd3 = ""; delete (temp, verify=no)
	        	hedit (arc, "refspec1", arc, add=yes, verify=no,
		    	    show=no, update=yes)
		    }
	    	    fd2 = ""
		    newarcs = no
		}

		print ("Assign arc spectra for ", spec) | tee (log1)
		refspectra (spec, references=arcrefs,
		    apertures="", refaps="", ignoreaps=no,
		    select=sparams.select, sort=sparams.sort,
		    group=sparams.group, time=sparams.time,
		    timewrap=sparams.timewrap, override=yes, confirm=no,
		    assign=yes, logfiles="STDOUT", verbose=no) |
		    tee (log1, > log2)

		sdoarcs (spec, arcref, reextract, arcap, log1, no)

	        hselect (specec, "refspec1", yes, > temp)
	        fd2 = temp
	        i = fscan (fd2, arc)
	        fd2 = ""; delete (temp, verify=no)
		if (i < 1) {
		    print ("No arc reference assigned for ", spec) | tee (log1)
		    disperr = yes
		} else {
	            print ("Dispersion correct ", spec) | tee (log1)
		    dispcor (specec, "", linearize=sparams.linearize,
			database=database, table=arcref//ectype,
			w1=INDEF, w2=INDEF, dw=INDEF, nw=INDEF,
			log=sparams.log, flux=sparams.flux, samedisp=no,
			global=no, confirm=no, ignoreaps=no, listonly=no,
			logfile=logfile)
		    hedit (specec, "dc-flag", 0, add=yes, show=no,
			verify=no, update=yes)
		}
	    }

	    if (!disperr && (extract || disp)) {
	        if (ext)
		    print ("Extinction correct ", spec) | tee (log1)
	        if (flux)
		    print ("Flux calibrate ", spec) | tee (log1)
	        if (flux || ext)
		    calibrate (specec, "", extinct=extcor, flux=fluxcal1,
			extinction=extinction, observatory=observatory,
			ignoreaps=no, sensitivity="sens", fnu=sparams.fnu) |
			tee (log1, > log2)
	    }
	    if (extract || disp || ext || flux) {
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
		else if (splot && quicklook) {
		    if (disp) {
			print ("q") |
			specplot (specec, apertures="", autolayout=no,
			    scale=1., offset=0., step=0., sysid=yes,
			    yscale=yes, xmin=INDEF, xmax=INDEF, ymin=INDEF,
			    ymax=INDEF, logfile="", graphics="stdgraph",
			    cursor="STDIN")
		    } else {
			print ("q") |
			specplot (specec, apertures="", autolayout=yes,
			    autoscale=no, scale=1., offset=0., step=0.,
			    sysid=yes, yscale=no, xmin=INDEF, xmax=INDEF,
			    ymin=INDEF, ymax=INDEF, logfile="",
			    graphics="stdgraph", cursor="STDIN")
		    }
		}
	    }
	    print (spec, >> done)
	}
	fd1 = ""

	if (access (done))
	    delete (done, verify=no)
end
