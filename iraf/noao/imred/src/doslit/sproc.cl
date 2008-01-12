# SPROC -- Process spectra from 2D to 1D
#
# This program combines all the operations of extraction, dispersion
# correction, extinction correction, and flux calibration in as simple
# and noninteractive manner as possible.

procedure sproc (objects, arcs1, arctable, standards, crval, cdelt, dispcor,
	extcor, fluxcal, resize, clean, splot, redo, update, quicklook, batch,
	listonly)

file	objects = ""		{prompt="List of object spectra"}

file	arcs1 = ""		{prompt="List of arc spectra"}
file	arctable = ""		{prompt="Arc assignment table (optional)"}
file	standards = ""		{prompt="List of standard star spectra\n"}

string	crval = "INDEF"		{prompt="Approximate wavelength"}
string	cdelt = "INDEF"		{prompt="Approximate dispersion\n"}

bool	dispcor = yes		{prompt="Dispersion correct spectra?"}
bool	extcor = no		{prompt="Extinction correct spectra?"}
bool	fluxcal = no		{prompt="Flux calibrate spectra?"}
bool	resize = no		{prompt="Automatically resize apertures?"}
bool	clean = no		{prompt="Detect and replace bad pixels?"}
bool	splot = no		{prompt="Plot the final spectrum?"}
bool	redo = no		{prompt="Redo operations if previously done?"}
bool	update = no		{prompt="Update spectra if cal data changes?"}
bool	quicklook = no		{prompt="Minimally interactive quick-look?"}
bool	batch = no		{prompt="Extract objects in batch?"}
bool	listonly = no		{prompt="List steps but don't process?\n"}

real	datamax = INDEF		{prompt="Max data value / cosmic ray threshold"}

string	anssplot		{prompt="Splot spectrum?", mode="q",
				 enum="no|yes|NO|YES"}
bool	newdisp, newsens, newarcs
bool	fluxcal1, splot1, splot2
bool	dobatch

struct	*fd1, *fd2, *fd3

begin
	file	arcref1, spec, arc
	file	arcref1ms, specms, arcms
	file	temp, done
	string	imtype, mstype
	string	str1, str2, str3, str4, arcrefs, log1, log2
	bool	reextract, extract, disp, ext, flux, log
	int	i, j, n, nspec
	struct	err

	# Call a separate task to do the listing to minimize the size of
	# this script and improve it's readability.

	dobatch = no
	if (listonly) {
	    slistonly (objects, arcs1, standards, dispcor, extcor,
		fluxcal, redo, update)
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

	temp = mktemp ("tmp$iraf")
	done = mktemp ("tmp$iraf")

	# Get query parameters
	if (arctable == "")
	    arcrefs = "@"//arcs1
	else
	    arcrefs = arctable
	arcref1 = ""

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

	# If the update switch is used changes in the calibration data can
	# cause images to be reprocessed (if they are in the object list).
	# Possible changes are in the dispersion solution and sensitivity
	# function.  The newarcs flag is used to only go through the arc
	# image headers once setting the reference spectrum, airmass, and UT.

	newdisp = no
	newsens = no
	newarcs = yes
	fluxcal1 = fluxcal

	# Check if there are aperture definitions in the database and
	# define them if needed.  This is interactive.

	print ("Define object apertures", >> log1)
	if (resize)
	    apslitproc.ansresize = "YES"
	else
	    apslitproc.ansresize = "NO"
	if (quicklook) {
	    apslitproc.ansedit = "NO"
	    apslitproc.ansfittrace = "NO"
	} else {
	    apslitproc.ansedit = "yes"
	    apslitproc.ansfittrace = "yes"
	}
	if (redo) {
	    delete (database//"/ap//@"//objects, verify=no, >& "dev$null")
	    apslitproc ("@"//objects, references="", ansfind="YES",
		ansrecenter="NO", anstrace="YES", ansextract="NO")
	} else
	    apslitproc ("@"//objects, references="NEW", ansfind="YES",
		ansrecenter="NO", anstrace="YES", ansextract="NO")
	if (dispcor && fluxcal1) {
	    if (redo) {
		delete (database//"/ap//@"//standards, verify=no, >& "dev$null")
		apslitproc ("@"//standards, references="", ansfind="YES",
		    ansrecenter="NO", anstrace="YES", ansextract="NO")
	    } else
		apslitproc ("@"//standards, references="NEW", ansfind="YES",
		    ansrecenter="NO", anstrace="YES", ansextract="NO")
	}

	# Initialize APSLITPROC.
	apslitproc.saturation = INDEF
	apslitproc.references = ""
	apslitproc.profiles = ""
	apslitproc.ansrecenter = "NO"
	apslitproc.ansresize = "NO"
	apslitproc.ansedit = "NO"
	apslitproc.anstrace = "NO"
	apslitproc.ansfittrace = "NO"
	apslitproc.ansextract = "YES"
	apslitproc.ansreview = "NO"

	# Initialize REIDENTIFY
	if (quicklook)
	    reidentify.answer = "NO"
	else
	    reidentify.answer = "yes"

	if (splot && !quicklook) {
	    splot1 = yes
	    splot2 = yes
	} else {
	    splot1 = no
	    splot2 = no
	}

	# If not dispersion correcting we can go directly to extracting
	# the object spectra.  The reference arcs are the first on
	# the arc lists.  The processing of the reference arcs is done
	# by the task SARCREFS.

	if (dispcor) {
	    fd1 = arcs1
	    fd2 = objects
	    if (fscan (fd1, arcref1) == EOF)
		error (1, "No reference arcs")
	    fd1 = ""
	    if (fscan (fd2, spec) == EOF)
		error (1, "No object spectra for arc reference")
	    fd2 = ""
	    i = strlen (arcref1)
	    if (!access (arcref1 // imtype)) {
		printf ("Arc reference spectrum not found - %s%s\n",
		    arcref1, imtype) | scan (err)
		error (1, err // "\nCheck setting of imtype")
	    }
	    arcref1ms = arcref1 // mstype
	    if (redo && access (arcref1ms))
	        imdelete (arcref1ms, verify=no)
	    apslitproc.references = spec
	    sarcrefs (arcref1, crval, cdelt, done, log1, log2)
	    apslitproc.references = ""

	    if (fluxcal1)
		sfluxcal (standards, arcs1, arcref1, arcrefs,
		    redo, update, extcor, done, log1, log2)
	}

	# Now we are ready to process the object spectra.

	reextract = redo || (update && newdisp)
	fd1 = objects
	while (fscan (fd1, spec) != EOF) {
	    i = strlen (spec)
	    if (i > n && substr (spec, i-n+1, i) == imtype)
		spec = substr (spec, 1, i-n)
	    
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
		print ("Object spectrum not found - " // spec // imtype) |
		    tee (log1)
		print ("Check setting of imtype")
		next
	    }
	    specms = spec // mstype

	    # Determine required operations from the flags and image header.
	    extract = no
	    disp = no
	    ext = no
	    flux = no
	    if (reextract || !access (specms))
		extract = yes
	    else {
		hselect (specms, "dispcor", yes, > temp)
		hselect (specms, "ex-flag", yes, >> temp)
		hselect (specms, "ca-flag", yes, >> temp)
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
		sbatch.arcs1 = arcs1
		sbatch.arcref1 = arcref1
		sbatch.arcrefs = arcrefs
		sbatch.done = done
		sbatch.logfile = log1
		sbatch.redo = reextract
		sbatch.update = update
		sbatch.dispcor = dispcor
		sbatch.fluxcal1 = fluxcal1
		sbatch.extcor = extcor
		sbatch.newdisp = newdisp
		sbatch.newsens = newsens
		sbatch.newarcs = newarcs
		dobatch = yes
		return
	    }

	    # Process the spectrum in foreground.
	    if (extract) {
		if (access (specms))
		    imdelete (specms, verify=no)
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

	    if (disp) {
		# Fix arc headers if necessary.
		if (newarcs) {
	    	    fd2 = arcs1
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

		sdoarcs (spec, arcref1, reextract, log1, no)

	        hselect (specms, "refspec1", yes, > temp)
	        fd2 = temp
	        i = fscan (fd2, arc)
	        fd2 = ""; delete (temp, verify=no)
		if (i < 1)
		    print ("No arc reference assigned for ", spec) | tee (log1)
		else {
	            print ("Dispersion correct ", spec) | tee (log1)
		    dispcor (specms, "", linearize=sparams.linearize,
			database=database, table=arcref1//mstype,
			w1=INDEF, w2=INDEF, dw=INDEF, nw=INDEF,
			log=sparams.log, flux=sparams.flux, samedisp=no,
			global=no, ignoreaps=yes, confirm=no, listonly=no,
			verbose=verbose, logfile=logfile)
		    flpr
		    hedit (specms, "dispcor", 0, add=yes, verify=no,
			show=no, update=yes)
		    disp = no
		}
	    }

	    if (!disp) {
	        if (ext)
		    print ("Extinction correct ", spec) | tee (log1)
	        if (flux)
		    print ("Flux calibrate ", spec) | tee (log1)
	        if (flux || ext)
		    calibrate (specms, "", extinct=extcor, flux=fluxcal1,
			extinction=extinction, observatory=observatory,
			ignoreaps=yes, sensitivity="sens", fnu=sparams.fnu) |
			tee (log1, > log2)
	    }
	    if (extract || disp || ext || flux) {
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
		else if (splot && quicklook)
		    bplot (specms, apertures="1", band=1, graphics="stdgraph",
			cursor="onedspec$gcurval")
	    }
	    print (spec, >> done)
	}
	fd1 = ""

	if (access (done))
	    delete (done, verify=no)
end
