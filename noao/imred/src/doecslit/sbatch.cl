# SBATCH -- Process spectra in batch.
# This task is called in batch mode.  It only processes objects
# not previously processed unless the update or redo flags are set.

procedure sbatch ()

file	objects		{prompt="Object spectra"}

file	arcs		{prompt="List of arc spectra"}
file	arcref		{prompt="Arc reference for dispersion solution"}
string	arcrefs		{prompt="Arc references\n"}

file	done		{prompt="File of spectra already done"}
file	logfile		{prompt="Logfile"}
bool	redo		{prompt="Redo operations?"}
bool	update		{prompt="Update spectra?\n"}

bool	scattered	{prompt="Subtract scattered light?"}
bool	arcap		{prompt="Use object apertures for arcs?"}
bool	dispcor		{prompt="Dispersion correct spectra?"}
bool	extcor		{prompt="Extinction correct spectra?"}
bool	fluxcal1	{prompt="Flux calibrate spectra?"}

bool	newaps, newdisp, newsens, newarcs

struct	*fd1, *fd2

begin
	file	temp, spec, specec, arc
	bool	reextract, extract, scat, disp, ext, flux, log, disperr
	string	str
	int	i
	str = ""

	temp = mktemp ("tmp$iraf")

	reextract = redo || (update && (newaps || newdisp))

	fd1 = objects
	while (fscan (fd1, spec) != EOF) {
	    if (access (done)) {
	        fd2 = done
	        while (fscan (fd2, specec) != EOF)
		    if (spec == specec)
		        break
	        if (spec == specec)
		    next
	        fd2 = ""
	    }
	    if (!access (spec // ".imh")) {
		print ("Object spectrum not found - " // spec, >> logfile)
		next
	    }
	    specec = spec // ".ec.imh"

	    scat = no
	    extract = no
	    disp = no
	    ext = no
	    flux = no
	    if (scattered) {
		hselect (spec, "apscatte", yes, > temp)
		fd2 = temp
		if (fscan (fd2, str) < 1)
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
		if (fscan (fd2, str) == 1) {
		    extract = update && newdisp
		    if (update && !newdisp)
		        # We really should check if REFSPEC will assign
			# different reference spectra.
			;
		} else
		    disp = dispcor
		if (fscan (fd2, str) == 1)
		    extract = update && !extcor
		else
		    ext = extcor
		if (fscan (fd2, str) == 1)
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
		
	    if (extract) {
		if (access (specec))
		    imdelete (specec, verify=no)

		if (scat) {
		    print ("Subtract scattered light in ", spec, >> logfile)
		    apslitproc (spec, ansextract="NO", ansscat="YES")
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
		apslitproc (spec, verbose=no)
	    }

	    disperr = no
	    if (disp) {
		# Fix arc headers if necessary.
		if (newarcs) {
		    setjd ("@"//arcs, observatory=observatory, date="date-obs",
			time="ut", exposure="exptime", jd="jd", hjd="",
			ljd="ljd", utdate=yes, uttime=yes, listonly=no,
			>> logfile)
	    	    setairmass ("@"//arcs, intype="beginning",
			outtype="effective", exposure="exptime",
			observatory=observatory, show=no, update=yes,
			override=yes, >> logfile)
	    	    fd2 = arcs
	    	    while (fscan (fd2, arc) != EOF)
	        	hedit (arc, "refspec1", arc, add=yes, verify=no,
		    	    show=no, update=yes)
	    	    fd2 = ""
		    newarcs = no
		}

		print ("Assign arc spectra for ", spec, >> logfile)
		refspectra (spec, references=arcrefs,
		    apertures="", refaps="", ignoreaps=no,
		    select=sparams.select, sort=sparams.sort,
		    group=sparams.group, time=sparams.time,
		    timewrap=sparams.timewrap, override=yes, confirm=no,
		    assign=yes, logfiles="STDOUT", verbose=no,
		    >> logfile)

		sdoarcs (spec, arcref, reextract, arcap, logfile, yes)

	        hselect (specec, "refspec1", yes, > temp)
	        fd2 = temp
	        i = fscan (fd2, arc)
	        fd2 = ""; delete (temp, verify=no)
		if (i < 1) {
		    print ("No arc reference assigned for ", spec, >> logfile)
		    disperr = yes
		} else {
	            print ("Dispersion correct ", spec, >> logfile)
		    dispcor (specec, "", linearize=sparams.linearize,
			database=database, table=arcref//".ec.imh",
			w1=INDEF, w2=INDEF, dw=INDEF, nw=INDEF,
			log=sparams.log, flux=sparams.flux, samedisp=no,
			global=no, ignoreaps=no, confirm=no, listonly=no,
			logfile=logfile, > "dev$null")
		    hedit (specec, "dc-flag", 0, add=yes, show=no,
			verify=no, update=yes)
		}
	    }

	    if (!disperr && (extract || disp)) {
	        if (ext)
		    print ("Extinction correct ", spec, >> logfile)
	        if (flux)
		    print ("Flux calibrate ", spec, >> logfile)
	        if (flux || ext)
		    calibrate (specec, "", extinct=extcor, flux=fluxcal1,
			extinction=extinction, observatory=observatory,
			ignoreaps=no, sensitivity="sens", fnu=sparams.fnu,
			>> logfile)
	    }
	}
	fd1 = ""
	delete (objects, verify=no)
	delete (arcs, verify=no)

	if (access (done))
	    delete (done, verify=no)

	flprcache (0)
end
