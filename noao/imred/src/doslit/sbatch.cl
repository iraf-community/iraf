# SBATCH -- Process spectra in batch.
# This task is called in batch mode.  It only processes objects
# not previously processed unless the update or redo flags are set.

procedure sbatch ()

file	objects		{prompt="List of object spectra"}
file	arcs1		{prompt="List of arc spectra"}
file	arcref1		{prompt="Arc reference for dispersion solution"}
string	arcrefs		{prompt="Arc references\n"}

file	done		{prompt="File of spectra already done"}
file	logfile		{prompt="Logfile"}

bool	redo		{prompt="Redo operations?"}
bool	update		{prompt="Update spectra?"}
bool	dispcor		{prompt="Dispersion correct spectra?"}
bool	extcor		{prompt="Extinction correct spectra?"}
bool	fluxcal1	{prompt="Flux calibrate spectra?"}

bool	newdisp, newsens, newarcs

struct	*fd1, *fd2

begin
	file	temp, spec, specms, arc, arcms
	bool	reextract, extract, disp, ext, flux, log
	string	str1
	int	i

	temp = mktemp ("tmp$iraf")

	reextract = redo || (update && newdisp)

	fd1 = objects
	while (fscan (fd1, spec) != EOF) {
	    if (access (done)) {
	        fd2 = done
	        while (fscan (fd2, specms) != EOF)
		    if (spec == specms)
		        break
	        if (spec == specms)
		    next
	        fd2 = ""
	    }
	    if (!access (spec // ".imh")) {
		print ("Object spectrum not found - " // spec, >> logfile)
		next
	    }
	    specms = spec // ".ms.imh"

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

	    if (extract) {
		if (access (specms))
		    imdelete (specms, verify=no)
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

	    if (disp) {
		# Fix arc headers if necessary.
		if (newarcs) {
		    setjd ("@"//arcs1, observatory=observatory, date="date-obs",
			time="ut", exposure="exptime", jd="jd", hjd="",
			ljd="ljd", utdate=yes, uttime=yes, listonly=no,
			>> logfile)
	    	    setairmass ("@"//arcs1, intype="beginning",
			outtype="effective", exposure="exptime",
			observatory=observatory, show=no, update=yes,
			override=yes, >> logfile)
	    	    fd2 = arcs1
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

		sdoarcs (spec, arcref1, reextract, logfile, yes)

	        hselect (specms, "refspec1", yes, > temp)
	        fd2 = temp
	        i = fscan (fd2, arc)
	        fd2 = ""; delete (temp, verify=no)
		if (i < 1)
		    print ("No arc reference assigned for ", spec, >> logfile)
		else {
	            print ("Dispersion correct ", spec, >> logfile)
		    dispcor (specms, "", linearize=sparams.linearize,
			database=database, table=arcref1//".ms.imh",
			w1=INDEF, w2=INDEF, dw=INDEF, nw=INDEF,
			log=sparams.log, flux=sparams.flux, samedisp=no,
			global=no, ignoreaps=no, confirm=no, listonly=no,
			verbose=no, logfile=logfile)
		    hedit (specms, "dispcor", 0, add=yes, verify=no,
			show=no, update=yes)
		    disp = no
		}
	    }

	    if (!disp) {
	        if (ext)
		    print ("Extinction correct ", spec, >> logfile)
	        if (flux)
		    print ("Flux calibrate ", spec, >> logfile)
	        if (flux || ext)
		    calibrate (specms, "", extinct=extcor, flux=fluxcal1,
			extinction=extinction, observatory=observatory,
			ignoreaps=yes, sensitivity="sens", fnu=sparams.fnu,
			>> logfile)
	    }
	}
	fd1 = ""
	delete (objects, verify=no)
	delete (arcs1, verify=no)

	if (access (done))
	    delete (done, verify=no)

	flprcache (0)
end
