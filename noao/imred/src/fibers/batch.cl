# BATCH -- Process spectra in batch.
# This task is called in batch mode.  It only processes objects
# not previously processed unless the update or redo flags are set.

procedure batch ()

string	objects		{prompt="Object spectra"}

file	response	{prompt="Response spectrum"}
string	arcs1		{prompt="List of arc spectra"}
string	arcs2		{prompt="List of shift arc spectra"}
file	arcref1		{prompt="Arc reference for dispersion solution"}
file	arcref2		{prompt="Arc reference for dispersion solution"}
file	arcreplace	{prompt="Special aperture replacements"}
string	arcrefs		{prompt="Arc references"}
string	extn		{prompt="Extraction extension"}

file	apidtable	{prompt="Aperture identifications"}
string	objaps		{prompt="Object apertures"}
string	skyaps		{prompt="Sky apertures"}
string	arcaps		{prompt="Arc apertures"}
string	objbeams	{prompt="Object beam numbers"}
string	skybeams	{prompt="Sky beam numbers"}
string	arcbeams	{prompt="Arc beam numbers\n"}

file	done		{prompt="File of spectra already done"}
file	logfile		{prompt="Logfile"}

bool	redo		{prompt="Redo operations?"}
bool	update		{prompt="Update spectra?"}
bool	arcap		{prompt="Use object apertures for arcs?"}
bool	dispcor		{prompt="Dispersion correct spectra?"}
bool	savearcs	{prompt="Save internal arcs?"}
bool	skysubtract	{prompt="Subtract sky?"}
bool	saveskys	{prompt="Save sky spectra?\n"}

bool	newaps, newresp, newdisp, newarcs

struct	*fd1, *fd2

begin
	file	objs, temp, spec, specms, arc
	bool	reextract, extract, disp, sky, log
	string	str
	int	i

	objs = mktemp ("tmp$iraf")
	temp = mktemp ("tmp$iraf")

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

	getspec (objects, > objs)
	fd1 = objs
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
	    sky = no
	    if (reextract || !access (specms))
		extract = yes
	    else {
		hselect (specms, "dc-flag", yes, > temp)
		fd2 = temp
		if (fscan (fd2, str) == 1) {
		    extract = update && newdisp
		    if (update && !newdisp)
		        # We really should check if REFSPEC will assign
			# different reference spectra.
			;
		} else
		    disp = dispcor
		fd2 = ""; delete (temp, verify=no)

		hselect (specms, "skysub", yes, > temp)
		fd2 = temp
		if (fscan (fd2, str) < 1)
		    sky = skysubtract
		fd2 = ""; delete (temp, verify=no)
	    }
		
	    if (extract) {
		disp = dispcor
		sky = skysubtract
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
		apscript (spec, nsubaps=params.nsubaps, verbose=no)
		sapertures (specms, apertures="", apidtable=apidtable,
		    verbose=no)
		if (response != "") {
		    if (params.nsubaps == 1)
			sarith (specms, "/", response, specms, w1=INDEF,
			    w2=INDEF, apertures="", beams="", apmodulus=0,
			    reverse=no, ignoreaps=no, format="multispec",
			    renumber=no, offset=0, clobber=yes, merge=no,
			    errval=0, verbose=no)
		    else {
			blkrep (response, temp, 1, params.nsubaps)
			sarith (specms, "/", temp, specms, w1=INDEF,
			    w2=INDEF, apertures="", beams="", apmodulus=0,
			    reverse=no, ignoreaps=yes, format="multispec",
			    renumber=no, offset=0, clobber=yes, merge=no,
			    errval=0, verbose=no)
			imdelete (temp, verify=no)
		    }
		}
	    }

	    if (disp) {
		# Fix arc headers if necessary.
		if (newarcs) {
		    getspec (arcs1, > temp)
		    setjd ("@"//temp, observatory=observatory, date="date-obs",
			time="ut", exposure="exptime", jd="jd", hjd="",
			ljd="ljd", utdate=yes, uttime=yes, listonly=no,
			>> logfile)
	    	    setairmass ("@"//temp, intype="beginning",
			outtype="effective", exposure="exptime",
			observatory=observatory, show=no, update=yes,
			override=yes, >> logfile)
	    	    fd2 = temp
	    	    while (fscan (fd2, arc) != EOF) {
	        	hedit (arc, "refspec1", arc, add=yes, verify=no,
		    	    show=no, update=yes)
	        	hedit (arc, "arctype", "henear", add=yes, verify=no,
		    	    show=no, update=yes)
	    	    }
	    	    fd2 = ""; delete (temp, verify=no)
		    getspec (arcs2, > temp)
		    setjd ("@"//temp, observatory=observatory, date="date-obs",
			time="ut", exposure="exptime", jd="jd", hjd="",
			ljd="ljd", utdate=yes, uttime=yes, listonly=no,
			>> logfile)
	    	    setairmass ("@"//temp, intype="beginning",
			outtype="effective", exposure="exptime",
			observatory=observatory, show=no, update=yes,
			override=yes, >> logfile)
	    	    fd2 = temp
	    	    while (fscan (fd2, arc) != EOF) {
	        	hedit (arc, "refspec1", arc, add=yes, verify=no,
		    	    show=no, update=yes)
	        	hedit (arc, "arctype", "shift", add=yes, verify=no,
		    	    show=no, update=yes)
	    	    }
	    	    fd2 = ""; delete (temp, verify=no)
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

		doarcs (spec, response, arcref1, arcref2, extn, arcreplace,
		    apidtable, arcaps, arcbeams, savearcs, reextract, arcap,
		    logfile, yes)

	        hselect (specms, "refspec1", yes, > temp)
	        fd2 = temp
	        i = fscan (fd2, arc)
	        fd2 = ""; delete (temp, verify=no)
		if (i < 1)
		    print ("No arc reference assigned for ", spec, >> logfile)
		else {
	            print ("Dispersion correct ", spec, >> logfile)
		    dispcor (specms, "", linearize=params.linearize,
			database=database, table=arcref1//extn, w1=INDEF,
			w2=INDEF, dw=INDEF, nw=INDEF, log=params.log,
			flux=params.flux, samedisp=no, global=no,
			ignoreaps=no, confirm=no, listonly=no, verbose=no,
			logfile=logfile)
		    hedit (specms, "dc-flag", 0, add=yes, verify=no,
			show=no, update=yes)
		    if (params.nsubaps > 1) {
			imrename (specms, temp, verbose=no)
			scopy (temp, specms, w1=INDEF, w2=INDEF,
			    apertures="-999", beams="", apmodulus=0, offset=0,
			    format="multispec", clobber=no, merge=no,
			    renumber=no, verbose=no)
			blkavg (temp, temp, 1, params.nsubaps, option="sum")
			imcopy (temp, specms//"[*,*]", verbose=no)
			imdelete (temp, verify=no)
		    }
		    disp = no
		}
	    }

	    if (sky && !disp) {
		str = ""
		if (skyaps != "")
		    str = "skyaps=" // skyaps
		if (skybeams != "")
		    str = str // " skybeams=" // skybeams
	        print ("Sky subtract ", spec, ": ", str, >> logfile)
		skysub (specms, output="", objaps=objaps, skyaps=skyaps,
		    objbeams=objbeams, skybeams=skybeams, skyedit=no,
		    combine=params.combine, reject=params.reject,
		    scale=params.scale, saveskys=saveskys, logfile=logfile)
	        hedit (specms, "skysub", str, add=yes, show=no, verify=no,
		    update=yes)
	    }
	}
	fd1 = ""; delete (objs, verify=no)

	if (access (done))
	    delete (done, verify=no)

	flprcache (0)
end
