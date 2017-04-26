# BATCH -- Process spectra in batch.
# This task is called in batch mode.  It only processes objects
# not previously processed unless the update or redo flags are set.

procedure batch ()

string	objects		{prompt="Object spectra"}
real	datamax		{prompt="Max data value / cosmic ray threshold"}

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
bool	scattered	{prompt="Subtracted scattered light?"}
bool	arcap		{prompt="Use object apertures for arcs?"}
bool	dispcor		{prompt="Dispersion correct spectra?"}
bool	savearcs	{prompt="Save internal arcs?"}
bool	skyalign	{prompt="Align sky lines?"}
bool	skysubtract	{prompt="Subtract sky?"}
bool	saveskys	{prompt="Save sky spectra?\n"}

bool	newaps, newresp, newdisp, newarcs

struct	*fd1, *fd2, *fd3

begin
	file	objs, temp, temp1, spec, specms, arc
	bool	reextract, extract, scat, disp, sky, log
	string	imtype, mstype, str, str2, str3, str4
	int	i

	imtype = "." // envget ("imtype")
	i = stridx (",", imtype)
	if (i > 0)
	    imtype = substr (imtype, 1, i-1)
	mstype = ".ms" // imtype

	objs = mktemp ("tmp$iraf")
	temp = mktemp ("tmp$iraf")
	temp1 = mktemp ("tmp$iraf")

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

	getspec (objects, objs)
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
	    if (!access (spec // imtype)) {
		printf ("Object spectrum not found - %s%s\nCheck setting of imtype\n", spec, imtype, >> logfile)
		next
	    }
	    specms = spec // mstype

	    scat = no
	    extract = no
	    disp = no
	    sky = no
	    if (scattered) {
		if (redo && access (spec//"noscat"//imtype)) {
		    imdelete (spec, verify=no)
		    imrename (spec//"noscat", spec)
		}
		hselect (spec, "apscatte", yes) | scan (str)
		if (nscan() == 0)
		    scat = yes
	    }
	    if (reextract || !access (specms) || (update && scat))
		extract = yes
	    else {
		hselect (specms, "dclog1", yes) | scan (str)
		if (nscan () == 1) {
		    extract = update && newdisp
		    if (update && !newdisp)
		        # We really should check if REFSPEC will assign
			# different reference spectra.
			;
		} else
		    disp = dispcor

		hselect (specms, "skysub", yes) | scan (str)
		if (nscan() == 0)
		    sky = skysubtract
	    }
		
	    if (extract) {
		disp = dispcor
		sky = skysubtract
	    }

	    if (extract) {
		if (access (specms))
		    imdelete (specms, verify=no)
		if (scat) {
		    print ("Subtract scattered light from ", spec, >> logfile)
		    imrename (spec, spec//"noscat")
		    apscript (spec//"noscat", output=spec, ansextract="NO",
			ansscat="YES", anssmooth="YES", verbose=no)
		}
		print ("Extract object spectrum ", spec, >> logfile)
		hselect (spec, "date-obs,ut,exptime", yes, > temp1)
		hselect (spec, "ra,dec,epoch,st", yes, >> temp1)
		fd3 = temp1
		if (fscan (fd3, str, str2, str3) == 3) {
		    setjd (spec, observatory=observatory, date="date-obs",
			time="ut", exposure="exptime", jd="jd", hjd="",
			ljd="ljd", utdate=yes, uttime=yes, listonly=no,
			>> logfile)
		    if (fscan (fd3, str, str2, str3, str4) == 4)
			setairmass (spec, intype="beginning",
			    outtype="effective", exposure="exptime",
			    observatory=observatory, show=no, update=yes,
			    override=yes, >> logfile)
		}
		fd3 = ""; delete (temp1, verify=no)
		apscript (spec, nsubaps=params.nsubaps, saturation=datamax,
		    verbose=no)
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

	    if (disp) {
		# Fix arc headers if necessary.
		if (newarcs) {
		    getspec (arcs1, temp)
	    	    fd2 = temp
	    	    while (fscan (fd2, arc) != EOF) {
			hselect (arc, "date-obs,ut,exptime", yes, > temp1)
			hselect (arc, "ra,dec,epoch,st", yes, >> temp1)
			fd3 = temp1
			if (fscan (fd3, str, str2, str3) == 3) {
			    setjd (arc, observatory=observatory, date="date-obs",
				time="ut", exposure="exptime", jd="jd", hjd="",
				ljd="ljd", utdate=yes, uttime=yes, listonly=no,
				>> logfile)
			    if (fscan (fd3, str, str2, str3, str4) == 4)
				setairmass (arc, intype="beginning",
				    outtype="effective", exposure="exptime",
				    observatory=observatory, show=no, update=yes,
				    override=yes, >> logfile)
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
			if (fscan (fd3, str, str2, str3) == 3) {
			    setjd (arc, observatory=observatory,
				date="date-obs", time="ut", exposure="exptime",
				jd="jd", hjd="", ljd="ljd", utdate=yes,
				uttime=yes, listonly=no, >> logfile)
			    if (fscan (fd3, str, str2, str3, str4) == 4)
				setairmass (arc, intype="beginning",
				    outtype="effective", exposure="exptime",
				    observatory=observatory, show=no,
				    update=yes, override=yes, >> logfile)

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
		    logfile, yes, done)

	        hselect (specms, "refspec1", yes, > temp)
	        fd2 = temp
	        i = fscan (fd2, arc)
	        fd2 = ""; delete (temp, verify=no)
		if (i < 1)
		    print ("No arc reference assigned for ", spec, >> logfile)
		else {
		    if (skyalign)
			doalign (spec, specms, "align"//extn//imtype,
			    arcref1//extn, logfile, yes)
	            print ("Dispersion correct ", spec, >> logfile)
		    dispcor (specms, "", linearize=params.linearize,
			database=database, table=arcref1//extn, w1=INDEF,
			w2=INDEF, dw=INDEF, nw=INDEF, log=params.log,
			flux=params.flux, samedisp=no, global=no,
			ignoreaps=no, confirm=no, listonly=no, verbose=no,
			logfile=logfile)
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
