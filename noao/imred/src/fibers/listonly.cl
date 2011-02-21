# LISTONLY -- List processing to be done.
#
# This follows pretty much the same logic as the full procedure but doesn't
# do anything but list the operations.

procedure listonly (objects, apidtable, apref, flat, throughput, arcs1, arcs2,
	scattered, dispcor, skysubtract, redo, update)

string	objects = ""		{prompt="List of object spectra"}
file	apidtable = ""		{prompt="Aperture ID table"}
file	apref = ""		{prompt="Aperture reference spectrum"}
file	flat = ""		{prompt="Flat field spectrum"}
file	throughput = ""		{prompt="Throughput file or image"}
string	arcs1 = ""		{prompt="List of arc spectra"}
string	arcs2 = ""		{prompt="List of shift arc spectra"}

bool	scattered		{prompt="Subtract scattered light?"}
bool	dispcor			{prompt="Dispersion correct spectra?"}
bool	skysubtract		{prompt="Subtract sky?"}
bool	redo = no		{prompt="Redo operations if previously done?"}
bool	update = yes		{prompt="Update spectra if cal data changes?"}

struct	*fd1
struct	*fd2

begin
	string	imtype, mstype, extn
	string	spec, arcref1, arcref2
	string	specms, arcref1ms, arcref2ms, response
	string	objs, temp, done, str
	bool	reextract, newaps, newresp, newdisp, scat, extract, disp, sky
	int	i, j, n, dc

	imtype = "." // envget ("imtype")
	i = stridx (",", imtype)
	if (i > 0)
	    imtype = substr (imtype, 1, i-1)
	mstype = ".ms" // imtype
	n = strlen (imtype)

	objs = mktemp ("tmp$iraf")
	temp = mktemp ("tmp$iraf")
	done = mktemp ("tmp$iraf")

	if (apidtable != "") {
	    j = strlen (apidtable)
	    for (i=1; i<=j && substr(apidtable,i,i)==" "; i+=1);
	    apidtable = substr (apidtable, i, j)
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

	newaps = no
	newresp = no
	newdisp = no

	i = strlen (apref)
	if (i > n && substr (apref, i-n+1, i) == imtype)
	    apref = substr (apref, 1, i-n)

	reextract = redo
	if (reextract || !access (database // "/ap" // apref // extn)) {
	    print ("Set reference aperture for ", apref)
	    newaps = yes
	}

	i = strlen (flat)
	if (i > n && substr (flat, i-n+1, i) == imtype)
	    flat = substr (flat, 1, i-n)
	if (flat != "") {
	    scat = no
	    if (scattered) {
		if (redo && access (flat//"noscat"//imtype))
		    hselect (flat//"noscat", "apscatte", yes) | scan (str)
		else
		    hselect (flat, "apscatte", yes) | scan (str)
		if (nscan() == 0)
		    scat = yes
	    }
	    if (scat)
		print ("Subtract scattered light from ", flat)
	}

	spec = throughput
	i = strlen (spec)
	if (i > n && substr (spec, i-n+1, i) == imtype)
	    spec = substr (spec, 1, i-n)
	if (spec != "") {
	    scat = no
	    if (scattered) {
		if (redo && access (flat//"noscat"//imtype))
		    hselect (flat//"noscat", "apscatte", yes) | scan (str)
		else
		    hselect (flat, "apscatte", yes) | scan (str)
		if (nscan() == 0)
		    scat = yes
	    }
	    if (scat)
		print ("Subtract scattered light from ", spec)
	}

	response = ""
	if (flat != "" || spec != "") {
	    if (extn == ".ms")
		response = flat // spec // "norm.ms"
	    else
		response = flat // spec // extn

	    reextract = redo || (update && newaps)
	    if (reextract || !access (response // imtype) || (redo && scat)) {
	        print ("Create response function ", response)
	        newresp = yes
	    }
	}

	if (dispcor) {
	    getspec (arcs1, temp)
	    fd1 = temp
	    if (fscan (fd1, arcref1) == EOF)
		error (1, "No reference arcs")
	    fd1 = ""; delete (temp, verify=no)
	    arcref1ms = arcref1 // extn

	    getspec (arcs2, temp)
	    fd1 = temp
	    if (fscan (fd1, arcref2) == EOF)
		arcref2 = ""
	    fd1 = ""; delete (temp, verify=no)
	    arcref2ms = arcref2 // extn

	    reextract = redo || (update && newaps)
	    if (reextract || !access (arcref1ms//imtype)) {
	        print ("Extract arc reference image ", arcref1)
	        print ("Determine dispersion solution for ", arcref1)
	        newdisp = yes
	    } else {
	        hselect (arcref1ms, "dclog1", yes, > temp)
	        fd1 = temp
		dc = -1
	        i = fscan (fd1, dc)
	        fd1 = ""; delete (temp, verify=no)
	        if (i < 1) {
	            print ("Determine dispersion solution for ", arcref1)
	            newdisp = yes
	        }
	    }
	    print (arcref1, > done)

	    if (arcref2 != "") {
	        if (reextract || !access (arcref2ms//imtype) || newdisp) {
		    print ("Extract shift arc reference image ", arcref2)
	            print ("Determine dispersion solution for ", arcref2)
		} else {
		    hselect (arcref2ms, "dclog1", yes, > temp)
		    fd1 = temp
		    dc = -1
		    i = fscan (fd1, dc)
		    fd1 = ""; delete (temp, verify=no)
		    if (i < 1)
	                print ("Determine dispersion solution for ", arcref2)
		}
	    }
	    print (arcref2, >> done)
	}

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

	    specms = spec // mstype

	    scat = no
	    extract = no
	    disp = no
	    sky = no
	    if (scattered) {
		if (redo && access (spec//"noscat"//imtype))
		    hselect (spec//"noscat", "apscatte", yes) | scan (str)
		else
		    hselect (spec, "apscatte", yes) | scan (str)
		if (nscan() == 0)
		    scat = yes
	    }
	    if (reextract || !access (specms) || (redo && scat))
		extract = yes
	    else {
		hselect (specms, "dclog1", yes) | scan (str)
		if (nscan() == 0)
		    disp = yes
		else
		    extract = update && newdisp
		hselect (specms, "skysub", yes) | scan (str)
		if (nscan() == 0)
		    sky = skysubtract
	    }
		
	    if (extract) {
		disp = dispcor
		sky = skysubtract
	    }
		    
	    if (scat)
		print ("Subtract scattered light from ", spec)
	    if (extract)
		print ("Extract object spectrum ", spec)
	    if (disp)
	        print ("Dispersion correct ", spec)
	    if (sky)
		print ("Sky subtract ", spec)
	}
	fd1 = ""; delete (objs, verify=no)

	if (access (done))
	    delete (done, verify=no)
end
