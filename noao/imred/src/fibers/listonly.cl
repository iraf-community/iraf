# LISTONLY -- List processing to be done.
#
# This follows pretty much the same logic as the full procedure but doesn't
# do anything but list the operations.

procedure listonly (objects, apref, flat, throughput, arcs1, arcs2, dispcor,
	skysubtract, redo, update)

string	objects = ""		{prompt="List of object spectra"}
file	apref = ""		{prompt="Aperture reference spectrum"}
file	flat = ""		{prompt="Flat field spectrum"}
file	throughput = ""		{prompt="Throughput file or image"}
string	arcs1 = ""		{prompt="List of arc spectra"}
string	arcs2 = ""		{prompt="List of shift arc spectra"}

bool	dispcor = yes		{prompt="Dispersion correct spectra?"}
bool	skysubtract = yes	{prompt="Subtract sky?"}
bool	redo = no		{prompt="Redo operations if previously done?"}
bool	update = yes		{prompt="Update spectra if cal data changes?"}

struct	*fd1
struct	*fd2

begin
	string	spec, arcref1, arcref2
	string	specms, arcref1ms, arcref2ms, response
	string	objs, temp, done, str
	bool	reextract, newaps, newresp, newdisp, extract, disp, sky
	int	i, dc

	objs = mktemp ("tmp$iraf")
	temp = mktemp ("tmp$iraf")
	done = mktemp ("tmp$iraf")

	newaps = no
	newresp = no
	newdisp = no

	i = strlen (apref)
	if (i > 4 && substr (apref, i-3, i) == ".imh")
	    apref = substr (apref, 1, i-4)

	reextract = redo
	if (reextract || !access (database // "/ap" // apref)) {
	    print ("Set reference aperture for ", apref)
	    newaps = yes
	}

	if (flat != "") {
	    response = flat
	    i = strlen (response)
	    if (i > 4 && substr (response, i-3, i) == ".imh")
	        response = substr (response, 1, i-4)
	    spec = throughput
	    i = strlen (spec)
	    if (i > 4 && substr (spec, i-3, i) == ".imh")
	        spec = substr (spec, 1, i-4)
	    response = response // spec // "norm.ms"

	    reextract = redo || (update && newaps)
	    if (reextract || !access (response // ".imh")) {
	        print ("Create response function ", response)
	        newresp = yes
	    }
	}

	if (dispcor) {
	    getspec (arcs1, > temp)
	    fd1 = temp
	    if (fscan (fd1, arcref1) == EOF)
		error (1, "No reference arcs")
	    fd1 = ""; delete (temp, verify=no)
	    arcref1ms = arcref1 // ".ms.imh"

	    getspec (arcs2, > temp)
	    fd1 = temp
	    if (fscan (fd1, arcref2) == EOF)
		arcref2 = ""
	    fd1 = ""; delete (temp, verify=no)
	    arcref2ms = arcref2 // ".ms.imh"

	    reextract = redo || (update && newaps)
	    if (reextract || !access (arcref1ms)) {
	        print ("Extract arc reference image ", arcref1)
	        print ("Determine dispersion solution for ", arcref1)
	        newdisp = yes
	    } else {
	        hselect (arcref1ms, "dc-flag", yes, > temp)
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
	        if (reextract || !access (arcref2ms) || newdisp) {
		    print ("Extract shift arc reference image ", arcref2)
	            print ("Determine dispersion solution for ", arcref2)
		} else {
		    hselect (arcref2ms, "dc-flag", yes, > temp)
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

	    specms = spec // ".ms.imh"

	    extract = no
	    disp = no
	    sky = no
	    if (reextract || !access (specms))
		extract = yes
	    else {
		hselect (specms, "dc-flag", yes, > temp)
		fd2 = temp
		extract = update && newaps
		if (fscan (fd2, str) == 1)
		    extract = update && newdisp
		else
		    disp = yes
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
