# LISTONLY -- List processing to be done.
#
# This follows pretty much the same logic as the full procedure but doesn't
# do anything but list the operations.

procedure listonly (objects, apref, flat, arcs, scattered, dispcor,
	redo, update)

string	objects = ""		{prompt="List of object spectra"}
file	apref = ""		{prompt="Aperture reference spectrum"}
file	flat = ""		{prompt="Flat field spectrum"}
string	arcs = ""		{prompt="List of arc spectra"}

bool	scattered		{prompt="Subtract scattered light?"}
bool	dispcor			{prompt="Dispersion correct spectra?"}
bool	redo			{prompt="Redo operations if previously done?"}
bool	update			{prompt="Update spectra if cal data changes?"}

struct	*fd1
struct	*fd2

begin
	string	imtype, ectype
	string	spec, arcref
	string	specec, arcrefec, response
	string	temp1, temp2, done, str
	bool	reextract, newaps, newresp, newdisp, extract, disp, scat
	int	i, j, n

	imtype = "." // envget ("imtype")
	i = stridx (",", imtype)
	if (i > 0)
	    imtype = substr (imtype, 1, i-1)
	ectype = ".ec" // imtype
	n = strlen (imtype)

	temp1 = mktemp ("tmp$iraf")
	temp2 = mktemp ("tmp$iraf")
	done = mktemp ("tmp$iraf")

	newaps = no
	newresp = no
	newdisp = no

	i = strlen (apref)
	if (i > n && substr (apref, i-n+1, i) == imtype)
	    apref = substr (apref, 1, i-n)

	reextract = redo
	if (reextract || !access (database // "/ap" // apref)) {
	    print ("Set reference aperture for ", apref)
	    newaps = yes
	}

	if (flat != "") {
	    response = flat
	    i = strlen (response)
	    if (i > n && substr (response, i-n+1, i) == imtype)
	        response = substr (response, 1, i-n)
	    response = response // "norm.ec"

	    reextract = redo || (update && newaps)
	    scat = no
	    if (scattered) {
		hselect (flat, "apscatte", yes, > temp2)
		fd2 = temp2
		if (fscan (fd2, str) < 1)
		    scat = yes
		fd2 = ""; delete (temp2, verify=no)
	    }
	    if (reextract || !access (response // imtype) || (update && scat)) {
		if (scat)
		    print ("Subtract scattered light from ", flat)
	        print ("Create response function ", response)
	        newresp = yes
	    }
	}

	if (dispcor) {
	    hselect (arcs, "$I", yes, > temp1)
	    #sections (arcs, option="fullname", > temp1)
	    fd1 = temp1
	    i = fscan (fd1, arcref)
	    if (i < 1)
		error (1, "No reference arcs")
	    fd1 = ""; delete (temp1, verify=no)
	    i = strlen (arcref)
	    if (i > n && substr (arcref, i-n+1, i) == imtype)
	        arcref = substr (arcref, 1, i-n)
	    arcrefec = arcref // ectype

	    reextract = redo || (update && newaps)
	    if (reextract || !access (arcrefec)) {
	        print ("Extract arc reference image ", arcref)
	        print ("Determine dispersion solution for ", arcref)
	        newdisp = yes
	    } else {
		hselect (arcrefec, "refspec1,dc-flag", yes, > temp1)
	        fd1 = temp1
	        i = fscan (fd1, str, j)
	        fd1 = ""; delete (temp1, verify=no)
	        if (i < 1) {
	            print ("Determine dispersion solution for ", arcref)
	            newdisp = yes
	        }
	    }
	    print (arcref, > done)
	}

	reextract = redo || (update && (newaps || newresp || newdisp))
	hselect (objects, "$I", yes, > temp1)
	#sections (objects, option="fullname", > temp1)
	fd1 = temp1
	while (fscan (fd1, spec) != EOF) {
	    if (i > n && substr (spec, i-n+1, i) == imtype)
	        spec = substr (spec, 1, i-n)

	    if (access (done)) {
	        fd2 = done
	        while (fscan (fd2, specec) != EOF)
		    if (spec == specec)
		        break
	        if (spec == specec)
		    next
	        fd2 = ""
	    }

	    specec = spec // ectype

	    scat = no
	    extract = no
	    disp = no
	    if (scattered) {
		hselect (spec, "apscatte", yes, > temp2)
		fd2 = temp2
		if (fscan (fd2, str) < 1)
		    scat = yes
		fd2 = ""; delete (temp2, verify=no)
	    }
	    if (reextract || !access (specec) || (update && scat)) {
		extract = yes
	    } else {
		hselect (specec, "dc-flag", yes, > temp2)
		fd2 = temp2
		extract = update && newaps
		if (fscan (fd2, str) == 1)
		    extract = update && newdisp
		else
		    disp = yes
		fd2 = ""; delete (temp2, verify=no)
	    }
		
	    if (extract)
		disp = dispcor
		    
	    if (scat)
		print ("Subtract scattered light from ", spec)
	    if (extract)
		print ("Extract object spectrum ", spec)
	    if (disp)
	        print ("Dispersion correct ", spec)
	}
	fd1 = ""; delete (temp1, verify=no)

	if (access (done))
	    delete (done, verify=no)
end
