# SLISTONLY -- List processing to be done.
#
# This follows pretty much the same logic as the full procedure but doesn't
# do anything but list the operations.

procedure slistonly (objects, apref, arcs, standards, scattered, dispcor,
	extcor, fluxcal, redo, update)

string	objects
file	apref
string	arcs
string	standards

bool	scattered
bool	dispcor
bool	extcor
bool	fluxcal
bool	redo
bool	update

struct	*fd1
struct	*fd2

begin
	string	imtype, ectype
	string	spec, arcref
	string	specec, arcrefec
	string	temp1, temp2, done, str
	bool	newaps, newdisp, newsens
	bool	extract, disp, ext, flux, scat, reextract, fluxcal1, stdfile
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
	newdisp = no
	newsens = no
	fluxcal1 = fluxcal

	i = strlen (apref)
	if (i > n && substr (apref, i-n+1, i) == imtype)
	    apref = substr (apref, 1, i-n)

	reextract = redo
	if (reextract || !access (database // "/ap" // apref)) {
	    print ("Set reference aperture for ", apref)
	    newaps = yes
	}

	scat = no
	if (scattered) {
	    hselect (apref, "apscatte", yes, > temp1)
	    fd1 = temp1
	    if (fscan (fd1, str1) < 1)
		scat = yes
	    fd1 = ""; delete (temp1, verify=no)
	}
	if (scat)
	    print ("Subtract scattered light in ", apref) | tee (log1)

	if (dispcor) {
	    hselect (arcs, "$I,wat0_001", yes, > temp1)
	    fd1 = temp1; s1 = ""
	    i = fscanf (fd1, "%s\tsystem=%s", arcref, s1)
	    if (i < 1 || (i == 2 && (s1 == "equispec" || s1 == "multispec")))
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

	    if (fluxcal1) {
		stdfile = access ("std")
		if (redo && stdfile)
		    stdfile = no

	        reextract = redo || (update && (newaps || newdisp))
		hselect (standards, "$I,ctype1", yes, >temp1)
	        fd1 = temp1
	        while (fscan (fd1, spec, s1) != EOF) {
		    if (nscan() == 2 && s1 == "MULTISPE")
			next
		    i = strlen (spec)
		    if (i > n && substr (spec, i-n+1, i) == imtype)
		        spec = substr (spec, 1, i-n)
		    specec = spec // ectype

		    scat = no
		    if (scattered) {
			hselect (spec, "apscatte", yes, > temp2)
			fd2 = temp2
			if (fscan (fd2, str) < 1)
			    scat = yes
			fd2 = ""; delete (temp2, verify=no)
		    }
	            if (reextract || !access (specec) || (update && scat)) {
			if (scat)
			    print ("Subtract scattered light from ", spec)
		        print ("Extract standard star spectrum ", spec)
	                print ("Dispersion correct ", spec)
	                print ("Compile standard star fluxes for ", spec)
		        stdfile = yes
		        newsens = yes
		    } else {
		        hselect (specec, "dc-flag,std-flag", yes, > temp2)
		        fd2 = temp2
		        i = fscan (fd2, str1, str2)
		        fd2 = ""; delete (temp2, verify=no)
		        if (i < 1)
		            print ("Dispersion correct ", spec)
		        if (i < 2) {
		            print ("Compile standard star fluxes for ", spec)
		            stdfile = yes
		            newsens = yes
		        }
	            }
		    print (spec, >> done)
	        }
	        fd1 = ""; delete (temp1, verify=no)

	        sections ("sens.????"//imtype, option="nolist")
	        if (newsens || sections.nimages == 0) {
		    if (!stdfile) {
		        print ("No standard stars")
		        fluxcal1 = no
		    } else {
	                print ("Compute sensitivity function")
		        newsens = yes
		    }
	        }

	        if (fluxcal1 && newsens)
	            print ("Flux and/or extinction calibrate standard stars")
	    }
	}

	reextract = redo || (update && (newaps || newdisp))
	hselect (objects, "$I,ctype1", yes, > temp1)
	fd1 = temp1
	while (fscan (fd1, spec, s1) != EOF) {
	    if (nscan() == 2 && s1 == "MULTISPE")
		next
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
	    ext = no
	    flux = no
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
		hselect (specec, "ex-flag", yes, >> temp2)
		hselect (specec, "ca-flag", yes, >> temp2)
		fd2 = temp2
		extract = update && newaps
		if (fscan (fd2, str1) == 1)
		    extract = update && newdisp
		else
		    disp = yes
		if (fscan (fd2, str1) == 1)
		    extract = update && !extcor
		else
		    ext = extcor
		if (fscan (fd2, str1) == 1)
		    extract = update && (!fluxcal1 || newsens)
		else
		    flux = fluxcal1
		fd2 = ""; delete (temp2, verify=no)
	    }
		
	    if (extract) {
		disp = dispcor
		ext = extcor
		flux = fluxcal1
	    }
		    
	    if (scat)
		print ("Subtract scattered light from ", spec)
	    if (extract)
		print ("Extract object spectrum ", spec)
	    if (disp)
	        print ("Dispersion correct ", spec)
	    if (ext)
		print ("Extinction correct ", spec)
	    if (flux)
		print ("Flux calibrate ", spec)
	}
	fd1 = ""; delete (temp1, verify=no)

	if (access (done))
	    delete (done, verify=no)
end
