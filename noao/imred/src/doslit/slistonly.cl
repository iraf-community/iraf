# SLISTONLY -- List processing to be done.
#
# This follows pretty much the same logic as the full procedure but doesn't
# do anything but list the operations.

procedure slistonly (objects, arcs1, standards, dispcor, extcor, fluxcal,
	redo, update)

file	objects
file	arcs1
file	standards

bool	dispcor
bool	extcor
bool	fluxcal
bool	redo
bool	update

struct	*fd1
struct	*fd2

begin
	string	imtype, mstype
	string	spec, arcref1
	string	specms, arcref1ms
	string	temp, done, str1, str2
	bool	reextract, fluxcal1, stdfile
	bool	newdisp, newsens, extract, disp, ext, flux
	int	i, dc, sf

	imtype = "." // envget ("imtype")
	i = stridx (",", imtype)
	if (i > 0)
	    imtype = substr (imtype, 1, i-1)
	mstype = ".ms" // imtype
	temp = mktemp ("tmp$iraf")
	done = mktemp ("tmp$iraf")

	newdisp = no
	newsens = no
	fluxcal1 = fluxcal

	print ("Check and set new object aperture definitions")

	if (dispcor) {
	    fd1 = arcs1
	    if (fscan (fd1, arcref1) == EOF)
		error (1, "No reference arcs")
	    fd1 = ""
	    arcref1ms = arcref1 // mstype

	    if (redo || !access (arcref1ms)) {
	        print ("Extract arc reference image ", arcref1)
	        print ("Determine dispersion solution for ", arcref1)
	        newdisp = yes
	    } else {
		hselect (arcref1ms, "dispcor", yes, > temp)
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

	    if (fluxcal1) {
		stdfile = access ("std")
		if (redo && stdfile)
		    stdfile = no

	        reextract = redo || (update && newdisp)
	        fd1 = standards
	        while (fscan (fd1, spec) != EOF) {
		    specms = spec // mstype
	            if (reextract || !access (specms)) {
		        print ("Extract standard star spectrum ", spec)
	                print ("Dispersion correct ", spec)
	                print ("Compile standard star fluxes for ", spec)
		        stdfile = yes
		        newsens = yes
		    } else {
		        hselect (specms, "dispcor,std-flag", yes, > temp)
		        fd2 = temp
			dc = -1
			sf = -1
		        i = fscan (fd2, dc, sf)
		        fd2 = ""; delete (temp, verify=no)
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
	        fd1 = ""

	        sections ("sens.????" // imtype, option="nolist")
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

	    specms = spec // mstype

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
		extract = update
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
		fd2 = ""; delete (temp, verify=no)
	    }
		
	    if (extract) {
		disp = dispcor
		ext = extcor
		flux = fluxcal1
	    }
		    
	    if (extract)
		print ("Extract object spectrum ", spec)
	    if (disp)
	        print ("Dispersion correct ", spec)
	    if (ext)
		print ("Extinction correct ", spec)
	    if (flux)
		print ("Flux calibrate ", spec)
	}
	fd1 = ""

	if (access (done))
	    delete (done, verify=no)
end
