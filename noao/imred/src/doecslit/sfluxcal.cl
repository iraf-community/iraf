# SFLUXCAL -- Extract standard stars and determine sensitivity function.
# If flux calibrating, extract and dispersion correct the standard star
# spectra.  Compile the standard star fluxes from the calibration
# directory.  The user is queried for the star name but the band passes
# are not allow to change interactively.  Next compute the sensitivity
# function using SENSFUNC.  This is interactive.  Once the sensitivity
# function images are created, flux and extinction calibrate the standard
# stars.  This is done in such a way that if new standard stars are added
# in a later execution only the new stars are added and then a new
# sensitivity function is computed.  If the update flag is set all
# spectra which are specified are reprocessed if they were previously
# processed.  In a redo the "std" file is deleted, otherwise additions
# are appended to this file.

procedure sfluxcal (stds, arcs, arcref, arcrefs, redo, update,
	scattered, arcap, extcor, done, log1, log2)

file	stds
file	arcs
file	arcref
string	arcrefs
bool	redo
bool	update
bool	scattered
bool	arcap
bool	extcor
file	done
file	log1
file	log2

struct	*fd1, *fd2, *fd3

begin
	string	imtype, ectype
	string	spec, specec, arc, str1, str2, str3, str4
	file	temp1, temp2
	int	i, j
	bool	reextract, log, scat
	str1 = ""
	str2 = ""

	imtype = "." // envget ("imtype")
	i = stridx (",", imtype)
	if (i > 0)
	    imtype = substr (imtype, 1, i-1)
	ectype = ".ec" // imtype

	temp1 = mktemp ("tmp$iraf")
	temp2 = mktemp ("tmp$iraf")

	reextract = redo || (update && (sproc.newaps || sproc.newdisp))
	sproc.newsens = no

	if (redo && access ("std"))
	    delete ("std", verify=no)

        fd1 = stds
        while (fscan (fd1, spec) != EOF) {
	    specec = spec // ectype

	    scat = no
	    if (scattered) {
		hselect (spec, "apscatte", yes, > temp1)
		fd2 = temp1
		if (fscan (fd2, str1) < 1)
		    scat = yes
		fd2 = ""; delete (temp1, verify=no)
	    }
	    if (reextract || !access (specec) || (update && scat)) {
		if (access (specec))
		    imdelete (specec, verify=no)

		if (scat) {
		    print ("Subtract scattered light in ", spec) | tee (log1)
		    apslitproc (spec, ansextract="NO", ansscat="YES")
		}

	        print ("Extract standard star spectrum ", spec) | tee (log1)
		hselect (spec, "date-obs,ut,exptime", yes, > temp1)
		hselect (spec, "ra,dec,epoch,st", yes, >> temp1)
		fd2 = temp1
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
		fd2 = ""; delete (temp1, verify=no)
		apslitproc (spec)
	    }

	    hselect (specec, "dc-flag,std-flag", yes, > temp1)
	    fd2 = temp1
	    j = fscan (fd2, str1, str2)
	    fd2 = ""; delete (temp1, verify=no)
	    if (j < 1) {
		# Fix arc headers if necessary.
		if (sproc.newarcs) {
	    	    fd2 = arcs
	    	    while (fscan (fd2, arc) != EOF) {
			hselect (arc, "date-obs,ut,exptime", yes, > temp1)
			hselect (arc, "ra,dec,epoch,st", yes, >> temp1)
			fd3 = temp1
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
			fd3 = ""; delete (temp1, verify=no)
	        	hedit (arc, "refspec1", arc, add=yes, verify=no,
		    	    show=no, update=yes)
		    }
	    	    fd2 = ""
		    sproc.newarcs = no
		}

		print ("Assign arc spectra for ", spec) | tee (log1)
		refspectra (spec, references=arcrefs,
		    apertures="", refaps="", ignoreaps=no,
		    select=sparams.select, sort=sparams.sort,
		    group=sparams.group, time=sparams.time,
		    timewrap=sparams.timewrap, override=yes, confirm=no,
		    assign=yes, logfiles="STDOUT", verbose=no) |
		    tee (log1, > log2)

		sdoarcs (spec, arcref, reextract, arcap, log1, no)

	        hselect (specec, "refspec1", yes, > temp1)
	        fd2 = temp1
	        i = fscan (fd2, arc)
	        fd2 = ""; delete (temp1, verify=no)
		if (i < 1) {
		    print ("No arc reference assigned for ", spec) | tee (log1)
		    next
		} else {
	            print ("Dispersion correct ", spec) | tee (log1)
		    dispcor (specec, "", linearize=sparams.linearize,
			database=database, table=arcref//ectype,
			w1=INDEF, w2=INDEF, dw=INDEF, nw=INDEF, log=sparams.log,
			flux=sparams.flux, global=no, ignoreaps=no, confirm=no,
			listonly=no, logfile=logfile)
		    hedit (specec, "dc-flag", 0, add=yes, show=no,
			verify=no, update=yes)
		}
	    }

	    if (j < 2 || !access ("std")) {
                print ("Compile standard star fluxes for ", spec) | tee (log1)
	        standard (specec, output="std", samestar=yes, beam_switch=no,
		    apertures="", bandwidth=sparams.bandwidth,
		    bandsep=sparams.bandsep, fnuzero=3.68E-20,
		    extinction=extinction, caldir=caldir,
		    observatory=observatory, interact=sparams.s_interact)
	        hedit (specec, "std-flag", "yes", add=yes, verify=no,
		    show=no, update=yes)
	        print (specec, >> temp2)
	        sproc.newsens = yes
            }
        }
        fd1 = ""

        sections ("sens.????"//imtype, option="nolist")
        if (sproc.newsens || sections.nimages == 0) {
	    if (!access ("std")) {
	        print ("No standard star data") | tee (log1)
	        sproc.fluxcal1 = no
	    } else {
	        imdelete ("sens.????"//imtype, verify=no)
                print ("Compute sensitivity function") | tee (log1)
                sensfunc ("std", "sens", apertures="", ignoreaps=no,
		    logfile=logfile, extinction=extinction,
		    newextinction="extinct.dat", observatory=observatory,
		    function=sparams.s_function, order=sparams.s_order,
		    interactive=yes, graphs="sr", marks="plus cross box")
	        sproc.newsens = yes
	    }
        }

        # Note that if new standard stars are added the old standard
        # stars are not recalibrated unless the redo flag is used.

        if (sproc.fluxcal1 && sproc.newsens) {
            print ("Flux and/or extinction calibrate standard stars") |
	        tee (log1)
	    calibrate ("@"//temp2, "", extinct=extcor, flux=sproc.fluxcal1,
		extinction=extinction, observatory=observatory, ignoreaps=no,
		sensitivity="sens", fnu=sparams.fnu) | tee (log1, > log2)
	    if (sproc.splot1) {
	        print ("Standard stars:")
	        str1 = sproc.anssplot
	        if (str1 == "NO" || str1 == "YES")
		    sproc.splot1 = no
	        if (str1 == "no" || str1 == "NO")
		    sproc.splot2 = no
	        else
		    sproc.splot2 = yes
	    }
	    if (sproc.splot2)
		splot ("@"//temp2)
	    sections (temp2, option="fullname", >> done)
            delete (temp2, verify=no)
        }
end
