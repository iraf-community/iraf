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

procedure sfluxcal (stds, arcs1, arcref1, arcrefs, redo, update, extcor,
	done, log1, log2)

file	stds
file	arcs1
file	arcref1
string	arcrefs
bool	redo
bool	update
bool	extcor
file	done
file	log1
file	log2

struct	*fd1, *fd2, *fd3

begin
	string	imtype, mstype
	string	spec, specms, arc, str1, str2, str3, str4
	file	temp1, temp2
	int	i, j
	bool	reextract, log

	imtype = "." // envget ("imtype")
	i = stridx (",", imtype)
	if (i > 0)
	    imtype = substr (imtype, 1, i-1)
	mstype = ".ms" // imtype

	temp1 = mktemp ("tmp$iraf")
	temp2 = mktemp ("tmp$iraf")

	reextract = redo || (update && sproc.newdisp)
	sproc.newsens = no

	if (redo && access ("std"))
	    delete ("std", verify=no)

        fd1 = stds
        while (fscan (fd1, spec) != EOF) {
	    specms = spec // mstype

	    if (reextract && access (specms))
	        imdelete (specms, verify=no)
            if (!access (specms)) {
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

	    hselect (specms, "dispcor,std-flag", yes, > temp1)
	    fd2 = temp1
	    j = fscan (fd2, str1, str2)
	    fd2 = ""; delete (temp1, verify=no)
	    if (j < 1) {
		# Fix arc headers if necessary.
		if (sproc.newarcs) {
	    	    fd2 = arcs1
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

		sdoarcs (spec, arcref1, reextract, log1, no)

	        hselect (specms, "refspec1", yes, > temp1)
	        fd2 = temp1
	        i = fscan (fd2, arc)
	        fd2 = ""; delete (temp1, verify=no)
		if (i < 1) {
		    print ("No arc reference assigned for ", spec) | tee (log1)
		    next
		} else {
	            print ("Dispersion correct ", spec) | tee (log1)
		    dispcor (specms, "", linearize=sparams.linearize,
			database=database, table=arcref1//mstype,
			w1=INDEF, w2=INDEF, dw=INDEF, nw=INDEF,
			log=sparams.log, flux=sparams.flux, samedisp=no,
			global=no, ignoreaps=no, confirm=no, listonly=no,
			logfile=logfile, > log2)
		    flpr
		    hedit (specms, "dispcor", 0, add=yes, verify=no,
			show=no, update=yes)
		}
	    }

	    if (j < 2 || !access ("std")) {
                print ("Compile standard star fluxes for ", spec) | tee (log1)
	        standard (specms, output="std", samestar=no, beam_switch=no,
		    apertures="", bandwidth=INDEF, bandsep=INDEF,
		    fnuzero=3.68E-20, extinction=extinction, caldir=caldir,
		    observatory=observatory, interact=no)
	        hedit (specms, "std-flag", "yes", add=yes, verify=no,
		    show=no, update=yes)
	        print (specms, >> temp2)
	        sproc.newsens = yes
            }
        }
        fd1 = ""

	if (sproc.newsens || !access ("sens"//imtype)) {
	    if (!access ("std")) {
	        print ("No standard star data") | tee (log1)
	        sproc.fluxcal1 = no
	    } else {
	        imdelete ("sens"//imtype, verify=no, >& "dev$null")
                print ("Compute sensitivity function") | tee (log1)
                sensfunc ("std", "sens", apertures="", ignoreaps=yes,
		    logfile=logfile, extinction=extinction,
		    newextinction="extinct.dat", observatory=observatory,
		    function=sparams.s_function, order=sparams.s_order,
		    interactive=yes, graphs="sr", marks="plus cross box")
	        sproc.newsens = yes
	    }
        }

        # Note that if new standard stars are added the old standard
        # stars are not recalibrated unless the redo flag is used.

        if (sproc.fluxcal1 && sproc.newsens && access (temp2)) {
            print ("Flux and/or extinction calibrate standard stars") |
	        tee (log1)
	    calibrate ("@"//temp2, "", extinct=extcor, flux=sproc.fluxcal1,
		extinction=extinction, observatory=observatory, ignoreaps=yes,
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
