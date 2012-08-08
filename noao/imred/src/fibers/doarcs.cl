# DOARCS -- Determine dispersion relation for spectrum based on reference arcs.
# This procedure is complicated by:
#    1. The need to reextract arcs if the objects spectra are being
#	recentered or retraced.
#    2. The use of shift spectra to track shifts in the dispersion from
#	the reference arc spectrum.
#    3. The use of multiple exposures to correct for illumination problems
#	in taking the arcs.

procedure doarcs (spec, response, arcref1, arcref2, extn, arcreplace, apidtable,
	arcaps, arcbeams, savearcs, reextract, arcap, logfile, batch, done)

file	spec
file	response
file	arcref1
file	arcref2
string	extn
file	arcreplace
file	apidtable
string	arcaps
string	arcbeams
bool	savearcs
bool	reextract
bool	arcap
file	logfile
bool	batch
file	done

struct	*fd

begin
	string	imtype
	int	i, j, k, n
	file	temp, arc1, arc2, str1, str2, arctype, apref, arc, arcms
	bool	verbose1

	imtype = "." // envget ("imtype")
	i = stridx (",", imtype)
	if (i > 0)
	    imtype = substr (imtype, 1, i-1)
	n = strlen (imtype)

	temp = mktemp ("tmp$iraf")

	if (batch)
	    verbose1 = no
	else
	    verbose1 = verbose

	for (j=1; j<=2; j+=1) {
	    # The reference spectra refer initially to the 2D image.  At the
	    # end we will reset them to refer to the 1D spectra.

	    hselect (spec, "refspec"//j, yes, > temp)
	    fd = temp
	    k = fscan (fd, arc1, str1)
	    fd = ""; delete (temp, verify=no)
	    if (k < 1)
		break

	    # Strip possible image extension.
	    i = strlen (arc1)
	    if (i > n && substr (arc1, i-n+1, i) == imtype)
		arc1 = substr (arc1, 1, i-n)
    
	    # Set extraction output and aperture reference depending on whether
	    # the arcs are to be rextracted using recentered or retraced object
	    # apertures.

	    if (arcap &&
		(apscript.ansrecenter=="yes" || apscript.anstrace=="yes" ||
		 apscript.ansrecenter=="YES" || apscript.anstrace=="YES")) {
		arc2 = spec // arc1 // ".ms"
		apref = spec
		if (access (arc2//imtype))
		    imdelete (arc2//imtype, verify=no)
		delete (database//"/id"//arc2//"*", verify = no)
	    } else {
		arc2 = arc1 // extn
		apref = apscript.references
		if (reextract && access (arc2//imtype)) {
		    if (arc2 != arcref1 // extn && arc2 != arcref2 // extn) {
			if (access (done)) {
			    fd = done
			    while (fscan (fd, arcms) != EOF)
				if (arcms == arc2)
				    break
			    fd = ""
			} else
			    arcms = ""
			if (arcms != arc2)
			    imdelete (arc2, verify=no)
		    }
		}
	    }

	    # SHIFT arcs are reidentified with only a shift.
	    # HENEAR arcs are reidentified using the user refit option.
	    #	Also internal arcs are checked if HENEAR.

	    hselect (arc1, "arctype", yes, > temp)
	    fd = temp
	    i = fscan (fd, arctype)
	    fd = ""; delete (temp, verify=no)
    
	    # Extract and determine dispersion function if necessary.
	    if (!access (arc2//imtype)) {
		delete (database//"/id"//arc2//"*", verify = no)
		if (!batch)
		    print ("Extract and reidentify arc spectrum ", arc1)
		print ("Extract and reidentify arc spectrum ", arc1, >> logfile)
		apscript (arc1, output=arc2, references=apref,
		    ansrecenter="NO", ansresize="NO", ansedit="NO",
		    anstrace="NO", nsubaps=params.nsubaps, background="none",
		    clean=no, weights="none", verbose=verbose1)
		sapertures (arc2, apertures="", apidtable=apidtable,
		    wcsreset=no, verbose=no, beam=INDEF, dtype=INDEF, w1=INDEF,
		    dw=INDEF, z=INDEF, aplow=INDEF, aphigh=INDEF, title=INDEF)
		if (response != "") {
		    if (params.nsubaps == 1)
			sarith (arc2, "/", response, arc2, w1=INDEF, w2=INDEF,
			    apertures="", bands="", beams="", apmodulus=0,
			    reverse=no, ignoreaps=no, format="multispec",
			    renumber=no, offset=0, clobber=yes, merge=no,
			    errval=0, verbose=no)
		    else {
			blkrep (response, temp, 1, params.nsubaps)
			sarith (arc2, "/", temp, arc2, w1=INDEF, w2=INDEF,
			    apertures="", bands="", beams="", apmodulus=0,
			    reverse=no, ignoreaps=yes, format="multispec",
			    renumber=no, offset=0, clobber=yes, merge=no,
			    errval=0, verbose=no)
			imdelete (temp, verify=no)
		    }
		}
		print (arc2, >> done)

		if (arctype == "shift") {
		    reidentify (arcref2//extn, arc2,
			interactive=no, section="middle line", shift=0.,
			step=1, nsum=1, cradius=params.cradius,
			threshold=params.threshold, nlost=100, newaps=no,
			refit=no, trace=no, override=no, addfeatures=no,
			database=database, plotfile=plotfile,
			logfiles=logfile, verbose=verbose1)
		} else {
		    if (arcreplace != "") {
			fd = arcreplace
			while (fscan (fd, arc, arcms, str2) != EOF) {
			    i = strlen (arc)
			    if (i > n && substr (arc, i-n+1, i) == imtype)
				arc = substr (arc, 1, i-n)
			    if (arc != arc1)
				    next
			    arc = arcms
			    if (i > n && substr (arc, i-n+1, i) == imtype)
				arc = substr (arc, 1, i-n)
			    arcms = arc // extn // imtype
	
			    if (access (arcms))
				imdelete (arcms, verify=no)
	
			    if (!batch)
			        print ("Extract arc spectrum ", arc)
			    print ("Extract arc spectrum ", arc, >> logfile)
			    apscript (arc, references=apref,
				ansrecenter="NO", ansresize="NO", ansedit="NO",
				anstrace="NO", nsubaps=params.nsubaps,
				background="none", clean=no,
				weights="none", verbose=verbose1)
			    sapertures (arcms, apertures="",
				apidtable=apidtable, wcsreset=no, verbose=no,
				beam=INDEF, dtype=INDEF, w1=INDEF, dw=INDEF,
				z=INDEF, aplow=INDEF, aphigh=INDEF, title=INDEF)
			    if (response != "") {
			        if (params.nsubaps == 1)
				    sarith (arcms, "/", response, arcfms,
					w1=INDEF, w2=INDEF,
					apertures="", bands="", beams="",
					apmodulus=0, reverse=no,
					ignoreaps=no, format="multispec",
					renumber=no, offset=0, clobber=yes,
					merge=no, errval=0, verbose=no)
			        else {
				    blkrep (response, temp, 1, params.nsubaps)
				    sarith (arcms, "/", temp, arcfms,
					w1=INDEF, w2=INDEF,
					apertures="", bands="", beams="",
					apmodulus=0, reverse=no,
					ignoreaps=yes, format="multispec",
					renumber=no, offset=0, clobber=yes,
					merge=no, errval=0, verbose=no)
				    imdelete (temp, verify=no)
			        }
			    }
			    scopy (arcms, arc2, w1=INDEF, w2=INDEF,
				apertures=str2, bands="", beams="",
				apmodulus=1000, offset=0, format="multispec",
				clobber=yes, merge=yes, renumber=no,
				verbose=yes, >> logfile)
			    imdelete (arcms, verify=no)
			}
			fd = ""
		    }
		    reidentify (arcref1//extn, arc2,
			interactive=!batch, section="middle line",
			shift=0., step=1, nsum=1, cradius=params.cradius,
			threshold=params.threshold, nlost=100,
			refit=params.refit, trace=no, override=no,
			addfeatures=params.addfeatures,
			coordlist=params.coordlist, match=params.match,
			maxfeatures=50, minsep=2., database=database,
			plotfile=plotfile, logfiles=logfile,
			verbose=verbose1)
		}

		# If not reextracting arcs based on object apertures
		# then save the extracted arc to avoid doing it again.

		if (arc1//extn != arc2)
		    imdelete (arc2, verify=no)
	    }
    
	    # Set the REFSPEC parameters for multispec spectrum.
	    if (k == 1)
		hedit (spec//".ms", "refspec"//j, arc2, add=yes,
		    verify=no, show=no, update=yes)
	    else
		hedit (spec//".ms", "refspec"//j, arc2//" "//str1,
		    add=yes, verify=no, show=no, update=yes)

	    # Check for arc fibers in object spectra.
	    if (arctype != "shift" && (arcaps != "" || arcbeams != "")) {
	        scopy (spec//".ms", spec//"arc.ms", w1=INDEF, w2=INDEF,
		    apertures=arcaps, bands="", beams=arcbeams, apmodulus=1000,
		    offset=0, format="multispec", clobber=yes, merge=no,
		    renumber=no, verbose=no, >& "dev$null")
	        if (access (spec//"arc.ms"//imtype)) {
		    if (!batch)
		        print ("Reidentify arc fibers in ", spec,
			    " with respect to ", arc1)
		    print ("Reidentify arc fibers in ", spec,
		        " with respect to ", arc1, >> logfile)
		    delete (database//"/id"//spec//"arc.ms*", verify = no)
		    reidentify (arc2, spec//"arc.ms", interactive=no,
		        section="middle line", shift=0., step=1, nsum=1,
			cradius=params.cradius, threshold=params.threshold,
			nlost=100, refit=no, trace=no, override=no,
			addfeatures=no, database=database,
			plotfile=plotfile, logfiles=logfile,
			verbose=verbose1)
		    imdelete (spec//"arc.ms", verify=no)
		    hedit (spec//".ms", "refshft"//j, spec//"arc.ms interp",
		        add=yes, verify=no, show=no, update=yes)
		    if (!savearcs)
	                scopy (spec//".ms", "", w1=INDEF, w2=INDEF,
			    apertures="!"//arcaps, bands="", beams=arcbeams,
			    apmodulus=1000, offset=0, format="multispec",
			    clobber=yes, merge=no, renumber=no,
			    verbose=yes, >> logfile)
	        }
	    }
	}
end
