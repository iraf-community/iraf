# DOARCS -- Determine dispersion relation for spectrum based on reference arcs.

procedure doarcs (spec, response, arcref, arcaps, arcbeams, reextract,
	arcap, logfile, batch)

file	spec
file	response
file	arcref
string	arcaps
string	arcbeams
bool	reextract
bool	arcap
file	logfile
bool	batch

struct	*fd

begin
	string	imtype, ectype
	int	i, j, k, n
	file	temp, arc1, arc2, str1, str2, arctype, apref, arc, arcec, logs
	file	specec, specarc
	bool	verbose1

	imtype = "." // envget ("imtype")
	i = stridx (",", imtype)
	if (i > 0)
	    imtype = substr (imtype, 1, i-1)
	ectype = ".ec" // imtype
	n = strlen (imtype)

	temp = mktemp ("tmp$iraf")

	if (batch)
	    verbose1 = no
	else
	    verbose1 = verbose
	if (verbose1)
	    logs = logfile//",STDOUT"
	else
	    logs = logfile

	# Separate simultaneous arc from object.
	specec = spec // ".ec"
	if (arcaps != "" || arcbeams != "")
	    specarc = spec // "arc1.ec"
	else
	     specarc = ""
	if (specarc != "") {
	    scopy (specec, specarc, w1=INDEF, w2=INDEF, apertures=arcaps,
		bands="", beams="", apmodulus=0, format="multispec",
		renumber=yes, offset=0, clobber=yes, merge=no, verbose=no)
	    scopy (specec, "", w1=INDEF, w2=INDEF, apertures="!"//arcaps,
		bands="", beams="", apmodulus=0, format="multispec",
		renumber=yes, offset=0, clobber=yes, merge=no, verbose=no)
	}

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
		arc2 = spec // arc1
		apref = spec
		if (access (arc2//ectype))
		    imdelete (arc2//ectype, verify=no)
		delete (database//"/ec"//arc2//".ec*", verify = no)
	    } else {
		arc2 = arc1
		apref = apscript.references
	    }

	    # Arcs are reidentified using the user refit option.
	    #	Also internal arcs are checked if HENEAR.

	    hselect (arc1, "arctype", yes, > temp)
	    fd = temp
	    i = fscan (fd, arctype)
	    fd = ""; delete (temp, verify=no)
    
	    # Extract and determine dispersion function if necessary.
	    if (!access (arc2//ectype)) {
		if (!batch)
		    print ("Extract and reidentify arc spectrum ", arc1)
		print ("Extract and reidentify arc spectrum ", arc1, >> logfile)
		apscript (arc1, output=arc2//".ec", references=apref,
		    ansrecenter="NO", ansresize="NO", ansedit="NO",
		    anstrace="NO", background="none",
		    clean=no, weights="none", verbose=verbose1)
		if (response != "")
		    sarith (arc2//".ec", "/", response, arc2//".ec", w1=INDEF,
			w2=INDEF, apertures="", bands="", beams="", apmodulus=0,
			reverse=no, ignoreaps=no, format="multispec",
			renumber=no, offset=0, clobber=yes, merge=no, errval=0,
			verbose=no)

		if (arcaps != "") {
		    scopy (arc2//".ec", arc2//"arc.ec", w1=INDEF, w2=INDEF,
			apertures=arcaps, bands="",  beams="", apmodulus=0,
			format="multispec", renumber=yes, offset=0,
			clobber=yes, merge=no, verbose=no)
		    scopy (arc2//".ec", "", w1=INDEF, w2=INDEF,
			apertures="!"//arcaps, bands="",  beams="",
			apmodulus=0, format="multispec", renumber=yes, offset=0,
			clobber=yes, merge=no, verbose=no)
		    ecreidentify (arc2//"arc.ec", arcref//"arc.ec", shift=0.,
			cradius=params.cradius, threshold=params.threshold,
			refit=yes, database=database, logfiles=logs)
		    imdelete (arc2//"arc.ec", verify=no)
		}
		ecreidentify (arc2//".ec", arcref//".ec", shift=0.,
		    cradius=params.cradius, threshold=params.threshold,
		    refit=yes, database=database, logfiles=logs)

		# If not reextracting arcs based on object apertures
		# then save the extracted arc to avoid doing it again.

		if (arc1 != arc2)
		    imdelete (arc2//".ec", verify=no)
	    }
    
	    # Set the REFSPEC parameters for echelle spectrum.
	    if (k == 1)
		hedit (specec, "refspec"//j, arc2//".ec", add=yes,
		    verify=no, show=no, update=yes)
	    else
		hedit (specec, "refspec"//j, arc2//".ec "//str1,
		    add=yes, verify=no, show=no, update=yes)

	    # Check for arc fibers in object spectra.
	    if (specarc != "") {
		if (!batch)
		    print ("Reidentify arc fibers in ", spec,
			" with respect to ", arc1)
		print ("Reidentify arc fibers in ", spec,
		    " with respect to ", arc1, >> logfile)
		delete (database//"/ec"//specarc, verify = no, >& "dev$null")
		ecreidentify (specarc, arc2//"arc.ec", shift=0.,
		    cradius=params.cradius, threshold=params.threshold,
		    refit=no, database=database, logfiles=logs)
		hedit (specec, "refshft"//j, specarc,
		    add=yes, verify=no, show=no, update=yes)
		imrename (specarc, spec//"arc"//j+1//".ec", verbose=no)
		specarc = spec // "arc" // j+1 // ".ec"
	    }
	}
	if (specarc != "")
	    imdelete (specarc, verify=no)
end
