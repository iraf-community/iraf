# ARCREFS -- Determine dispersion relation for reference arc.

procedure arcrefs (arcref, arcaps, arcbeams, response, done, log1, log2)

file	arcref
string	arcaps
string	arcbeams
file	response
file	done
file	log1
file	log2

struct	*fd

begin
	string	arcrefec, arcec, temp, str, imtype
	int	i, dc
	bool	log

	temp = mktemp ("tmp$iraf")

	# Extract the primary arc reference spectrum.  Determine the
	# dispersion function with ECIDENTIFY/ECREIDENTIFY.  Set the wavelength
	# parameters with ECDISPCOR.

	imtype = "." // envget ("imtype")
	i = stridx (",", imtype)
	if (i > 0)
	    imtype = substr (imtype, 1, i-1)
	arcrefec = arcref // ".ec"
	if (arcaps != "" || arcbeams  != "")
	    arcec = arcref // "arc.ec"
	else
	    arcec = ""
	if (!access (arcrefec//imtype)) {
	    print ("Extract arc reference image ", arcref) | tee (log1)
	    apscript (arcref, ansrecenter="NO", ansresize="NO", ansedit="NO",
		anstrace="NO", background="none", clean=no, weights="none")
	    if (response != "")
		sarith (arcrefec, "/", response, arcrefec, w1=INDEF, w2=INDEF,
		    apertures="", bands="", beams="", apmodulus=0, reverse=no,
		    ignoreaps=no, format="multispec", renumber=no, offset=0,
		    clobber=yes, merge=no, errval=0, verbose=no)
	    if (arcec != "") {
		scopy (arcrefec, arcec, w1=INDEF, w2=INDEF, apertures=arcaps,
		    bands="", beams=arcbeams, apmodulus=0, offset=0,
		    format="multispec", clobber=yes, merge=no, renumber=yes,
		    verbose=no)
		scopy (arcrefec, "", w1=INDEF, w2=INDEF, apertures="!"//arcaps,
		    bands="", beams=arcbeams, apmodulus=0, offset=0,
		    format="multispec", clobber=yes, merge=no, renumber=yes,
		    verbose=no)
	    }
	}
		    
	# Get the dispersion parameters from the header.  These are
	# used for all further spectra and also flag whether this
	# spectrum has been processed.  If the parameters are missing
	# the spectrum needs to have the dispersion function and
	# wavelength scale determined.  The HEDIT is needed because
	# in some cases the user may exit IDENTIFY without updating
	# the database (if the image was deleted but the database
	# entry was not).

	hselect (arcrefec, "dc-flag", yes, > temp)
	fd = temp
	dc = -1
	i = fscan (fd, dc)
	fd = ""; delete (temp, verify=no)
	if (dc == -1) {
	    print ("Determine dispersion solution for ", arcref) | tee (log1)
	    delete (database//"/ec"//arcref//".ec*", verify=no)
	    ecidentify (arcrefec, database=database,
		coordlist=params.coordlist, match=params.match,
		maxfeatures=100, zwidth=10., ftype="emission",
		fwidth=params.fwidth, cradius=params.cradius,
		threshold=params.threshold, minsep=2.,
		function=params.i_function, xorder=params.i_xorder,
		yorder=params.i_yorder, niterate=params.i_niterate,
		lowreject=params.i_low, highreject=params.i_high,
		autowrite=yes)
	    if (arcec != "") {
		ecreidentify (arcec, arcrefec, shift=0., cradius=params.cradius,
		    threshold=params.threshold, refit=yes, database=database,
		    logfiles=log1//","//log2)
		imdelete (arcec, verify=no)
	    }
	    hedit (arcrefec, "refspec1", arcref // ".ec", add=yes,
		show=no, verify=no, update=yes)
	}

	# Dispersion correct the reference arc.  Set the newdisp flag.

	if (dc == -1) {
	    dispcor (arcrefec, "", linearize=params.linearize,
		database=database, table="", w1=INDEF, w2=INDEF, dw=INDEF,
		nw=INDEF, log=params.log, flux=params.flux, samedisp=no,
		global=no, ignoreaps=no, confirm=no, listonly=no, verbose=yes,
		logfile=log1, > log2)
	    hedit (arcrefec, "dc-flag", 0, add=yes, verify=no,
		show=no, update=yes)
	    proc.newdisp = yes
	}

	print (arcref, >> done)
end
