# ARCREFS -- Determine dispersion relation for reference arcs.

procedure arcrefs (arcref1, arcref2, extn, arcreplace, apidtable, response,
	crval, cdelt, done, log1, log2)

file	arcref1
file	arcref2
string	extn
file	arcreplace
file	apidtable
file	response
string	crval = "INDEF"
string	cdelt = "INDEF"
file	done
file	log1
file	log2

struct	*fd

begin
	string	imtype
	string	arcref, arcrefms, arc, arcms
	string	temp, temp1, temp2, str1, str2
	int	i, n, nspec, dc
	real	w
	bool	log
	struct	str3

	imtype = "." // envget ("imtype")
	i = stridx (",", imtype)
	if (i > 0)
	    imtype = substr (imtype, 1, i-1)
	n = strlen (imtype)

	temp = mktemp ("tmp$iraf")
	temp1 = mktemp ("tmp$iraf")
	temp2 = mktemp ("tmp$iraf")

	# Extract the primary arc reference spectrum.  Extract and replace
	# any replacement arcs defined in the arcreplace file.  Determine the
	# dispersion function with IDENTIFY/REIDENTIFY.  Set the wavelength
	# parameters with MSDISPCOR.

	arcref = arcref1
	arcrefms = arcref1 // extn
	if (!access (arcrefms//imtype)) {
	    print ("Extract arc reference image ", arcref) | tee (log1)
	    apscript (arcref, output=arcrefms, ansrecenter="NO",
		ansresize="NO", ansedit="NO", anstrace="NO",
		nsubaps=params.nsubaps, background="none", clean=no,
		weights="none")
	    sapertures (arcrefms, apertures="", apidtable=apidtable,
		wcsreset=no, verbose=no, beam=INDEF, dtype=INDEF, w1=INDEF,
		dw=INDEF, z=INDEF, aplow=INDEF, aphigh=INDEF, title=INDEF)
	    if (response != "") {
		if (params.nsubaps == 1)
	            sarith (arcrefms, "/", response, arcrefms, w1=INDEF,
			w2=INDEF, apertures="", bands="", beams="", apmodulus=0,
			reverse=no, ignoreaps=no, format="multispec",
			renumber=no, offset=0, clobber=yes, merge=no,
			errval=0, verbose=no)
		else {
		    blkrep (response, temp, 1, params.nsubaps)
	            sarith (arcrefms, "/", temp, arcrefms, w1=INDEF, w2=INDEF,
			apertures="", bands="", beams="", apmodulus=0,
			reverse=no, ignoreaps=yes, format="multispec",
			renumber=no, offset=0, clobber=yes, merge=no,
			errval=0, verbose=no)
		    imdelete (temp, verify=no)
		}
	    }

	    if (arcreplace != "") {
		if (!access (arcreplace))
		    error (1, "Can't access file "//arcreplace)
		fd = arcreplace
		while (fscan (fd, arc, str1, str2) != EOF) {
		    i = strlen (arc)
		    if (i > n && substr (arc, i-n+1, i) == imtype)
			arc = substr (arc, 1, i-n)
		    if (arc != arcref)
			    next
		    arc = str1
		    if (i > n && substr (arc, i-n+1, i) == imtype)
			arc = substr (arc, 1, i-n)
		    arcms = arc // extn

		    if (access (arcms//imtype))
			imdelete (arcms, verify=no)

		    print ("Extract arc reference image ", arc) | tee (log1)
		    apscript (arc, output=arcms, ansrecenter="NO",
			ansresize="NO", ansedit="NO", anstrace="NO",
			nsubaps=params.nsubaps, background="none", clean=no,
			weights="none")
		    sapertures (arcms, apertures="", apidtable=apidtable,
			wcsreset=no, verbose=no, beam=INDEF, dtype=INDEF,
			w1=INDEF, dw=INDEF, z=INDEF, aplow=INDEF, aphigh=INDEF,
			title=INDEF)
		    if (response != "") {
			if (params.nsubaps == 1)
			    sarith (arcms, "/", response, arcms, w1=INDEF,
				w2=INDEF, apertures="", bands="", beams="",
				apmodulus=0, reverse=no, ignoreaps=no,
				format="multispec", renumber=no, offset=0,
				clobber=yes, merge=no, errval=0, verbose=no)
			else {
			    blkrep (response, temp, 1, params.nsubaps)
			    sarith (arcms, "/", temp, arcms, w1=INDEF,
				w2=INDEF, apertures="", bands="", beams="",
				apmodulus=0, reverse=no, ignoreaps=yes,
				format="multispec", renumber=no, offset=0,
				clobber=yes, merge=no, errval=0, verbose=no)
			    imdelete (temp, verify=no)
			}
		    }
		    scopy (arcms, arcrefms, w1=INDEF, w2=INDEF, apertures=str2,
			bands="", beams="", apmodulus=1000, offset=0,
			format="multispec", clobber=yes, merge=yes, renumber=no,
			verbose=yes) | tee (log1, > log2)
		    imdelete (arcms, verify=no)
		}
		fd = ""
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

	hselect (arcrefms, "dclog1", yes) | scan (str1)
	if (nscan () != 1) {
	    print ("Determine dispersion solution for ", arcref) | tee (log1)
	    #delete (database//"/id"//arcrefms//"*", verify=no)
	    printf ("%s %s\n", crval, cdelt) | scan (str3)
	    if (str3 == "INDEF INDEF")
		identify (arcrefms, section="middle line", database=database,
		    coordlist=params.coordlist, nsum=1, match=params.match,
		    maxfeatures=50, zwidth=100., ftype="emission",
		    fwidth=params.fwidth, cradius=params.cradius,
		    threshold=params.threshold, minsep=2.,
		    function=params.i_function, order=params.i_order,
		    sample="*", niterate=params.i_niterate,
		    low_reject=params.i_low, high_reject=params.i_high,
		    grow=0., autowrite=yes)
	    else
		autoidentify (arcrefms, crval, cdelt,
		    coordlist=params.coordlist,
		    interactive="YES", section="middle line", nsum="1",
		    ftype="emission", fwidth=params.fwidth,
		    cradius=params.cradius, threshold=params.threshold,
		    minsep=2., match=params.match, function=params.i_function,
		    order=params.i_order, sample="*",
		    niterate=params.i_niterate, low_reject=params.i_low,
		    high_reject=params.i_high, grow=0., dbwrite="YES",
		    overwrite=yes, database="database", verbose=yes,
		    logfile=logfile, plotfile=plotfile,
		    reflist="", refspec="", crpix="INDEF", cddir="unknown",
		    crsearch="-0.5", cdsearch="INDEF", aidpars="")

	    hedit (arcrefms, "refspec1", arcrefms, add=yes,
		show=no, verify=no)

	    nspec = 1
	    hselect (arcrefms, "naxis2", yes) | scan (nspec)
	    if (nspec > 1)
		reidentify (arcrefms, "", interactive=yes,
		    section="middle line", shift=0., step=1, nsum=1,
		    cradius=params.cradius, threshold=params.threshold,
		    nlost=100, refit=params.refit, trace=no, override=yes,
		    addfeatures=params.addfeatures, newaps=no,
		    coordlist=params.coordlist, match=params.match,
		    maxfeatures=50, minsep=2., database=database,
		    plotfile=plotfile, logfiles=logfile, verbose=yes)

	    # Dispersion correct the reference arc.  This step is required to
	    # use the confirm option of MSDISPCOR to set the wavelength scale
	    # for all further spectra.  Set the newdisp flag.

	    print ("Dispersion correct ", arcref) | tee (log1)
	    dispcor (arcrefms, "", linearize=params.linearize,
		database=database, table="", w1=INDEF, w2=INDEF, dw=INDEF,
		nw=INDEF, log=params.log, flux=params.flux, samedisp=yes,
		global=no, ignoreaps=no, confirm=yes, listonly=no, verbose=no,
		logfile=logfile)
	    if (params.nsubaps > 1) {
		imrename (arcrefms, temp, verbose=no)
		scopy (temp, arcrefms, w1=INDEF, w2=INDEF, apertures="1-999",
		    bands="", beams="", apmodulus=0, offset=0,
		    format="multispec", clobber=no, merge=no, renumber=no,
		    verbose=no)
		blkavg (temp, temp, 1, params.nsubaps, option="sum")
		imcopy (temp, arcrefms//"[*,*]", verbose=no)
		imdelete (temp, verify=no)
	    }
	    proc.newdisp = yes
	}
	if (extn == ".ms")
	    print (arcref, >> done)

	# Extract the alternate shift arc reference.  Transfer the dispersion
	# function from the primary arc reference and then identify shift
	# lines.

	if (arcref2 != "") {
	    arcref = arcref2
	    arcrefms = arcref2 // extn
	    if (proc.newdisp && access (arcrefms//imtype))
		imdelete (arcrefms, verify=no)
	    if (!access (arcrefms)) {
		print ("Extract arc reference image ", arcref) | tee (log1)
		apscript (arcref, output=arcrefms, ansrecenter="NO",
		    ansresize="NO", ansedit="NO", anstrace="NO",
		    nsubaps=params.nsubaps, background="none", clean=no,
		    weights="none")
		sapertures (arcrefms, apertures="", apidtable=apidtable,
		    wcsreset=no, verbose=no, beam=INDEF, dtype=INDEF, w1=INDEF,
		    dw=INDEF, z=INDEF, aplow=INDEF, aphigh=INDEF, title=INDEF)
		if (response != "") {
		    if (params.nsubaps == 1)
			sarith (arcrefms, "/", response, arcrefms, w1=INDEF,
			    w2=INDEF, apertures="", bands="", beams="",
			    apmodulus=0, reverse=no, ignoreaps=no,
			    format="multispec", renumber=no, offset=0,
			    clobber=yes, merge=no, errval=0, verbose=no)
		    else {
			blkrep (response, temp, 1, params.nsubaps)
			sarith (arcrefms, "/", temp, arcrefms, w1=INDEF,
			    w2=INDEF, apertures="", bands="", beams="",
			    apmodulus=0, reverse=no, ignoreaps=yes,
			    format="multispec", renumber=no, offset=0,
			    clobber=yes, merge=no, errval=0, verbose=no)
			imdelete (temp, verify=no)
		    }
		}
	    }

	    hselect (arcrefms, "dclog1", yes) | scan (str1)
	    if (nscan () != 1) {
		print ("Determine dispersion solution for ", arcref) |
		    tee (log1)
		#delete (database//"/id"//arcrefms//"*", verify=no)

		print (":r ", arcref1//extn, "\na\nd") |
		identify (arcrefms, section="middle line", database=database,
		    coordlist="", nsum=1, match=params.match, maxfeatures=50,
		    zwidth=100., ftype="emission", fwidth=params.fwidth,
		    cradius=params.cradius, threshold=params.threshold,
		    minsep=2., function=params.i_function,
		    order=params.i_order, sample="*",
		    niterate=params.i_niterate, low_reject=params.i_low,
		    high_reject=params.i_high, grow=0., autowrite=yes,
		    cursor="STDIN", >G "dev$null", >& "dev$null")
		identify (arcrefms, section="middle line", database=database,
		    coordlist="", nsum=1, match=params.match, maxfeatures=50,
		    zwidth=100., ftype="emission", fwidth=params.fwidth,
		    cradius=params.cradius, threshold=params.threshold,
		    minsep=2., function=params.i_function,
		    order=params.i_order, sample="*",
		    niterate=params.i_niterate, low_reject=params.i_low,
		    high_reject=params.i_high, grow=0., autowrite=yes)
		print (":feat ", temp) |
		identify (arcrefms, section="middle line", database=database,
		    coordlist="", nsum=1, match=params.match, maxfeatures=50,
		    zwidth=100., ftype="emission", fwidth=params.fwidth,
		    cradius=params.cradius, threshold=params.threshold,
		    minsep=2., function=params.i_function,
		    order=params.i_order, sample="*",
		    niterate=params.i_niterate, low_reject=params.i_low,
		    high_reject=params.i_high, grow=0., autowrite=yes,
		    cursor="STDIN", >G "dev$null", >& "dev$null")
		print (":r ", arcref1//extn, "\na\nd", > temp1)
		fd = temp
		while (fscan (fd, i, w, w, w) != EOF) {
		    if (nscan() == 4) {
			print (w, 1, 1, "m", >> temp1)
			print (w, >> temp2)
		    }
		}
		print ("g", >> temp1)
		fd = ""; delete (temp, verify=no)

		nspec = 1
		hselect (arcrefms, "naxis2", yes) | scan (nspec)
		for (i = 1; i <= nspec; i+=1)
		    identify (arcrefms, section="line "//i,
			database=database, coordlist="", nsum=1,
			match=params.match, maxfeatures=50, zwidth=100.,
			ftype="emission", fwidth=params.fwidth,
			cradius=params.cradius, threshold=params.threshold,
			minsep=2., function=params.i_function,
			order=params.i_order, sample="*",
			niterate=params.i_niterate,
			low_reject=params.i_low, high_reject=params.i_high,
			grow=0., autowrite=yes, cursor=temp1, < temp2,
			>G "dev$null", >>& temp)
		delete (temp1, verify=no); delete (temp2, verify=no)
		system.match ("Coordinate shift", temp, stop=no, print_file_n=yes,
		    metacharacte=yes) | tee (log1, > log2)
		delete (temp, verify=no)

		dispcor (arcrefms, "", linearize=params.linearize,
		    database=database, table="", w1=INDEF, w2=INDEF,
		    dw=INDEF, nw=INDEF, log=params.log, flux=params.flux,
		    samedisp=yes, global=no, ignoreaps=no, confirm=no,
		    listonly=no, verbose=yes, logfile=logfile, > log2)
		if (params.nsubaps > 1) {
		    imrename (arcrefms, temp, verbose=no)
		    scopy (temp, arcrefms, w1=INDEF, w2=INDEF, apertures="1-999",
			bands="", beams="", apmodulus=0, offset=0,
			format="multispec", clobber=no, merge=no, renumber=no,
			verbose=no)
		    blkavg (temp, temp, 1, params.nsubaps, option="sum")
		    imcopy (temp, arcrefms//"[*,*]", verbose=no)
		    imdelete (temp, verify=no)
		}
	    }
	    if (extn == ".ms")
		print (arcref, >> done)
	}
end
