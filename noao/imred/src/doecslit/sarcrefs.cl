# SARCREFS -- Determine dispersion relation for reference arc.

procedure sarcrefs (arcref, done, log1, log2)

file	arcref
file	done
file	log1
file	log2

struct	*fd

begin
	string	arcrefec, arcec, temp
	int	i, dc
	bool	log

	temp = mktemp ("tmp$iraf")

	# Extract the primary arc reference spectrum.  Determine the
	# dispersion function with IDENTIFY/REIDENTIFY.  Set the wavelength
	# parameters with ECDISPCOR.

	arcrefec = arcref // ".ec." // envget ("imtype")
	i = stridx (",", arcrefec)
	if (i > 0)
	    arcrefec = substr (arcrefec, 1, i-1)
	if (!access (arcrefec)) {
	    print ("Extract arc reference image ", arcref) | tee (log1)
	    apslitproc (arcref, background="none", clean=no, weights="none")
	}

	# Get the dispersion parameters from the header.  These are
	# used for all further spectra and also flag whether this
	# spectrum has been processed.  If the parameters are missing
	# the spectrum needs to have the dispersion function and
	# wavelength scale determined.  The HEDIT is needed because
	# in some cases the user may exit ECIDENTIFY without updating
	# the database (if the image was deleted but the database
	# entry was not).

	hselect (arcrefec, "dc-flag", yes, > temp)
	fd = temp
	dc = -1
	i = fscan (fd, dc)
	fd = ""; delete (temp, verify=no)
	if (i < 1) {
	    print ("Determine dispersion solution for ", arcref) | tee (log1)
	    #delete (database//"/ec"//arcref//".ec*", verify=no)
	    ecidentify (arcrefec, database=database,
		coordlist=sparams.coordlist, match=sparams.match,
		maxfeatures=100, zwidth=10., ftype="emission",
		fwidth=sparams.fwidth, cradius=sparams.cradius,
		threshold=sparams.threshold, minsep=2.,
		function=sparams.i_function, xorder=sparams.i_xorder,
		yorder=sparams.i_yorder, niterate=sparams.i_niterate,
		lowreject=sparams.i_low, highreject=sparams.i_high,
		autowrite=yes)
	    hedit (arcrefec, "refspec1", arcref // ".ec", add=yes,
		show=no, verify=no, update=yes)
	}

	# Dispersion correct the reference arc.  This step is required to
	# to set the wavelength scale for all further spectra.

	if (i < 1) {
	    dispcor (arcrefec, "", linearize=sparams.linearize,
		database=database, table="", w1=INDEF, w2=INDEF, dw=INDEF,
		nw=INDEF, log=sparams.log, flux=sparams.flux, samedisp=no,
		global=no, ignoreaps=no, confirm=no, listonly=no, verbose=yes,
		logfile=log1, > log2)
	    hedit (arcrefec, "dc-flag", 0, add=yes, show=no,
		verify=no, update=yes)
	    sproc.newdisp = yes
	}

	print (arcref, >> done)
end
