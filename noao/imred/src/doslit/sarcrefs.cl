# SARCREFS -- Determine dispersion relation for reference arcs.

procedure sarcrefs (arcref1, crval, cdelt, done, log1, log2)

file	arcref1
string	crval = "INDEF"
string	cdelt = "INDEF"
file	done
file	log1
file	log2
bool	newdisp = no

struct	*fd

begin
	string	arcref, arcrefms, arc, arcms, temp, str1, str2
	int	i, dc, nspec
	bool	log
	struct	str3

	temp = mktemp ("tmp$iraf")

	# Extract the primary arc reference spectrum.  Determine the
	# dispersion function with IDENTIFY/REIDENTIFY.  Set the wavelength
	# parameters with MSDISPCOR.

	newdisp = no
	arcref = arcref1
	arcrefms = arcref1 // ".ms." // envget ("imtype")
	i = stridx (",", arcrefms)
	if (i > 0)
	    arcrefms = substr (arcrefms, 1, i-1)
	if (!access (arcrefms)) {
	    print ("Extract arc reference image ", arcref) | tee (log1)
	    if (apslitproc.reference == "") {
		delete (database//"/ap"//arcref, verify=no, >& "dev$null")
		apslitproc (arcref, nfind=-1, ansfind="YES",
		    background="none", clean=no, weights="none")
	    } else
		apslitproc (arcref, background="none", clean=no, weights="none")

	    nspec = 1
	    hselect (arcrefms, "naxis2", yes) | scan (nspec)
	    if (nspec > 1)
		scopy (arcrefms//"[*,1]", arcrefms, w1=INDEF, w2=INDEF,
		    apertures="", bands="", beams="", apmodulus=0,
		    format="multispec", renumber=no, offset=0, clobber=yes,
		    merge=no, rebin=yes, verbose=no)
	}
		    
	# Check for dispersion correction.  If missing determine the
	# dispersion function and dispersion correct.  Dispersion
	# correction is required to define the dispersion parameters
	# for the objects.

	hselect (arcrefms, "dispcor", yes, > temp)
	fd = temp
	dc = -1
	i = fscan (fd, dc)
	fd = ""; delete (temp, verify=no)
	if (i < 1 || dc == -1) {
	    print ("Determine dispersion solution for ", arcref) | tee (log1)
	    #delete (database//"/id"//arcref//".ms*", verify=no)
	    printf ("%s %s\n", crval, cdelt) | scan (str3)
	    if (str3 == "INDEF INDEF")
		identify (arcrefms, section="middle line", database=database,
		    coordlist=sparams.coordlist, nsum=1, match=sparams.match,
		    maxfeatures=50, zwidth=100., ftype="emission",
		    fwidth=sparams.fwidth, cradius=sparams.cradius,
		    threshold=sparams.threshold, minsep=2.,
		    function=sparams.i_function, order=sparams.i_order,
		    sample="*", niterate=sparams.i_niterate,
		    low_reject=sparams.i_low, high_reject=sparams.i_high,
		    grow=0., autowrite=yes)
	    else
		autoidentify (arcrefms, crval, cdelt,
		    coordlist=sparams.coordlist,
		    interactive="YES", section="middle line", nsum="1",
		    ftype="emission", fwidth=sparams.fwidth,
		    cradius=sparams.cradius, threshold=sparams.threshold,
		    minsep=2., match=sparams.match, function=sparams.i_function,
		    order=sparams.i_order, sample="*",
		    niterate=sparams.i_niterate, low_reject=sparams.i_low,
		    high_reject=sparams.i_high, grow=0., dbwrite="YES",
		    overwrite=yes, database="database", verbose=yes,
		    logfile=logfile, plotfile=plotfile,
		    reflist="", refspec="", crpix="INDEF", cddir="unknown",
		    crsearch="-0.5", cdsearch="INDEF", aidpars="")

	    hedit (arcrefms, "refspec1", arcref // ".ms", add=yes,
		show=no, verify=no)

	    dispcor (arcrefms, "", linearize=sparams.linearize,
		database=database, table="", w1=INDEF, w2=INDEF, dw=INDEF,
		nw=INDEF, log=sparams.log, flux=sparams.flux, samedisp=yes,
		global=no, ignoreaps=yes, confirm=yes, verbose=no, listonly=no,
		logfile=logfile)
	    flpr

	    hedit (arcrefms, "dispcor", 0, add=yes, verify=no,
		show=no, update=yes)
	    newdisp = yes

#	    if (sproc.splot1) {
#	        print (arcrefms, ":")
#	        str1 = sproc.anssplot
#	        if (str1 == "NO" || str1 == "YES")
#		    sproc.splot1 = no
#	        if (str1 == "no" || str1 == "NO")
#		    sproc.splot2 = no
#	        else
#		    sproc.splot2 = yes
#	    }
#	    if (sproc.splot2)
#		splot (arcrefms)
	}
	print (arcref, >> done)
end
