# SARCREFS -- Determine dispersion relation for reference arcs.

procedure sarcrefs (arcref1, done, log1, log2)

file	arcref1
file	done
file	log1
file	log2
bool	newdisp = no

struct	*fd

begin
	string	arcref, arcrefms, arc, arcms, temp, str1, str2
	int	i, dc
	bool	log

	temp = mktemp ("tmp$iraf")

	# Extract the primary arc reference spectrum.  Determine the
	# dispersion function with IDENTIFY/REIDENTIFY.  Set the wavelength
	# parameters with MSDISPCOR.

	newdisp = no
	arcref = arcref1
	arcrefms = arcref1 // ".ms.imh"
	if (!access (arcrefms)) {
	    print ("Extract arc reference image ", arcref) | tee (log1)
	    if (apslitproc.reference == "") {
		delete (database//"/ap"//arcref, verify=no, >& "dev$null")
		apslitproc (arcref, nfind=-1, ansfind="YES",
		    background="none", clean=no, weights="none")
	    } else
		apslitproc (arcref, background="none", clean=no, weights="none")
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
	    identify (arcrefms, section="middle line", database=database,
		coordlist=sparams.coordlist, nsum=1, match=sparams.match,
		maxfeatures=50, zwidth=100., ftype="emission",
		fwidth=sparams.fwidth, cradius=sparams.cradius,
		threshold=10., minsep=2., function=sparams.i_function,
		order=sparams.i_order, sample="*",
		niterate=sparams.i_niterate, low_reject=sparams.i_low,
		high_reject=sparams.i_high, grow=0., autowrite=yes)
	    hedit (arcrefms, "refspec1", arcref // ".ms", add=yes,
		show=no, verify=no)

	    dispcor (arcrefms, "", linearize=sparams.linearize,
		database=database, table="", w1=INDEF, w2=INDEF, dw=INDEF,
		nw=INDEF, log=sparams.log, flux=sparams.flux, samedisp=yes,
		global=no, ignoreaps=no, confirm=yes, verbose=no, listonly=no,
		logfile=logfile)

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
