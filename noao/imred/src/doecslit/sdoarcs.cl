# SDOARCS -- Determine dispersion relation for spectrum based on reference arcs.

procedure sdoarcs (spec, arcref, reextract, arcap, logfile, batch)

file	spec
file	arcref
bool	reextract
bool	arcap
file	logfile
bool	batch

struct	*fd

begin
	int	i, j, k
	file	temp, arc1, arc2, str1, str2, arctype, apref, arc, arcec, logs
	file	specec, specarc
	bool	verbose1

	temp = mktemp ("tmp$iraf")

	if (batch)
	    verbose1 = no
	else
	    verbose1 = verbose
	if (verbose1)
	    logs = logfile//",STDOUT"
	else
	    logs = logfile

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
	    if (i > 4 && substr (arc1, i-3, i) == ".imh")
		arc1 = substr (arc1, 1, i-4)

	    # Set extraction output and aperture reference depending on whether
	    # the arcs are to be rextracted using recentered or retraced object
	    # apertures.

	    if (arcap) {
		arc2 = spec // arc1
		apref = spec
		if (access (arc2//".ec.imh"))
		    imdelete (arc2//".ec.imh", verify=no)
		delete (database//"/ec"//arc2//".ec*", verify = no)
	    } else {
		arc2 = arc1
		apref = apslitproc.references
		if (reextract && access (arc2//".ec.imh")) {
		    if (arc2 != arcref)
			imdelete (arc2//".ec.imh", verify=no)
		}
	    }

	    # Extract and determine dispersion function if necessary.
	    if (!access (arc2//".ec.imh")) {
		delete (database//"/ec"//arc2//".ec*", verify = no)
		if (!batch)
		    print ("Extract and reidentify arc spectrum ", arc1)
		print ("Extract and reidentify arc spectrum ", arc1, >> logfile)
		apslitproc (arc1, output=arc2//".ec", references=apref,
		    background="none", clean=no, weights="none",
		    verbose=verbose1)
		ecreidentify (arc2//".ec", arcref//".ec", shift=0.,
		    cradius=sparams.cradius, threshold=10., refit=yes,
		    database=database, logfiles=logs)

		# If not reextracting arcs based on object apertures
		# then save the extracted arc to avoid doing it again.

		if (arc1 != arc2)
		    imdelete (arc2//".ec", verify=no)
	    }
    
	    # Set the REFSPEC parameters for echelle spectrum.
	    if (k == 1)
		hedit (spec//".ec", "refspec"//j, arc2//".ec", add=yes,
		    verify=no, show=no, update=yes)
	    else
		hedit (spec//".ec", "refspec"//j, arc2//".ec "//str1, add=yes,
		    verify=no, show=no, update=yes)
	}
end
