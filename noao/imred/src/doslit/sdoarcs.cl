# SDOARCS -- Determine dispersion relation for spectrum based on refernece arcs.

procedure sdoarcs (spec, arcref1, reextract, logfile, batch)

file	spec
file	arcref1
bool	reextract
file	logfile
bool	batch

struct	*fd

begin
	file	temp, arc1, arc2, str1
	string	imtype, mstype, reid
	bool	verbose1
	int	i, n

	imtype = "." // envget ("imtype")
	i = stridx (",", imtype)
	if (i > 0)
	    imtype = substr (imtype, 1, i-1)
	mstype = ".ms" // imtype
	n = strlen (imtype)

	temp = mktemp ("tmp$iraf")

	for (j=1; j<=2; j+=1) {

	    # Setup interactive/batch parameters
	    if (batch) {
		verbose1 = no
		reid = "no"
	    } else {
		verbose1 = verbose
		reidentify.answer.p_mode = "h"
		reid = reidentify.answer
		reidentify.answer.p_mode = "q"
		if (reid == "no")
		    reid = "yes"
	    }

	    # The reference spectra refer initially to the 2D image.  At the
	    # end we will reset them to refer to the 1D spectra.

	    hselect (spec, "refspec"//j, yes, > temp)
	    fd = temp
	    i = fscan (fd, arc1, str1)
	    fd = ""; delete (temp, verify=no)
	    if (nscan() < 1)
		break

	    # Strip possible image extension.
	    i = strlen (arc1)
	    if (i > n && substr (arc1, i-n+1, i) == imtype)
		arc1 = substr (arc1, 1, i-n)
    
	    # Set extraction output and aperture reference depending on whether
	    # the arcs are to be rextracted using recentered or retraced object
	    # apertures.

	    arc2 = spec // arc1
	    if (access (arc2//mstype))
		imdelete (arc2//mstype, verify=no)
	    delete (database//"/id"//arc2//".ms*", verify = no, >& "dev$null")
    
	    # Extract and determine dispersion function if necessary.
	    if (!access (arc2//mstype)) {
		if (!batch)
		    print ("Extract and reidentify arc spectrum ", arc1)
		print ("Extract and reidentify arc spectrum ", arc1, >> logfile)
		apslitproc (arc1, output=arc2//".ms", references=spec,
		    background="none", clean=no, weights="none",
		    verbose=verbose1)
		delete (database//"/id"//arc2//".ms*", verify = no,
		    >& "dev$null")
		reidentify (arcref1//".ms", arc2//".ms", interactive=reid,
		    section="middle line", shift=0., step=1, nsum=1,
		    cradius=sparams.cradius, threshold=sparams.threshold,
		    nlost=100, refit=sparams.refit, trace=no, override=no,
		    newaps=yes, addfeatures=sparams.addfeatures,
		    coordlist=sparams.coordlist, match=sparams.match,
		    maxfeatures=50, minsep=2., database=database,
		    plotfile=plotfile, logfiles=logfile, verbose=verbose1)

		# If not reextracting arcs based on object apertures
		# then save the extracted arc to avoid doing it again.

		if (arc1 != arc2)
		    imdelete (arc2//".ms", verify=no)
	    }
    
	    # Set the REFSPEC parameters for multispec spectrum.
	    if (nscan() == 1)
		hedit (spec//".ms", "refspec"//j, arc2//".ms", add=yes,
		    verify=no, show=no, update=yes)
	    else
		hedit (spec//".ms", "refspec"//j, arc2//".ms "//str1,
		    add=yes, verify=no, show=no, update=yes)
	}
end
