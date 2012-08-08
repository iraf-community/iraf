include	<imhdr.h>
include	<error.h>
include	<pkg/gtools.h>
include	"apertures.h"

define	APFIND		1
define	APRECENTER	2
define	APRESIZE	3
define	APEDIT		4
define	APTRACE		5
define	APSUM		6
define	APNORM		7
define	APSCAT		8
define	APALL		9
define	APFIT		10
define	APFLAT		11
define	APMASK		12
define	APSCRIPT	13
define	APSLITPROC	14
define	APNOISE		15


# APEXTRACT TASK ENTRY POINTS
#
# The entry point for each task selects the operations to be performed
# and initializes the pset to be used for the algorithm parameters.

procedure t_apfind ()
begin
	call apall (APFIND)
end

procedure t_aprecenter ()
begin
	call apall (APRECENTER)
end

procedure t_apresize ()
begin
	call apall (APRESIZE)
end

procedure t_apedit ()
begin
	call apall (APEDIT)
end

procedure t_aptrace ()
begin
	call apall (APTRACE)
end

procedure t_apsum ()
begin
	call apall (APSUM)
end

procedure t_apnorm ()
begin
	call apall (APNORM)
end

procedure t_apscatter ()
begin
	call apall (APSCAT)
end

procedure t_apall ()
begin
	call apall (APALL)
end

procedure t_apflat ()
begin
	call apall (APFLAT)
end

procedure t_apfit ()
begin
	call apall (APFIT)
end

procedure t_apmask ()
begin
	call apall (APMASK)
end

procedure t_apscript ()
begin
	call apall (APSCRIPT)
end

procedure t_apslitproc ()
begin
	call apall (APSLITPROC)
end

procedure t_apnoise ()
begin
	call apall (APNOISE)
end


# APALL -- Master aperture definition and extraction procedure.

procedure apall (ltask)

int	ltask			# Logical task

bool	find			# Find apertures?
bool	recenter		# Recenter apertures?
bool	resize			# Resize apertures?
bool	edit			# Edit apertures?
bool	trace			# Trace apertures?
bool	extract			# Extract apertures?
bool	fit			# Extract fit?
bool	norm			# Normalize spectra?
bool	flat			# Flatten spectra?
bool	scat			# Subtract scattered light?
bool	mask			# Aperture mask?
bool	noise			# Noise calculation?

int	input			# List of input spectra
int	refs			# List of reference spectra
int	out			# List of output spectra
pointer	format			# Output format or fit type
int	scatout			# List of scattered light images
int	profs			# List of profile spectra
int	line			# Dispersion line
int	nsum			# Lines to sum

pointer	aps			# Pointer to array of aperture pointers
int	naps			# Number of apertures

char	nullstr[1]
int	i
pointer	sp, image, output, reference, profiles, str, str1

bool	clgetb(), apgetb(), streq(), ap_answer(), apgans(), apgansb()
int	imtopenp(), clgeti(), ap_getim(), ap_dbaccess(), strncmp()

errchk	ap_dbacess, ap_dbread, ap_find, ap_recenter, ap_resize, ap_edit
errchk	ap_trace, ap_plot, ap_extract, ap_scatter, ap_mask, ap_dbwrite

data	nullstr /0,0/

begin
	# Allocate memory for the apertures, filenames, and strings.
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (reference, SZ_FNAME, TY_CHAR)
	call salloc (format, SZ_LINE, TY_CHAR)
	call salloc (profiles, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (str1, SZ_LINE, TY_CHAR)

	switch (ltask) {
	case APALL:
	    call apopset ("apall1")
	case APFIT:
	    call apopset ("apfit1")
	case APFLAT:
	    call apopset ("apflat1")
	case APNORM:
	    call apopset ("apnorm1")
	case APSCRIPT:
	    call apopset ("apscript")
	case APSLITPROC:
	    call apopset ("apslitproc")
	case APNOISE:
	    call apopset ("apnoise1")
	default:
	    call apopset ("apparams")
	}

	input = imtopenp ("input")
	refs = imtopenp ("references")
	line = clgeti ("line")
	nsum = clgeti ("nsum")
	out = NULL
	profs = NULL
	scatout = NULL

	switch (ltask) {
	case APSUM, APALL, APFIT, APNORM, APFLAT, APSCAT,
	    APMASK, APSCRIPT, APSLITPROC:
	    out = imtopenp ("output")
	}

	switch (ltask) {
	case APSUM, APALL:
	    profs = imtopenp ("profiles")
	    call apgstr ("format", Memc[format], SZ_LINE)
	case APFIT:
	    call clgstr ("fittype", Memc[format], SZ_LINE)
	case APNORM:
	    call strcpy ("normalize", Memc[format], SZ_LINE)
	case APFLAT:
	    call strcpy ("flatten", Memc[format], SZ_LINE)
	case APSCAT:
	    scatout = imtopenp ("scatter")
	case APSCRIPT, APSLITPROC:
	    scatout = imtopenp ("scatter")
	    profs = imtopenp ("profiles")
	    call apgstr ("format", Memc[format], SZ_LINE)
	case APNOISE:
	    call strcpy ("noise", Memc[format], SZ_LINE)
	}

	trace = false
	extract = false
	fit = false
	norm = false
	flat = false
	scat = false
	mask = false
	noise = false

	if (apgetb ("initialize")) {
	    find = clgetb ("find")
	    recenter = clgetb ("recenter")
	    resize = clgetb ("resize")
	    edit = clgetb ("edit")

	    switch (ltask) {
	    case APTRACE, APSUM, APALL, APFIT, APNORM,
		APFLAT, APSCAT, APMASK, APSCRIPT, APSLITPROC, APNOISE:
	        trace = clgetb ("trace")
	    }

	    switch (ltask) {
	    case APSUM, APALL:
	        extract = clgetb ("extract")
	    case APFIT:
	        fit = clgetb ("fit")
	    case APNORM:
	        norm = clgetb ("normalize")
	    case APFLAT:
	        flat = clgetb ("flatten")
	    case APSCAT:
	        scat = clgetb ("subtract")
	    case APMASK:
	        mask = clgetb ("mask")
	    case APSCRIPT, APSLITPROC:
	        extract = clgetb ("extract")
	        scat = clgetb ("subtract")
		if (extract && scat)
		    call error (1,
		    "APSCRIPT: Can't combine scattered light and extraction")
	    case APNOISE:
		noise = true
	    }

	    call ap_init (find, recenter, resize, edit, trace, extract, fit,
	        norm, flat, scat, mask, noise)
	} else {
	    find = apgans ("ansfind")
	    recenter = apgans ("ansrecenter")
	    resize = apgans ("ansresize")
	    edit = apgans ("ansedit")

	    switch (ltask) {
	    case APTRACE, APSUM, APALL, APFIT, APNORM,
		APFLAT, APSCAT, APMASK, APSCRIPT, APSLITPROC, APNOISE:
	        trace = apgans ("anstrace")
	    }

	    switch (ltask) {
	    case APSUM, APALL:
	        extract = apgans ("ansextract")
	    case APFIT:
	        fit = apgans ("ansfit")
	    case APNORM:
	        norm = apgans ("ansnorm")
	    case APFLAT:
	        flat = apgans ("ansflat")
	    case APSCAT:
	        scat = apgans ("ansscat")
	    case APMASK:
	        mask = apgans ("ansmask")
	    case APSCRIPT, APSLITPROC:
	        extract = apgans ("ansextract")
	        scat = apgans ("ansscat")
		if (extract && scat)
		    call error (1,
		    "APSCRIPT: Can't combine scattered light and extraction")
	    }
	}

	# Initialize the apertures.
	naps = 0
	Memc[reference] = EOS
	Memc[profiles] = EOS
	call malloc (aps, 100, TY_POINTER)

	# Process the apertures from each input image.
	while (ap_getim (input, Memc[image], SZ_FNAME) != EOF) {
	    if (ap_getim (refs, Memc[str], SZ_LINE) != EOF)
		call strcpy (Memc[str], Memc[reference], SZ_FNAME)
	    if (extract || fit || flat || norm || scat || mask)
		if (ap_getim (out, Memc[output], SZ_FNAME) == EOF)
		    Memc[output] = EOS

	    # Get apertures.
	    call appstr ("ansdbwrite1", "no")
	    if (streq (Memc[reference], nullstr) ||
		streq (Memc[reference], Memc[image])) {
		if (clgetb ("verbose"))
		    call printf ("Searching aperture database ...\n")
		iferr (call ap_dbread (Memc[image], aps, naps))
		    ;
	    } else if (streq (Memc[reference], "OLD")) {
		iferr (call ap_dbread (Memc[image], aps, naps))
		    next
	    } else {
		if (strncmp (Memc[reference], "NEW", 3) == 0) {
		    if (ap_dbaccess (Memc[image]) == YES)
			next
		    call strcpy (Memc[reference+3], Memc[reference], SZ_FNAME)
		}
		if (clgetb ("verbose"))
		    call printf ("Searching aperture database ...\n")
		iferr (call ap_dbread (Memc[reference], aps, naps)) {
		    call eprintf (
			"WARNING: Reference image (%s) apertures not found\n")
			call pargstr (Memc[reference])
		    next
		}
		if (naps > 0)
		    call appstr ("ansdbwrite1", "yes")
	    }
	    call clgstr ("apertures", Memc[str], SZ_LINE)
	    call ap_select (Memc[str], Memi[aps], naps)

	    iferr {
		# Find apertures.
		if (find && naps == 0)
		    call ap_find (Memc[image], line, nsum, aps, naps)

		# Recenter apertures.
		else if (recenter)
		    call ap_recenter (Memc[image], line, nsum, Memi[aps], naps,
			NO)

		# Resize apertures.
		if (resize)
		    call ap_resize (Memc[image], line, nsum, Memi[aps], naps,
			NO)

		# Edit apertures.
		if (edit)
		    call ap_edit (Memc[image], line, nsum, aps, naps)

		# Trace apertures.
		if (trace)
		    call ap_trace (Memc[image], line, Memi[aps], naps, NO)

		# Write database and make aperture plot.
		if (apgansb ("ansdbwrite1")) {
		    call clgstr ("database", Memc[str1], SZ_LINE)
	            call sprintf (Memc[str], SZ_LINE,
		        "Write apertures for %s to %s")
	                call pargstr (Memc[image])
	                call pargstr (Memc[str1])
	            if (ap_answer ("ansdbwrite", Memc[str]))
		        call ap_dbwrite (Memc[image], aps, naps)
		}
	        iferr (call ap_dbwrite ("last", aps, naps))
		    ;
		iferr (call ap_plot (Memc[image], line, nsum, Memi[aps], naps))
		    call erract (EA_WARN)

		# Extract 1D spectra but do not extract negative beams
		if (extract) {
		    do i = 1, naps {
			if (AP_BEAM(Memi[aps+i-1]) < 0)
			    AP_SELECT(Memi[aps+i-1]) = NO
		    }

	    	    if (ap_getim (profs, Memc[str1], SZ_LINE) != EOF)
			call strcpy (Memc[str1], Memc[profiles], SZ_FNAME)
		    call sprintf (Memc[str], SZ_LINE,
		        "Extract aperture spectra for %s?")
	                call pargstr (Memc[image])
		    if (ap_answer ("ansextract", Memc[str])) {
			call sprintf (Memc[str], SZ_LINE,
			    "Review extracted spectra from %s?")
			    call pargstr (Memc[image])
			if (ap_answer ("ansreview", Memc[str])) {
			    call apgstr ("ansreview", Memc[str], SZ_LINE)
			    call appstr ("ansreview1", Memc[str])
			} else
			    call appstr ("ansreview1", "NO")
	                call ap_extract (Memc[image], Memc[output],
			    Memc[format], Memc[profiles], Memi[aps], naps)
		    }
		}

		# Fit apertures.
		if (fit) {
		    call sprintf (Memc[str], SZ_LINE,
			"Fit apertures in %s?")
	                call pargstr (Memc[image])
		    if (ap_answer ("ansfit", Memc[str])) {
	                call ap_extract (Memc[image], Memc[output],
			    Memc[format], nullstr, Memi[aps], naps)
		    }
		}

		# Normalize apertures.
		if (norm) {
		    call sprintf (Memc[str], SZ_LINE,
			"Normalize apertures in %s?")
	                call pargstr (Memc[image])
		    if (ap_answer ("ansnorm", Memc[str])) {
			call sprintf (Memc[str], SZ_LINE,
			    "Fit spectra from %s interactively?")
			    call pargstr (Memc[image])
			if (ap_answer ("ansfitspec", Memc[str])) {
			    call apgstr ("ansfitspec", Memc[str], SZ_LINE)
			    call appstr ("ansfitspec1", Memc[str])
			} else
			    call appstr ("ansfitspec1", "NO")
	                call ap_extract (Memc[image], Memc[output],
			    Memc[format], nullstr, Memi[aps], naps)
		    }
		}

		# Flatten apertures.
		if (flat) {
		    call sprintf (Memc[str], SZ_LINE,
			"Flatten apertures in %s?")
	                call pargstr (Memc[image])
		    if (ap_answer ("ansflat", Memc[str])) {
			call sprintf (Memc[str], SZ_LINE,
			    "Fit spectra from %s interactively?")
			    call pargstr (Memc[image])
			if (ap_answer ("ansfitspec", Memc[str])) {
			    call apgstr ("ansfitspec", Memc[str], SZ_LINE)
			    call appstr ("ansfitspec1", Memc[str])
			} else
			    call appstr ("ansfitspec1", "NO")
	                call ap_extract (Memc[image], Memc[output],
			    Memc[format], nullstr, Memi[aps], naps)
		    }
		}
 
		# Substract scattered light.
		if (scat) {
		    if (ap_getim (scatout, Memc[str1], SZ_LINE) == EOF)
		        Memc[str1] = EOS
		    if (Memc[output] == EOS ||
			streq (Memc[image], Memc[output])) {
		        call mktemp ("tmp", Memc[str], SZ_LINE)
	                call ap_scatter (Memc[image], Memc[str],
			    Memc[str1], Memi[aps], naps, line)
		        call imdelete (Memc[image])
		        call imrename (Memc[str], Memc[image])
		    } else
	                call ap_scatter (Memc[image], Memc[output],
			    Memc[str1], Memi[aps], naps, line)
		}

		# Make a aperture mask.
		if (mask)
		    call ap_mask (Memc[image], Memc[output], Memi[aps], naps)

		# Fit noise.
		if (noise)
		    call ap_extract (Memc[image], nullstr,
			Memc[format], nullstr, Memi[aps], naps)

	    } then
		call erract (EA_WARN)

	    # Free memory.
	    for (i = 1; i <= naps; i = i + 1)
		call ap_free (Memi[aps+i-1])
	    naps = 0
	}

	# Free memory and finish up.
	call imtclose (input)
	call imtclose (refs)
	if (out != NULL)
	    call imtclose (out)
	if (profs != NULL)
	    call imtclose (profs)
	if (norm || flat)
	    call ap_fitfree ()
	if (scat) {
	    if (scatout != NULL)
	        call imtclose (scatout)
	    call scat_free ()
	}
	call ap_gclose ()
	call ap_trfree ()
	call apcpset ()
	call sfree (sp)
end


procedure ap_init (find, recenter, resize, edit, trace, extract, fit,
	norm, flat, scat, mask, noise)

bool	find, recenter, resize, edit, trace
bool	extract, fit, norm, flat, scat, mask, noise

pointer	sp, str
bool	clgetb()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	if (find)
	    call appans ("ansfind", find, find)
	if (recenter)
	    call appans ("ansrecenter", recenter, recenter)
	if (resize)
	    call appans ("ansresize", resize, resize)
	if (edit)
	    call appans ("ansedit", edit, false)
	if (trace) {
	    call appans ("anstrace", trace, trace)
	    call appans ("ansfittrace", clgetb ("fittrace"), false)
	}
	if (extract) {
	    call appans ("ansextract", extract, extract)
            call appans ("ansreview", clgetb ("review"), false)
	}
	if (fit) {
	    call appans ("ansfit", fit, fit)
            call appstr ("ansreview1", "NO")
	}
	if (norm) {
	    call appans ("ansnorm", norm, norm)
	    call appans ("ansfitspec", clgetb ("fitspec"), false)
            call appstr ("ansreview1", "NO")
	}
	if (flat) {
	    call appans ("ansflat", flat, flat)
	    call appans ("ansfitspec", clgetb ("fitspec"), false)
            call appstr ("ansreview1", "NO")
	}
	if (scat) {
	    call appans ("ansscat", scat, scat)
            call appans ("anssmooth", clgetb ("smooth"), clgetb ("smooth"))
            call appans ("ansfitscatter", clgetb ("fitscatter"), false)
            call appans ("ansfitsmooth", clgetb ("fitsmooth"), false)
	}
	if (mask)
	    call appans ("ansmask", mask, mask)
	if (noise)
            call appstr ("ansreview1", "NO")

	if (extract || fit || norm || flat) {
	    if (clgetb ("interactive"))
	        call appstr ("ansclobber", "no")
            else
	        call appstr ("ansclobber", "NO")
	}

	call apgstr ("dbwrite", Memc[str], SZ_LINE)
	if (clgetb ("interactive"))
	    call appstr ("ansdbwrite", Memc[str])
	else {
	    if (Memc[str] == 'y' || Memc[str] == 'Y')
	        call appstr ("ansdbwrite", "YES")
	    else
	        call appstr ("ansdbwrite", "NO")
	}

	call sfree (sp)
end
