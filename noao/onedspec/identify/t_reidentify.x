include	<error.h>
include	<fset.h>
include	<gset.h>
include	<pkg/gtools.h>
include	<smw.h>
include	"identify.h"

define	ICFITHELP	"noao$lib/scr/idicgfit.key"

# T_REIDENTIFY -- Reidentify features starting from reference features.
# A reference spectrum is specified and the same features are identified
# in other images.  Some lines may be lost due to bad centering.  Additional
# lines may be excluded from a new fit to the dispersion function.  Instead
# of refitting the dispersion function the user may elect to determine only
# a shift in the reference dispersion function.  Additional features may
# be added given a coordinate list.
#
# In 2D images a starting line or column is selected.  A number of lines
# or columns may be averaged before identifying features.  If a positive step
# size is given then additional lines or columns may be reidentified in
# the reference image.  This may be done either by tracing or by reidentifying
# starting from the same reference features.  Reidentification between images
# is done by taking the same line or column from the reference image.
# The step and summing are ignored for multispec images.
# 
# Multispec format images are matched by aperture number and the spectra
# need not be in the same order in each image.

procedure t_reidentify ()

pointer	reference		# Reference image
int	list			# List of images
char	ans[3]			# Interactive?
double	crsearch		# Search radius

int	i, fd, nlogfd
pointer	sp, logfile, str, id, logfd, pd

int	clscan(), clgeti(), clpopnu(), clgfil(), clgwrd()
int	nscan(), open(), btoi(), nowhite(), imtopenp(), imtgetim()
bool	clgetb(), strne()
double	clgetd()
pointer	gopen(), gt_init()

begin
	call smark (sp)
	call salloc (reference, SZ_FNAME, TY_CHAR)
	call salloc (logfile, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Allocate the basic data structures.
	call id_init (id)
	call ic_open (ID_IC(id))

	# Get task parameters.
	call clgstr ("reference", Memc[reference], SZ_FNAME)
	list = imtopenp ("images")
	i = nowhite (Memc[reference], Memc[reference], SZ_FNAME)

	crsearch = clgetd ("search")
	ID_REFIT(id) = btoi (clgetb ("refit"))

	if (clscan ("nsum") != EOF) {
	    call gargi (ID_NSUM(id,1))
	    call gargi (ID_NSUM(id,2))
	    if (nscan() == 0)
		call error (1, "Error in 'nsum' parameter")
	    if (nscan() == 1)
		ID_NSUM(id,2) = ID_NSUM(id,1)
	    ID_NSUM(id,1) = max (1, ID_NSUM(id,1))
	    ID_NSUM(id,2) = max (1, ID_NSUM(id,2))
	}
	ID_MAXFEATURES(id) = clgeti ("maxfeatures")
	ID_MINSEP(id) = clgetd ("minsep")
	ID_MATCH(id) = clgetd ("match")
	ID_ZWIDTH(id) = clgetd ("identify.zwidth")
	ID_FTYPE(id) = clgwrd ("identify.ftype", Memc[str], SZ_LINE, FTYPES)
	ID_FWIDTH(id) = clgetd ("identify.fwidth")
	ID_CRADIUS(id) = clgetd ("cradius")
	ID_THRESHOLD(id) = clgetd ("threshold")
	call clgstr ("database", ID_DATABASE(id), ID_LENSTRING)
	call clgstr ("coordlist", ID_COORDLIST(id), ID_LENSTRING)
	ID_LABELS(id) = 1

	call id_mapll (id)
	ID_LOGFILES(id) = clpopnu ("logfiles")

	switch (clgwrd ("interactive", ans, SZ_FNAME, "|no|yes|NO|YES|")) {
	case 1, 3:
	    call strcpy ("NO", ans, 3)
	    ID_GP(id) = NULL
	case 2, 4:
	    # Open graphics
	    call clgstr ("graphics", Memc[logfile], SZ_FNAME)
	    ID_GP(id) = gopen (Memc[logfile], NEW_FILE+AW_DEFER, STDGRAPH)
	    call ic_pstr (ID_IC(id), "help", ICFITHELP)
	    call ic_pstr (ID_IC(id), "xlabel", "Feature positions")
	    call ic_pstr (ID_IC(id), "xunits", "pixels")
	    call ic_pstr (ID_IC(id), "ylabel", "")
	    call ic_pkey (ID_IC(id), 1, 'y', 'x')
	    call ic_pkey (ID_IC(id), 2, 'y', 'v')
	    call ic_pkey (ID_IC(id), 3, 'y', 'r')
	    call ic_pkey (ID_IC(id), 4, 'y', 'd')
	    call ic_pkey (ID_IC(id), 5, 'y', 'n')
	    call ic_puti (ID_IC(id), "key", 3)
	}

	# Open log and plot files.
	nlogfd = 0
	if (clgetb ("verbose")) {
	    nlogfd = 1
	    call malloc (logfd, nlogfd, TY_INT)
	    Memi[logfd] = STDOUT
	}
	while (clgfil (ID_LOGFILES(id), Memc[logfile], SZ_FNAME) != EOF) {
	    fd = open (Memc[logfile], APPEND, TEXT_FILE)
	    call fseti (fd, F_FLUSHNL, YES)
	    nlogfd = nlogfd + 1
	    if (nlogfd == 1)
		call malloc (logfd, nlogfd, TY_INT)
	    else
		call realloc (logfd, nlogfd, TY_INT)
	    Memi[logfd+nlogfd-1] = fd
	}
	call ri_loghdr (id, Memc[reference], Memi[logfd], nlogfd, 1)

	call clgstr ("plotfile", Memc[logfile], SZ_FNAME)
	if (nowhite (Memc[logfile], Memc[logfile], SZ_FNAME) > 0) {
	    fd = open (Memc[logfile], APPEND, BINARY_FILE)
	    pd = gopen ("stdvdm", NEW_FILE, fd)
	} else
	    pd = NULL

	ID_GT(id) = gt_init()
	call gt_sets (ID_GT(id), GTTYPE, "line")

	# Get and trace the reference solutions.
	call ri_reference (id, Memc[reference], crsearch, ans, Memi[logfd],
	    nlogfd, pd)

	# Expand the image template and reidentify features.
	while (imtgetim (list, ID_IMAGE(id), ID_LENSTRING) != EOF)
	    if (strne (Memc[reference], ID_IMAGE(id)))
		call ri_image (id, Memc[reference], ID_IMAGE(id), crsearch, ans,
		    Memi[logfd], nlogfd, pd)

	# Finish up.
	if (nlogfd > 0) {
	    do i = 1, nlogfd
	        call close (Memi[logfd+i-1])
	    call mfree (logfd, TY_INT)
	}

	if (ID_GP(id) != NULL)
	    call gclose (ID_GP(id))
	if (pd != NULL) {
	    call gclose (pd)
	    call close (fd)
	}
	call clpcls (ID_LOGFILES(id))
	call imtclose (list)
	call id_free (id)
	call smw_daxis (NULL, NULL, 0, 0, 0)
	call sfree (sp)
end


# RI_REFERENCE -- Set reference features.  Trace if needed.

procedure ri_reference (id, reference, crsearch, ans, logfd, nlogfd, pd)

pointer	id			# ID pointer
char	reference[ARB]		# Reference image
double	crsearch		# Search radius
char	ans[3]			# Interactive?
int	logfd[ARB]		# Logfiles
int	nlogfd			# Number of logfiles
pointer	pd			# Plot file pointer

int	step[2]
double	shift[2]
int	nreid
bool	override
bool	trace

int	i, apstart[2], start[2], line[2], loghdr
double	fit_shift[2]
pointer	ic, ic1
bool	clgetb()
int	clscan(), clgeti(), nscan(), id_dbcheck()
pointer	id_getap()
errchk	id_dbread

begin
	# Open the image and return if there is an error.
	call strcpy (reference, ID_IMAGE(id), ID_LENSTRING)
	iferr (call id_map (id)) {
	    call erract (EA_WARN)
	    iferr (call id_dbsave (id, ID_IMAGE(id)))
		call erract (EA_WARN)
	    return
	}

	# Get and save the reference database entry.
	call id_dbread (id, ID_IMAGE(id), ID_AP(id,1), NO, NO)
	call id_saveap (id)

	# Set parameters
	start[1] = ID_LINE(id,1)
	start[2] = ID_LINE(id,2)
	apstart[1] = ID_AP(id,1)
	apstart[2] = ID_AP(id,2)

	if (clscan ("step") == EOF)
	    call error (1, "Error in 'step' parameter")
	call gargi (step[1])
	call gargi (step[2])
	if (nscan() == 0)
	    call error (1, "Error in 'step' parameter")
	if (nscan() == 1)
	    step[2] = step[1]
	if (SMW_FORMAT(MW(ID_SH(id))) != SMW_ND) {
	    step[1] = min (step[1], 1)
	    step[2] = min (step[2], 1)
	}
	if (step[1] == 0)
	    step[1] = ID_MAXLINE(id,1)
	if (step[2] == 0)
	    step[2] = ID_MAXLINE(id,2)

	if (clscan ("shift") != EOF) {
	    call gargd (shift[1])
	    call gargd (shift[2])
	    if (nscan() == 0)
		call error (1, "Error in 'shift' parameter")
	    if (nscan() == 1)
		shift[2] = shift[1]
	}

	nreid = max (1, ID_NFEATURES(id) - clgeti ("nlost"))
	override = clgetb ("override")
	trace = clgetb ("trace")

	# Get and save other entries.
	if (!override) {
	    for (line[2]=start[2]; line[2]>0; line[2]=line[2]-step[2]) {
		ID_LINE(id,2) = line[2]
		ID_AP(id,2) = line[2]
		for (line[1]=start[1]; line[1]>0; line[1]=line[1]-step[1]) {
		    if (line[1]==start[1] && line[2]==start[2])
			next
		    ID_LINE(id,1) = line[1]
		    ID_AP(id,1) = line[1]
		    if (ID_APS(id) != NULL)
			ID_AP(id,1) = Memi[ID_APS(id)+line[1]-1]
		    ifnoerr (
			call id_dbread (id, ID_IMAGE(id), ID_AP(id,1),
			    NO, NO)) {
			call id_saveap (id)
		    }
		}
		for (line[1]=start[1]+step[1]; line[1]<=ID_MAXLINE(id,1);
		    line[1]=line[1]+step[1]) {
		    ID_LINE(id,1) = line[1]
		    ID_AP(id,1) = line[1]
		    if (ID_APS(id) != NULL)
			ID_AP(id,1) = Memi[ID_APS(id)+line[1]-1]
		    ifnoerr (call id_dbread (id, ID_IMAGE(id),
			ID_AP(id,1), NO, NO)) {
			call id_saveap (id)
		    }
		}
	    }
	    for (line[2]=start[2]+step[2]; line[2]<=ID_MAXLINE(id,2);
		line[2]=line[2]+step[2]) {
		ID_LINE(id,2) = line[2]
		ID_AP(id,2) = line[2]
		for (line[1]=start[1]-step[1]; line[1]>0;
		    line[1]=line[1]-step[1]) {
		    ID_LINE(id,1) = line[1]
		    ID_AP(id,1) = line[1]
		    if (ID_APS(id) != NULL)
			ID_AP(id,1) = Memi[ID_APS(id)+line[1]-1]
		    ifnoerr (
			call id_dbread (id, ID_IMAGE(id), ID_AP(id,1),
			    NO, NO)) {
			call id_saveap (id)
		    }
		}
		for (line[1]=start[1]+step[1]; line[1]<=ID_MAXLINE(id,1);
		    line[1]=line[1]+step[1]) {
		    ID_LINE(id,1) = line[1]
		    ID_AP(id,1) = line[1]
		    if (ID_APS(id) != NULL)
			ID_AP(id,1) = Memi[ID_APS(id)+line[1]-1]
		    ifnoerr (call id_dbread (id, ID_IMAGE(id),
			ID_AP(id,1), NO, NO)) {
			call id_saveap (id)
		    }
		}
	    }
	}

	# Reidentify.
	loghdr = 2
	ic = ID_IC(id)
	if (ans[1] == 'N')
	    ic1 = ic
	else {
	    call ic_open (ic1)
	    call ic_copy (ic, ic1)
	}

	fit_shift[2] = shift[2]
	for (line[2]=start[2]; line[2]>0; line[2]=line[2]-step[2]) {
	    ID_LINE(id,2) = line[2]
	    ID_AP(id,2) = line[2]
	    ID_IC(id) = ic

	    if (IS_INDEFD(shift[2]))
		fit_shift[2] = INDEFD
	    else {
		if (!trace)
		    fit_shift[2] = fit_shift[2] - shift[2]
		else
		    fit_shift[2] = -shift[2]
	    }

	    fit_shift[1] = fit_shift[2]
	    for (line[1]=start[1]; line[1]>0; line[1]=line[1]-step[1]) {
		if (line[1]==start[1] && line[2]==start[2])
		    next
		ID_LINE(id,1) = line[1]
		ID_AP(id,1) = line[1]
		ID_IC(id) = ic
		if (ID_APS(id) != NULL)
		    ID_AP(id,1) = Memi[ID_APS(id)+line[1]-1]
		if (!override)
		    if (id_dbcheck (id, ID_IMAGE(id), ID_AP(id,1)) == YES)
			next

		if (!trace) {
		    ID_NFEATURES(id) = 0
		    ID_AP(id,1) = apstart[1]
		    ID_AP(id,2) = apstart[2]
		    i = id_getap (id)
		    ID_LINE(id,1) = line[1]
		    ID_LINE(id,2) = line[2]
		}

		if (IS_INDEFD(shift[1]))
		    fit_shift[1] = INDEFD
		else {
		    if (!trace)
			fit_shift[1] = fit_shift[1] - shift[1]
		    else
			fit_shift[1] = -shift[1]
		}

		ID_IC(id) = ic1
		call id_gdata (id)
		iferr (call id_fitdata (id))
		    ;

		call ri_loghdr (id, reference, logfd, nlogfd, loghdr)
		loghdr = 0
		call ri_reidentify (id, fit_shift, crsearch, ans, logfd,
		    nlogfd, pd)

		if (ID_NFEATURES(id) < nreid) {
		    call ri_loghdr (id, reference, logfd, nlogfd, 3)
		    ID_NFEATURES(id) = 0
		    if (trace)
			break
		}

		if (ID_NFEATURES(id) > 0) {
		    call id_dbwrite (id, ID_IMAGE(id), ID_AP(id,1), NO)
		    call id_saveap (id)
		}
	    }

	    ID_IC(id) = ic
	    ID_NFEATURES(id) = 0
	    ID_AP(id,1) = apstart[1]
	    ID_AP(id,2) = apstart[2]
	    i = id_getap (id)
	    fit_shift[1] = fit_shift[2]
	    for (line[1]=start[1]+step[1]; line[1]<=ID_MAXLINE(id,1);
		line[1]=line[1]+step[1]) {
		ID_LINE(id,1) = line[1]
		ID_AP(id,1) = line[1]
		ID_IC(id) = ic
		if (ID_APS(id) != NULL)
		    ID_AP(id,1) = Memi[ID_APS(id)+line[1]-1]
		if (!override)
		    if (id_dbcheck (id, ID_IMAGE(id), ID_AP(id,1)) == YES)
			next

		if (!trace) {
		    ID_NFEATURES(id) = 0
		    ID_AP(id,1) = apstart[1]
		    ID_AP(id,2) = apstart[2]
		    i = id_getap (id)
		    ID_LINE(id,1) = line[1]
		    ID_LINE(id,2) = line[2]
		}

		if (IS_INDEFD(shift[1]))
		    fit_shift[1] = INDEFD
		else {
		    if (!trace)
			fit_shift[1] = fit_shift[1] + shift[1]
		    else
			fit_shift[1] = shift[1]
		}

		ID_IC(id) = ic1
		call id_gdata (id)
		iferr (call id_fitdata (id))
		    ;

		call ri_loghdr (id, reference, logfd, nlogfd, loghdr)
		loghdr = 0
		call ri_reidentify (id, fit_shift, crsearch, ans, logfd,
		    nlogfd, pd)

		if (ID_NFEATURES(id) < nreid) {
		    call ri_loghdr (id, reference, logfd, nlogfd, 3)
		    ID_NFEATURES(id) = 0
		    if (trace)
			break
		}

		if (ID_NFEATURES(id) > 0) {
		    call id_dbwrite (id, ID_IMAGE(id), ID_AP(id,1), NO)
		    call id_saveap (id)
		}
	    }
	}


	fit_shift[2] = 0.
	for (line[2]=start[2]+step[2]; line[2]<=ID_MAXLINE(id,2);
	    line[2]=line[2]+step[2]) {
	    ID_LINE(id,2) = line[2]
	    ID_AP(id,2) = line[2]
	    ID_IC(id) = ic

	    if (IS_INDEFD(shift[2]))
		fit_shift[2] = INDEFD
	    else {
		if (!trace)
		    fit_shift[2] = fit_shift[2] + shift[2]
		else
		    fit_shift[2] = shift[2]
	    }

	    fit_shift[1] = fit_shift[2]
	    for (line[1]=start[1]; line[1]>0; line[1]=line[1]-step[1]) {
		ID_LINE(id,1) = line[1]
		ID_AP(id,1) = line[1]
		ID_IC(id) = ic
		if (ID_APS(id) != NULL)
		    ID_AP(id,1) = Memi[ID_APS(id)+line[1]-1]
		if (!override)
		    if (id_dbcheck (id, ID_IMAGE(id), ID_AP(id,1)) == YES)
			next

		if (!trace) {
		    ID_NFEATURES(id) = 0
		    ID_AP(id,1) = apstart[1]
		    ID_AP(id,2) = apstart[2]
		    i = id_getap (id)
		    ID_LINE(id,1) = line[1]
		    ID_LINE(id,2) = line[2]
		}

		if (IS_INDEFD(shift[1]))
		    fit_shift[1] = INDEFD
		else {
		    if (!trace)
			fit_shift[1] = fit_shift[1] - shift[1]
		    else
			fit_shift[1] = -shift[1]
		}

		ID_IC(id) = ic1
		call id_gdata (id)
		iferr (call id_fitdata (id))
		    ;

		call ri_loghdr (id, reference, logfd, nlogfd, loghdr)
		loghdr = 0
		call ri_reidentify (id, fit_shift, crsearch, ans, logfd,
		    nlogfd, pd)

		if (ID_NFEATURES(id) < nreid) {
		    call ri_loghdr (id, reference, logfd, nlogfd, 3)
		    ID_NFEATURES(id) = 0
		    if (trace)
			break
		}

		if (ID_NFEATURES(id) > 0) {
		    call id_dbwrite (id, ID_IMAGE(id), ID_AP(id,1), NO)
		    call id_saveap (id)
		}
	    }

	    ID_IC(id) = ic
	    ID_NFEATURES(id) = 0
	    ID_AP(id,1) = apstart[1]
	    ID_AP(id,2) = apstart[2]
	    i = id_getap (id)
	    fit_shift[1] = fit_shift[2]
	    for (line[1]=start[1]+step[1]; line[1]<=ID_MAXLINE(id,1);
		line[1]=line[1]+step[1]) {
		ID_LINE(id,1) = line[1]
		ID_AP(id,1) = line[1]
		ID_IC(id) = ic
		if (ID_APS(id) != NULL)
		    ID_AP(id,1) = Memi[ID_APS(id)+line[1]-1]
		if (!override)
		    if (id_dbcheck (id, ID_IMAGE(id), ID_AP(id,1)) == YES)
			next

		if (!trace) {
		    ID_NFEATURES(id) = 0
		    ID_AP(id,1) = apstart[1]
		    ID_AP(id,2) = apstart[2]
		    i = id_getap (id)
		    ID_LINE(id,1) = line[1]
		    ID_LINE(id,2) = line[2]
		}

		if (IS_INDEFD(shift[1]))
		    fit_shift[1] = INDEFD
		else {
		    if (!trace)
			fit_shift[1] = fit_shift[1] + shift[1]
		    else
			fit_shift[1] = shift[1]
		}

		ID_IC(id) = ic1
		call id_gdata (id)
		iferr (call id_fitdata (id))
		    ;

		call ri_loghdr (id, reference, logfd, nlogfd, loghdr)
		loghdr = 0
		call ri_reidentify (id, fit_shift, crsearch, ans, logfd,
		    nlogfd, pd)

		if (ID_NFEATURES(id) < nreid) {
		    call ri_loghdr (id, reference, logfd, nlogfd, 3)
		    ID_NFEATURES(id) = 0
		    if (trace)
			break
		}

		if (ID_NFEATURES(id) > 0) {
		    call id_dbwrite (id, ID_IMAGE(id), ID_AP(id,1), NO)
		    call id_saveap (id)
		}
	    }
	}

	ID_IC(id) = ic
	if (ic != ic1)
	    call ic_closed (ic1)

	call smw_close (MW(ID_SH(id)))
	call imunmap (IM(ID_SH(id)))
	call shdr_close (ID_SH(id))
end


# RI_IMAGE -- Reidentify an image.

procedure ri_image (id, reference, image, crsearch, ans, logfd, nlogfd, pd)

pointer	id			# ID pointer
char	reference[ARB]		# Reference image
char	image[ARB]		# Image to be reidentified
double	crsearch		# Search radius
char	ans[3]			# Interactive?
int	logfd[ARB]		# Logfiles
int	nlogfd			# Number of logfiles
pointer	pd			# Plot file pointer

bool	newaps			# Add new apertures not in reference?
bool	override		# Override previous identifications?
bool	verbose			# Verbose output?

int	i, loghdr, id_dbcheck()
double	shift, fit_shift, clgetd()
pointer	sp, key, ic, ic1, stp, sid, stpmark
pointer	sthead(), stnext(), stname(), stfind(), id_getap()
bool	clgetb()

begin
	call smark (sp)
	call salloc (key, SZ_LINE, TY_CHAR)

	# Open the image and return if there is an error.
	call strcpy (image, ID_IMAGE(id), ID_LENSTRING)
	iferr (call id_map (id)) {
	    call erract (EA_WARN)
	    return
	}
	if (ID_DT(id) != NULL)
	    call dtunmap (ID_DT(id))

	newaps = clgetb ("newaps")
	override = clgetb ("override")
	verbose = clgetb ("verbose")

	ic = ID_IC(id)
	if (ans[1] == 'N')
	    ic1 = ic
	else {
	    call ic_open (ic1)
	    call ic_copy (ic, ic1)
	}

	loghdr = 2
	shift = clgetd ("shift")

	# For MULTISPEC search the reference list of each aperture.  If
	# a reference of the same aperture is not found and the newaps
	# flag is set use the initial reference and then add the
	# reidentification to the reference list.
	# For NDSPEC apply each reference to the image.

	stp = ID_STP(id)
	call stmark (stp, stpmark)
	if (SMW_FORMAT(MW(ID_SH(id))) == SMW_ES ||
	    SMW_FORMAT(MW(ID_SH(id))) == SMW_MS) {
	    for (i=1; i<=ID_MAXLINE(id,1); i=i+1) {
		ID_AP(id,1) = Memi[ID_APS(id)+i-1]
		ID_AP(id,2) = 1
		sid = id_getap (id)
		if (sid == NULL) {
		    if (!newaps) {
			if (verbose) {
			    call printf (
				"%s: Reference for aperture %d not found\n")
				call pargstr (image)
				call pargi (ID_AP(id,1))
			}
			next
		    }
		    if (crsearch != 0.)
			ID_NFEATURES(id) = 0
		}
		ID_LINE(id,1) = i

		if (i == 1 && ic != ic1)
		    call ic_copy (ic, ic1)

		if (!override)
		    if (id_dbcheck (id, ID_IMAGE(id), ID_AP(id,1)) == YES)
			next
		
		ID_IC(id) = ic1
		call id_gdata (id)
		iferr (call id_fitdata (id))
		    ;

		call ri_loghdr (id, reference, logfd, nlogfd, loghdr)
		loghdr = 0

		fit_shift = shift
		call ri_reidentify (id, fit_shift, crsearch, ans, logfd,
		    nlogfd, pd)

		if (ID_NFEATURES(id) > 0) {
		    call id_dbwrite (id, ID_IMAGE(id), ID_AP(id,1), NO)
		    if (sid == NULL && newaps) {
			call id_saveap (id)
			if (verbose) {
			    call printf (
				"%s: New reference for aperture %d\n")
				call pargstr (image)
				call pargi (ID_AP(id,1))
			} 
		    }
		}
		ID_IC(id) = ic
	    }

	} else {

	    # Go through the stored reference solutions.
	    # Because the symbol table might be changed in ri_reidentify
	    # save the key to restore the symbol pointer.

	    for (sid=sthead(stp); sid!=NULL; sid=stnext(stp,sid)) {
		call strcpy (Memc[stname(stp,sid)], Memc[key], SZ_LINE)
		call id_gid (id, sid)
		if (i == 1 && ic != ic1)
		    call ic_copy (ic, ic1)

		if (!override)
		    if (id_dbcheck (id, ID_IMAGE(id), ID_AP(id,1)) == YES)
			next
		
		ID_IC(id) = ic1
		call id_gdata (id)
		iferr (call id_fitdata (id))
		    ;

		call ri_loghdr (id, reference, logfd, nlogfd, loghdr)
		loghdr = 0

		fit_shift = shift
		call ri_reidentify (id, fit_shift, crsearch, ans, logfd,
		    nlogfd, pd)

		if (ID_NFEATURES(id) > 0)
		    call id_dbwrite (id, ID_IMAGE(id), ID_AP(id,1), NO)
		ID_IC(id) = ic
		sid = stfind (stp, Memc[key])
	    }
	    if (sid == NULL)
		ID_NFEATURES(id) = 0
	}

	ID_IC(id) = ic
	if (ic != ic1)
	    call ic_closed (ic1)
	call stfree (stp, stpmark)
	call smw_close (MW(ID_SH(id)))
	call imunmap (IM(ID_SH(id)))
	call shdr_close (ID_SH(id))
	call sfree (sp)
end


# RI_REIDENTIFY -- Reidentify features using a reference image database entry.

procedure ri_reidentify (id, fit_shift, crsearch, ans, logfd, nlogfd, pd)

pointer	id			# ID pointer
double	fit_shift		# Shift in fit coords (input and output)
double	crsearch		# Search radius
char	ans[3]			# Interactive?
int	logfd[ARB]		# Logfiles
int	nlogfd			# Number of logfiles
pointer	pd			# Plot file pointer

int	i, j, nfeatures1, nfeatures2, nfit, iden, mono, clgwrd()
double	shift, pix_shift, z_shift
double	clgetd(), id_fitpt(), fit_to_pix()
double	id_shift(), id_shift1(), id_center(), id_rms()
pointer	sp, str, pix, fit
bool	clgetb()
errchk	id_shift, id_shift1

begin
	call smark (sp)

	# Add features or determine a shift.
	nfeatures1 = ID_NFEATURES(id)
	if (nfeatures1 == 0) {
	    call salloc (str, SZ_LINE, TY_CHAR)
	    ID_FTYPE(id) =
		clgwrd ("identify.ftype", Memc[str], SZ_LINE, FTYPES)
	    ID_FWIDTH(id) = clgetd ("identify.fwidth")
	    if (crsearch != 0.)
		shift = id_shift (id, crsearch, -0.05D0)
	    else if (clgetb ("addfeatures")) {
		call id_linelist (id)
		shift = 0.
	    }
	} else if (IS_INDEFD(fit_shift)) {
	    ID_FWIDTH(id) = FWIDTH(id,1)
	    ID_FTYPE(id) = FTYPE(id,1)
	    if (IS_INDEFD(crsearch))
		shift = id_shift1 (id)
	    else if (crsearch != 0.)
		shift = id_shift (id, crsearch, -0.02D0)
	    else
		shift = 0.
	} else
	    shift = fit_shift

	nfeatures1 = ID_NFEATURES(id)
	if (nfeatures1 == 0)
	    call error (0, "No features in reference")
	call salloc (pix, nfeatures1, TY_DOUBLE)
	call salloc (fit, nfeatures1, TY_DOUBLE)
	call amovd (PIX(id,1), Memd[pix], nfeatures1)
	call amovd (FIT(id,1), Memd[fit], nfeatures1)

	# For each reference feature a shift is added to bring the pixel
	# position near that for the image being identified and then the
	# centering algorithm is used.  If the centering algorithm fails
	# the feature is discarded.  A mean shift is computed for the
	# features which have been reidentified.

	do i = 1, ID_NFEATURES(id) {
	    PIX(id,i) = fit_to_pix (id, FIT(id,i) + shift)
	    PIX(id,i) = id_center (id, PIX(id,i), FWIDTH(id,i), FTYPE(id,i))
	    if (!IS_INDEFD(PIX(id,i)))
	        FIT(id,i) = id_fitpt (id, PIX(id,i))
	}
	for (i=1; i<ID_NFEATURES(id); i=i+1) {
	    if (IS_INDEFD(PIX(id,i)))
		next
	    for (j=i+1; j<=ID_NFEATURES(id); j=j+1) {
	        if (IS_INDEFD(PIX(id,j)))
		    next
		if (abs (PIX(id,i)-PIX(id,j)) < ID_MINSEP(id)) {
		    if (abs (FIT(id,i)-USER(id,i)) < abs (FIT(id,j)-USER(id,j)))
			PIX(id,j) = INDEFD
		    else {
			PIX(id,i) = INDEFD
			break
		    }
		}
	    }
	}

	pix_shift = 0.
	fit_shift = 0.
	z_shift = 0.
	j = 0
	do i = 1, ID_NFEATURES(id) {
	    if (IS_INDEFD(PIX(id,i)))
		next

	    pix_shift = pix_shift + PIX(id,i) - Memd[pix+i-1]
	    fit_shift = fit_shift + FIT(id,i) - Memd[fit+i-1]
	    if (Memd[fit+i-1] != 0.)
	        z_shift = z_shift + (FIT(id,i) - Memd[fit+i-1]) / Memd[fit+i-1]

	    j = j + 1
	    PIX(id,j) = PIX(id,i)
	    FIT(id,j) = FIT(id,i)
	    USER(id,j) = USER(id,i)
	    WTS(id,j) = WTS(id,i)
	    FWIDTH(id,j) = FWIDTH(id,i)
	    FTYPE(id,j) = FTYPE(id,i)
	}
	ID_NFEATURES(id) = j
		    
	nfeatures2 = j
	pix_shift = pix_shift / max (1, ID_NFEATURES(id))
	fit_shift = fit_shift / max (1, ID_NFEATURES(id))
	z_shift = z_shift / max (1, ID_NFEATURES(id))

	# If refitting the coordinate function is requested and there is
	# more than one feature and there is a previously defined
	# coordinate function then refit.  Otherwise compute a coordinate
	# shift.

	mono = YES
	if (ID_REFIT(id)==YES && ID_CV(id)!=NULL && ID_NFEATURES(id)>1) {
	    if (clgetb("addfeatures") && abs(pix_shift) > 0.1*ID_NPTS(id)) {
		call id_doshift (id, NO)
		ID_NEWFEATURES(id) = YES
	    } else
		call id_dofit (id, NO)
	} else
	    call id_doshift (id, NO)
	if (ID_NEWCV(id) == YES) {
	    iferr (call id_fitdata (id))
		mono = NO
	    call id_fitfeatures (id)
	}

	if (clgetb ("addfeatures")) {
	    ID_FWIDTH(id) = FWIDTH(id,1)
	    ID_FTYPE(id) = FTYPE(id,1)
	    call id_linelist (id)
	    if (ID_NEWFEATURES(id) == YES) {
		if (ID_REFIT(id) == YES && ID_CV(id) != NULL)
	    	    call id_dofit (id, NO)
		else
		    call id_doshift (id, NO)
		if (ID_NEWCV(id) == YES) {
	    	    iferr (call id_fitdata (id))
			mono = NO
		    call id_fitfeatures (id)
		}
	    }
	}

	# Enter fitting interactively.
	iden = NO
	if ((ID_NFEATURES(id)>1) && (ID_CV(id)!=NULL)) {
	    if (ans[1] != 'N') {
	        if (ans[1] != 'Y') {
		    nfit = 0
		    for (j=1; j<=ID_NFEATURES(id); j=j+1)
		        if (WTS(id,j) > 0.)
			    nfit = nfit + 1
	            call printf (
		    "%s%s%23t%3d/%-3d %3d/%-3d %9.3g  %10.3g  %7.3g  %7.3g\n")
			call pargstr (ID_IMAGE(id))
			call pargstr (ID_SECTION(id))
			call pargi (nfeatures2)
			call pargi (nfeatures1)
			call pargi (nfit)
			call pargi (ID_NFEATURES(id))
			call pargd (pix_shift)
			call pargd (fit_shift)
			call pargd (z_shift)
			call pargd (id_rms(id))
	            call flush (STDOUT)
		    repeat {
	                ifnoerr (i = clgwrd ("answer", ans, SZ_FNAME,
			    "|no|yes|NO|YES|"))
			    break
		    }
		    call clpstr ("answer", ans)
		}
	        switch (ans[1]) {
	        case 'y', 'Y':
		    mono = YES
		    i = ID_REFIT(id)
		    call reidentify (id)
		    ID_REFIT(id) = i
		    iden = YES
		}
		if (ans[1] != 'Y')
		    call gdeactivate (ID_GP(id), 0)
	    }
	}

	# Record log information if a log file descriptor is given.
	for (i = 1; i <= nlogfd; i = i + 1) {
	    if (ans[1] == 'n' && logfd[i] == STDOUT)
		    next
	    nfit = 0
	    for (j=1; j<=ID_NFEATURES(id); j=j+1)
		if (WTS(id,j) > 0.)
		    nfit = nfit + 1
	        call fprintf (logfd[i],
		    "%s%s%23t%3d/%-3d %3d/%-3d %9.3g  %10.3g  %7.3g  %7.3g\n")
		    call pargstr (ID_IMAGE(id))
		    call pargstr (ID_SECTION(id))
		    call pargi (nfeatures2)
		    call pargi (nfeatures1)
		    call pargi (nfit)
		    call pargi (ID_NFEATURES(id))
		    call pargd (pix_shift)
		    call pargd (fit_shift)
		    call pargd (z_shift)
		    call pargd (id_rms(id))
	    if (mono == NO)
		call fprintf (logfd[i], "Non-monotonic dispersion function")
	    call flush (logfd[i])
	    if (logfd[i] == STDOUT)
		iden = NO
	}
	# Print log if STDOUT is not used but if the IDENTIFY is done.
	if (iden == YES) {
	    call printf (
		"%s%s%23t%3d/%-3d %3d/%-3d %9.3g  %10.3g  %7.3g  %7.3g\n")
		call pargstr (ID_IMAGE(id))
		call pargstr (ID_SECTION(id))
		call pargi (nfeatures2)
		call pargi (nfeatures1)
		call pargi (nfit)
		call pargi (ID_NFEATURES(id))
		call pargd (pix_shift)
		call pargd (fit_shift)
		call pargd (z_shift)
		call pargd (id_rms(id))
	    if (mono == NO)
		call printf ("Non-monotonic dispersion function")
	    call flush (STDOUT)
	}
		
	# Make log plot.
	call ri_plot (id, pd)

	call sfree (sp)
end


# RI_LOGHDR -- Print a log header in the log files.

procedure ri_loghdr (id, reference, logfd, nlogfd, flag)

pointer	id		# Identify structure
char	reference[ARB]	# Reference image
int	logfd[ARB]	# Log file descriptors
int	nlogfd		# Number of log files
int	flag		# Header type flag (1=banner, 2=Column labels, 3=Error)

int	i
pointer	str

begin
	for (i = 1; i <= nlogfd; i = i + 1) {
	    switch (flag) {
	    case 1:	# Print ID
		call malloc (str, SZ_LINE, TY_CHAR)
	        call sysid (Memc[str], SZ_LINE)
	        call fprintf (logfd[i], "\nREIDENTIFY: %s\n")
		    call pargstr (Memc[str])
		call mfree (str, TY_CHAR)
	    case 2:	# Print labels
	        call fprintf (logfd[i],
		    "  Reference image = %s, New image = %s, Refit = %b\n")
		    call pargstr (reference)
		    call pargstr (ID_IMAGE(id))
		    call pargb (ID_REFIT(id) == YES)
		call fprintf (logfd[i],
		    "%20s  %7s %7s %9s  %10s  %7s  %7s\n")
		    call pargstr ("Image Data")
		    call pargstr ("Found")
		    call pargstr ("Fit")
		    call pargstr ("Pix Shift")
		    call pargstr ("User Shift")
		    call pargstr ("Z Shift")
		    call pargstr ("RMS")
	    case 3:	# Error
		call fprintf (logfd[i], "    ** Too many features lost **\n")
	    }
	}
end


# RI_PLOT -- Plot residual graph of reidentified lines.

procedure ri_plot (id, pd)

pointer	id				# ID pointer
pointer	pd				# GIO pointer

int	i, j
pointer	sp, str, x, y, gt, gt_init()

begin
	# Check if there is anything to plot.
	if (pd == NULL || ID_NFEATURES(id) == 0)
	    return

	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (x, ID_NFEATURES(id), TY_REAL)
	call salloc (y, ID_NFEATURES(id), TY_REAL)

	# Set plot points.
	j = 0
	do i = 1, ID_NFEATURES(id) {
	    if (IS_INDEFD(USER(id,i)))
		break

	    Memr[x+j] = USER(id,i)
	    Memr[y+j] = FIT(id,i) - USER(id,i)
	    j = j + 1
	}

	if (j == 0) {
	    call sfree (sp)
	    return
	}

	# Make the plot.
	call sprintf (Memc[str], SZ_LINE, "Reidentify: %s")
	    call pargstr (ID_IMAGE(id))
	gt = gt_init ()
	call gt_sets (gt, GTTYPE, "mark")
	call gt_sets (gt, GTXLABEL, "user coordinates")
	call gt_sets (gt, GTYLABEL, "residuals (fit - user)")
	call gt_sets (gt, GTTITLE, Memc[str])
	call gclear (pd)
	call gascale (pd, Memr[x], j, 1)
	call gascale (pd, Memr[y], j, 2)
	call gt_swind (pd, gt)
	call gt_labax (pd, gt)
	call gt_plot (pd, gt, Memr[x], Memr[y], j)
	call gt_free (gt)

	call sfree (sp)
end
