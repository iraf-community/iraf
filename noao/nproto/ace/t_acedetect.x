include	<error.h>
include	<fset.h>
include	<imset.h>
include	<pmset.h>
include	<imhdr.h>
include	"ace.h"
include	"acedetect.h"
include	"cat.h"


# T_ACEDETECT -- Detect objects in images.
# This entry procedure simply sets up the parameters.

procedure t_acedetect ()

pointer	par			# Parameters

pointer	sp, str

bool	clgetb()
int	clgwrd(), imtopenp(), imtopen(), clpopnu(), fntopnb()

begin
	call smark (sp)
	call salloc (par, PAR_LEN, TY_STRUCT)
	call salloc (str, SZ_LINE, TY_CHAR)
	call aclri (Memi[par], PAR_LEN)

	# Get list parameters.
	PAR_IMLIST(par,1) = imtopenp ("images")
	PAR_BPMLIST(par,1) = imtopenp ("masks")
	PAR_SKYLIST(par,1) = imtopenp ("skys")
	PAR_SIGLIST(par,1) = imtopenp ("sigmas")
	PAR_EXPLIST(par,1) = imtopenp ("exps")
	PAR_GAINLIST(par,1) = imtopenp ("gains")
	PAR_SCALELIST(par,1) = fntopnb ("", NO)

	PAR_IMLIST(par,2) = imtopen ("")
	PAR_BPMLIST(par,2) = imtopen ("")
	PAR_SKYLIST(par,2) = imtopen ("")
	PAR_SIGLIST(par,2) = imtopen ("")
	PAR_EXPLIST(par,2) = imtopen ("")
	PAR_GAINLIST(par,2) = imtopen ("")
	PAR_SCALELIST(par,2) = fntopnb ("", NO)

	PAR_OMLIST(par) = imtopenp ("objmasks")
	PAR_OMTYPE(par) = clgwrd ("omtype", Memc[str], SZ_LINE, OM_TYPES)
	PAR_INCATLIST(par) = imtopen ("")
	PAR_OUTCATLIST(par) = imtopenp ("catalogs")
	PAR_CATDEFLIST(par) = clpopnu ("catdefs")
	PAR_LOGLIST(par) = clpopnu ("logfiles")

	PAR_OUTSKYLIST(par) = imtopen ("")
	PAR_OUTSIGLIST(par) = imtopen ("")

	call clgstr ("extnames", PAR_EXTNAMES(par), PAR_SZSTR)

	# Get other parameters.
	# The parameter structures flag whether an operation is requested.
	#if (clgetb ("dosky"))
	    call sky_pars ("open", "", PAR_SKY(par))
	if (clgetb ("dodetect"))
	    call det_pars ("open", "", PAR_DET(par))
	if (clgetb ("dosplit"))
	    call spt_pars ("open", "", PAR_SPT(par))
	if (clgetb ("dogrow"))
	    call grw_pars ("open", "", PAR_GRW(par))
	if (clgetb ("doevaluate"))
	    call evl_pars ("open", "", PAR_EVL(par))

	# Do the detection.
	call aceall (par)

	# Finish up.
	call sky_pars ("close", "", PAR_SKY(par))
	call det_pars ("close", "", PAR_DET(par))
	call spt_pars ("close", "", PAR_SPT(par))
	call grw_pars ("close", "", PAR_GRW(par))
	call evl_pars ("close", "", PAR_EVL(par))

	call imtclose (PAR_OUTSIGLIST(par))
	call imtclose (PAR_OUTSKYLIST(par))

	call clpcls (PAR_LOGLIST(par))
	call imtclose (PAR_OMLIST(par))
	call clpcls (PAR_CATDEFLIST(par))
	call imtclose (PAR_OUTCATLIST(par))
	call imtclose (PAR_INCATLIST(par))

	call clpcls (PAR_SCALELIST(par,2))
	call imtclose (PAR_GAINLIST(par,2))
	call imtclose (PAR_EXPLIST(par,2))
	call imtclose (PAR_SIGLIST(par,2))
	call imtclose (PAR_SKYLIST(par,2))
	call imtclose (PAR_BPMLIST(par,2))
	call imtclose (PAR_IMLIST(par,2))

	call clpcls (PAR_SCALELIST(par,1))
	call imtclose (PAR_GAINLIST(par,1))
	call imtclose (PAR_EXPLIST(par,1))
	call imtclose (PAR_SIGLIST(par,1))
	call imtclose (PAR_SKYLIST(par,1))
	call imtclose (PAR_BPMLIST(par,1))
	call imtclose (PAR_IMLIST(par,1))

	call sfree (sp)
end


# T_ACEEVALUATE -- Evaluate objects.
# This entry procedure simply sets up the parameters.

procedure t_aceevaluate ()

pointer	par			# Parameters

pointer	sp, str

int	imtopenp(), imtopen(), clpopnu(), fntopnb()

begin
	call smark (sp)
	call salloc (par, PAR_LEN, TY_STRUCT)
	call salloc (str, SZ_LINE, TY_CHAR)
	call aclri (Memi[par], PAR_LEN)

	# Get list parameters.
	PAR_IMLIST(par,1) = imtopenp ("images")
	PAR_BPMLIST(par,1) = imtopen ("")
	PAR_SKYLIST(par,1) = imtopenp ("skys")
	PAR_SIGLIST(par,1) = imtopenp ("sigmas")
	PAR_EXPLIST(par,1) = imtopenp ("exps")
	PAR_GAINLIST(par,1) = imtopenp ("gains")
	PAR_SCALELIST(par,1) = fntopnb ("", NO)

	PAR_IMLIST(par,2) = imtopen ("")
	PAR_BPMLIST(par,2) = imtopen ("")
	PAR_SKYLIST(par,2) = imtopen ("")
	PAR_SIGLIST(par,2) = imtopen ("")
	PAR_EXPLIST(par,2) = imtopen ("")
	PAR_GAINLIST(par,2) = imtopen ("")
	PAR_SCALELIST(par,2) = fntopnb ("", NO)

	PAR_OMLIST(par) = imtopenp ("objmasks")
	PAR_OMTYPE(par) = OM_ALL
	PAR_INCATLIST(par) = imtopenp ("incatalogs")
	PAR_OUTCATLIST(par) = imtopenp ("outcatalogs")
	PAR_CATDEFLIST(par) = clpopnu ("catdefs")
	PAR_LOGLIST(par) = clpopnu ("logfiles")

	PAR_OUTSKYLIST(par) = imtopen ("")
	PAR_OUTSIGLIST(par) = imtopen ("")

	# Get other parameters.
	# The parameter structures flag whether an operation is requested.
	call sky_pars ("open", "", PAR_SKY(par))
	call evl_pars ("open", "", PAR_EVL(par))

	# Do the detection.
	call aceall (par)

	# Finish up.
	call sky_pars ("close", "", PAR_SKY(par))
	call det_pars ("close", "", PAR_DET(par))
	call spt_pars ("close", "", PAR_SPT(par))
	call grw_pars ("close", "", PAR_GRW(par))
	call evl_pars ("close", "", PAR_EVL(par))

	call imtclose (PAR_OUTSIGLIST(par))
	call imtclose (PAR_OUTSKYLIST(par))

	call clpcls (PAR_LOGLIST(par))
	call imtclose (PAR_OMLIST(par))
	call clpcls (PAR_CATDEFLIST(par))
	call imtclose (PAR_INCATLIST(par))
	call imtclose (PAR_OUTCATLIST(par))

	call clpcls (PAR_SCALELIST(par,2))
	call imtclose (PAR_GAINLIST(par,2))
	call imtclose (PAR_EXPLIST(par,2))
	call imtclose (PAR_SIGLIST(par,2))
	call imtclose (PAR_SKYLIST(par,2))
	call imtclose (PAR_BPMLIST(par,2))
	call imtclose (PAR_IMLIST(par,2))

	call clpcls (PAR_SCALELIST(par,1))
	call imtclose (PAR_GAINLIST(par,1))
	call imtclose (PAR_EXPLIST(par,1))
	call imtclose (PAR_SIGLIST(par,1))
	call imtclose (PAR_SKYLIST(par,1))
	call imtclose (PAR_BPMLIST(par,1))
	call imtclose (PAR_IMLIST(par,1))

	call sfree (sp)
end


# T_ACESKY -- Output sky images.
# This entry procedure simply sets up the parameters.

procedure t_acesky ()

pointer	par			# Parameters

pointer	sp, str

int	imtopenp(), imtopen(), clpopnu(), fntopnb()

begin
	call smark (sp)
	call salloc (par, PAR_LEN, TY_STRUCT)
	call salloc (str, SZ_LINE, TY_CHAR)
	call aclri (Memi[par], PAR_LEN)

	# Get list parameters.
	PAR_IMLIST(par,1) = imtopenp ("images")
	PAR_OUTSKYLIST(par) = imtopenp ("skyimages")
	PAR_OUTSIGLIST(par) = imtopenp ("sigmaimages")
	PAR_BPMLIST(par,1) = imtopen ("")
	PAR_SKYLIST(par,1) = imtopenp ("skys")
	PAR_SIGLIST(par,1) = imtopenp ("sigmas")
	PAR_EXPLIST(par,1) = imtopenp ("exps")
	PAR_GAINLIST(par,1) = imtopenp ("gains")
	PAR_SCALELIST(par,1) = fntopnb ("", NO)

	PAR_IMLIST(par,2) = imtopen ("")
	PAR_BPMLIST(par,2) = imtopen ("")
	PAR_SKYLIST(par,2) = imtopen ("")
	PAR_SIGLIST(par,2) = imtopen ("")
	PAR_EXPLIST(par,2) = imtopen ("")
	PAR_GAINLIST(par,2) = imtopen ("")
	PAR_SCALELIST(par,2) = fntopnb ("", NO)

	PAR_OMLIST(par) = imtopen ("")
	PAR_OMTYPE(par) = OM_ALL
	PAR_INCATLIST(par) = imtopen ("")
	PAR_OUTCATLIST(par) = imtopen ("")
	PAR_CATDEFLIST(par) = fntopnb ("", NO)
	PAR_LOGLIST(par) = clpopnu ("logfiles")

	# Do the detection.
	call aceall (par)

	# Finish up.
	call sky_pars ("close", "", PAR_SKY(par))
	call det_pars ("close", "", PAR_DET(par))
	call spt_pars ("close", "", PAR_SPT(par))
	call grw_pars ("close", "", PAR_GRW(par))
	call evl_pars ("close", "", PAR_EVL(par))

	call imtclose (PAR_OUTSIGLIST(par))
	call imtclose (PAR_OUTSKYLIST(par))

	call clpcls (PAR_LOGLIST(par))
	call imtclose (PAR_OMLIST(par))
	call clpcls (PAR_CATDEFLIST(par))
	call imtclose (PAR_INCATLIST(par))
	call imtclose (PAR_OUTCATLIST(par))

	call clpcls (PAR_SCALELIST(par,2))
	call imtclose (PAR_GAINLIST(par,2))
	call imtclose (PAR_EXPLIST(par,2))
	call imtclose (PAR_SIGLIST(par,2))
	call imtclose (PAR_SKYLIST(par,2))
	call imtclose (PAR_BPMLIST(par,2))
	call imtclose (PAR_IMLIST(par,2))

	call clpcls (PAR_SCALELIST(par,1))
	call imtclose (PAR_GAINLIST(par,1))
	call imtclose (PAR_EXPLIST(par,1))
	call imtclose (PAR_SIGLIST(par,1))
	call imtclose (PAR_SKYLIST(par,1))
	call imtclose (PAR_BPMLIST(par,1))
	call imtclose (PAR_IMLIST(par,1))

	call sfree (sp)
end


# T_DIFFDETECT -- Detect objects in the difference of images.

procedure t_diffdetect ()

pointer	par			# Parameters

pointer	sp, str

int	imtopenp(), imtopen(), clpopnu()

begin
	call smark (sp)
	call salloc (par, PAR_LEN, TY_STRUCT)
	call salloc (str, SZ_LINE, TY_CHAR)
	call aclri (Memi[par], PAR_LEN)

	# Get list parameters.
	PAR_IMLIST(par,1) = imtopenp ("images")
	PAR_BPMLIST(par,1) = imtopenp ("masks")
	PAR_SKYLIST(par,1) = imtopenp ("skys")
	PAR_SIGLIST(par,1) = imtopenp ("sigmas")
	PAR_EXPLIST(par,1) = imtopenp ("exps")
	PAR_GAINLIST(par,1) = imtopenp ("gains")
	PAR_SCALELIST(par,1) = clpopnu ("scales")

	PAR_IMLIST(par,2) = imtopenp ("rimages")
	PAR_BPMLIST(par,2) = imtopenp ("rmasks")
	PAR_SKYLIST(par,2) = imtopenp ("rskys")
	PAR_SIGLIST(par,2) = imtopenp ("rsigmas")
	PAR_EXPLIST(par,2) = imtopenp ("rexps")
	PAR_GAINLIST(par,2) = imtopen ("")
	PAR_SCALELIST(par,2) = clpopnu ("rscales")

	PAR_OMLIST(par) = imtopenp ("objmasks")
	PAR_OMTYPE(par) = OM_ALL
	PAR_INCATLIST(par) = imtopen ("")
	PAR_OUTCATLIST(par) = imtopenp ("catalogs")
	PAR_CATDEFLIST(par) = clpopnu ("catdefs")
	PAR_LOGLIST(par) = clpopnu ("logfiles")

	PAR_OUTSKYLIST(par) = imtopen ("")
	PAR_OUTSIGLIST(par) = imtopen ("")

	# Get other parameters.
	call sky_pars ("open", "", PAR_SKY(par))
	call det_pars ("diff", "", PAR_DET(par))
	call grw_pars ("open", "", PAR_GRW(par))
	call evl_pars ("open", "", PAR_EVL(par))

	# Do the detection.
	call aceall (par)

	# Finish up.
	call sky_pars ("close", "", PAR_SKY(par))
	call det_pars ("close", "", PAR_DET(par))
	call spt_pars ("close", "", PAR_SPT(par))
	call grw_pars ("close", "", PAR_GRW(par))
	call evl_pars ("close", "", PAR_EVL(par))

	call imtclose (PAR_OUTSIGLIST(par))
	call imtclose (PAR_OUTSKYLIST(par))

	call clpcls (PAR_LOGLIST(par))
	call imtclose (PAR_OMLIST(par))
	call clpcls (PAR_CATDEFLIST(par))
	call imtclose (PAR_INCATLIST(par))
	call imtclose (PAR_OUTCATLIST(par))

	call clpcls (PAR_SCALELIST(par,2))
	call imtclose (PAR_GAINLIST(par,2))
	call imtclose (PAR_EXPLIST(par,2))
	call imtclose (PAR_SIGLIST(par,2))
	call imtclose (PAR_SKYLIST(par,2))
	call imtclose (PAR_BPMLIST(par,2))
	call imtclose (PAR_IMLIST(par,2))

	call clpcls (PAR_SCALELIST(par,1))
	call imtclose (PAR_GAINLIST(par,1))
	call imtclose (PAR_EXPLIST(par,1))
	call imtclose (PAR_SIGLIST(par,1))
	call imtclose (PAR_SKYLIST(par,1))
	call imtclose (PAR_BPMLIST(par,1))
	call imtclose (PAR_IMLIST(par,1))

	call sfree (sp)
end



# ACEALL -- Expand input list and set filenames.
# This calls ACE for each image to be analyzed.

procedure aceall (par)

pointer	par			#I Parameters

int	i, j, k, list, imext
pointer	sp, str
pointer	image[4], bpmask[4], skyname[4], signame[4], expname[4], gainname[4]
pointer	incat[2], outcat[2], objmask[2], outsky[2], outsig[2], scalestr[2]
pointer	catdef, logfile
pointer	im, ptr

int	nowhite(), mscextensions(), strldxs(), strlen()
int	imtlen(), imtgetim(), clplen(), clgfil()
pointer	immap()
errchk	immap

begin
	call smark (sp)

	# Allocate memory for all the file names.  The first half of each
	# array of names is for image names including extensions and the
	# second half is for cluster names.   The names are initialized
	# to EOS and are only filled in if specified.

	do j = 1, 4 {
	    call salloc (image[j], SZ_FNAME, TY_CHAR)
	    call salloc (bpmask[j], SZ_FNAME, TY_CHAR)
	    call salloc (skyname[j], SZ_FNAME, TY_CHAR)
	    call salloc (signame[j], SZ_FNAME, TY_CHAR)
	    call salloc (expname[j], SZ_FNAME, TY_CHAR)
	    call salloc (gainname[j], SZ_FNAME, TY_CHAR)
	    Memc[image[j]] = EOS
	    Memc[bpmask[j]] = EOS
	    Memc[skyname[j]] = EOS
	    Memc[signame[j]] = EOS
	    Memc[expname[j]] = EOS
	    Memc[gainname[j]] = EOS
	}
	do j = 1, 2 {
	    call salloc (objmask[j], SZ_FNAME, TY_CHAR)
	    call salloc (incat[j], SZ_FNAME, TY_CHAR)
	    call salloc (outcat[j], SZ_FNAME, TY_CHAR)
	    call salloc (outsky[j], SZ_FNAME, TY_CHAR)
	    call salloc (outsig[j], SZ_FNAME, TY_CHAR)
	    call salloc (scalestr[j], SZ_FNAME, TY_CHAR)
	    Memc[objmask[j]] = EOS
	    Memc[incat[j]] = EOS
	    Memc[outcat[j]] = EOS
	    Memc[outsky[j]] = EOS
	    Memc[outsig[j]] = EOS
	    Memc[scalestr[j]] = EOS
	}
	call salloc (catdef, SZ_FNAME, TY_CHAR)
	call salloc (logfile, SZ_FNAME, TY_CHAR)
	Memc[catdef] = EOS
	Memc[logfile] = EOS

	call salloc (str, SZ_LINE, TY_CHAR)

	# Check lists match. 
	j = imtlen (PAR_IMLIST(par,1))
	i = imtlen (PAR_BPMLIST(par,1))
	if (i > 1 && i != j)
	    call error (1,
		"Image and bad pixel mask lists do not match")
	i = imtlen (PAR_SKYLIST(par,1))
	if (i > 1 && i != j)
	    call error (1,
		"Image and sky lists do not match")
	i = imtlen (PAR_SIGLIST(par,1))
	if (i > 1 && i != j)
	    call error (1,
		"Image and sky sigma lists do not match")
	i = imtlen (PAR_EXPLIST(par,1))
	if (i > 1 && i != j)
	    call error (1,
		"Image and exposure map lists do not match")
	i = imtlen (PAR_GAINLIST(par,1))
	if (i > 1 && i != j)
	    call error (1,
		"Image and measurement gain lists do not match")
	i = clplen (PAR_SCALELIST(par,1))
	if (i > 1 && i != j)
	    call error (1,
		"Image and scale lists do not match")

	k = imtlen (PAR_IMLIST(par,2))
	if (k > 1 && i != j)
	    call error (1,
		"Image and reference lists do not match")
	i = imtlen (PAR_BPMLIST(par,2))
	if (i > 1 && i != k)
	    call error (1,
		"Reference image  bad pixel mask lists do not match")
	i = imtlen (PAR_SKYLIST(par,2))
	if (i > 1 && i != k)
	    call error (1,
		"Reference image and sky lists do not match")
	i = imtlen (PAR_SIGLIST(par,2))
	if (i > 1 && i != k)
	    call error (1,
		"Reference image and sky sigma lists do not match")
	i = imtlen (PAR_EXPLIST(par,2))
	if (i > 1 && i != k)
	    call error (1,
		"Reference image and exposure map lists do not match")
	i = imtlen (PAR_GAINLIST(par,2))
	if (i > 1 && i != j)
	    call error (1,
		"Reference image and measurement gain lists do not match")
	i = clplen (PAR_SCALELIST(par,2))
	if (i > 1 && i != k)
	    call error (1,
		"Reference image and scale lists do not match")

	i = clplen (PAR_INCATLIST(par))
	if (i > 0 && i != j)
	    call error (1,
		"Input image and input catalog lists do not match")
	i = clplen (PAR_OUTCATLIST(par))
	if (i > 0 && i != j)
	    call error (1,
		"Input image and output catalog lists do not match")
	i = clplen (PAR_CATDEFLIST(par))
	if (i > 1 && i != j)
	    call error (1,
		"Input image and catalog definition lists do not match")
	i = imtlen (PAR_OMLIST(par))
	if (i > 0 && i != j)
	    call error (1,
		"Input image and object mask lists do not match")
	i = clplen (PAR_LOGLIST(par))
	if (i > 1 && i != j)
	    call error (1,
		"Input image and logfile lists do not match")
	i = imtlen (PAR_OUTSKYLIST(par))
	if (i > 0 && i != j)
	    call error (1,
		"Input image and output sky lists do not match")
	i = imtlen (PAR_OUTSIGLIST(par))
	if (i > 0 && i != j)
	    call error (1,
		"Input image and output sigma lists do not match")

	# Do each input image cluster.
	while (imtgetim (PAR_IMLIST(par,1), Memc[image[1]], SZ_FNAME) != EOF) {
	    if (imtgetim (PAR_IMLIST(par,2), Memc[str], SZ_LINE) != EOF)
		call strcpy (Memc[str], Memc[image[2]], SZ_FNAME)

	    # Get associated cluster names.
	    # Initialize image names to the cluster names.
	    # Strip whitespace to check for no name.
	    do j = 1, 2 {
		if (imtgetim (PAR_BPMLIST(par,j), Memc[str], SZ_LINE) != EOF)
		    call strcpy (Memc[str], Memc[bpmask[j]], SZ_FNAME)
		if (imtgetim (PAR_SKYLIST(par,j), Memc[str], SZ_LINE) != EOF)
		    call strcpy (Memc[str], Memc[skyname[j]], SZ_FNAME)
		if (imtgetim (PAR_SIGLIST(par,j), Memc[str], SZ_LINE) != EOF)
		    call strcpy (Memc[str], Memc[signame[j]], SZ_FNAME)
		if (imtgetim (PAR_EXPLIST(par,j), Memc[str], SZ_LINE) != EOF)
		    call strcpy (Memc[str], Memc[expname[j]], SZ_FNAME)
		if (imtgetim (PAR_GAINLIST(par,j), Memc[str], SZ_LINE) != EOF)
		    call strcpy (Memc[str], Memc[gainname[j]], SZ_FNAME)
		if (clgfil (PAR_SCALELIST(par,j), Memc[str], SZ_LINE) != EOF)
		    call strcpy (Memc[str], Memc[scalestr[j]], SZ_FNAME)

		i = nowhite (Memc[bpmask[j]], Memc[bpmask[j]], SZ_FNAME)
		i = nowhite (Memc[skyname[j]], Memc[skyname[j]], SZ_FNAME)
		i = nowhite (Memc[signame[j]], Memc[signame[j]], SZ_FNAME)
		i = nowhite (Memc[expname[j]], Memc[expname[j]], SZ_FNAME)
		i = nowhite (Memc[gainname[j]], Memc[gainname[j]], SZ_FNAME)
		i = nowhite (Memc[scalestr[j]], Memc[scalestr[j]], SZ_FNAME)
	    }

	    if (clgfil (PAR_INCATLIST(par), Memc[str], SZ_LINE) != EOF)
		call strcpy (Memc[str], Memc[incat[1]], SZ_FNAME)
	    if (clgfil (PAR_OUTCATLIST(par), Memc[str], SZ_LINE) != EOF)
		call strcpy (Memc[str], Memc[outcat[1]], SZ_FNAME)
	    if (imtgetim (PAR_OMLIST(par), Memc[str], SZ_LINE) != EOF)
		call strcpy (Memc[str], Memc[objmask[1]], SZ_FNAME)
	    if (imtgetim (PAR_OUTSKYLIST(par), Memc[str], SZ_LINE) != EOF)
		call strcpy (Memc[str], Memc[outsky[1]], SZ_FNAME)
	    if (imtgetim (PAR_OUTSIGLIST(par), Memc[str], SZ_LINE) != EOF)
		call strcpy (Memc[str], Memc[outsig[1]], SZ_FNAME)
	    if (clgfil (PAR_CATDEFLIST(par), Memc[str], SZ_LINE) != EOF)
		call strcpy (Memc[str], Memc[catdef], SZ_FNAME)
	    if (clgfil (PAR_LOGLIST(par), Memc[str], SZ_LINE) != EOF)
		call strcpy (Memc[str], Memc[logfile], SZ_FNAME)

	    i = nowhite (Memc[incat[1]], Memc[incat[1]], SZ_FNAME)
	    i = nowhite (Memc[outcat[1]], Memc[outcat[1]], SZ_FNAME)
	    i = nowhite (Memc[objmask[1]], Memc[objmask[1]], SZ_FNAME)
	    i = nowhite (Memc[outsky[1]], Memc[outsky[1]], SZ_FNAME)
	    i = nowhite (Memc[outsig[1]], Memc[outsig[1]], SZ_FNAME)
	    i = nowhite (Memc[catdef], Memc[catdef], SZ_FNAME)
	    i = nowhite (Memc[logfile], Memc[logfile], SZ_FNAME)

	    # Expand clusters to images.  As a special case, if the input is
	    # an explicit extension image then don't treat the filenames as MEF.
	    list = mscextensions (Memc[image[1]], "0-", PAR_EXTNAMES(par),
	        "", NO, YES, NO, "", NO, imext)
	    if (strldxs ("[", Memc[image[1]]) != 0)
		imext = NO
	    while (imtgetim (list, Memc[image[3]], SZ_FNAME) != EOF) {
		call strcpy (Memc[image[2]], Memc[image[4]], SZ_FNAME)
		do j = 1, 2 {
		    call strcpy (Memc[bpmask[j]], Memc[bpmask[j+2]], SZ_FNAME)
		    call strcpy (Memc[skyname[j]], Memc[skyname[j+2]], SZ_FNAME)
		    call strcpy (Memc[signame[j]], Memc[signame[j+2]], SZ_FNAME)
		    call strcpy (Memc[expname[j]], Memc[expname[j+2]], SZ_FNAME)
		    call strcpy (Memc[gainname[j]],Memc[gainname[j+2]],SZ_FNAME)
		}
		call strcpy (Memc[incat[1]], Memc[incat[2]], SZ_FNAME)
		call strcpy (Memc[outcat[1]], Memc[outcat[2]], SZ_FNAME)
		call strcpy (Memc[objmask[1]], Memc[objmask[2]], SZ_FNAME)
		call strcpy (Memc[outsky[1]], Memc[outsky[2]], SZ_FNAME)
		call strcpy (Memc[outsig[1]], Memc[outsig[2]], SZ_FNAME)

		# Add extensions if needed.
		i = strldxs ("[", Memc[image[3]])
		if (imext == YES && i > 0) {
		    i = image[3]+i-1
		    call strcpy (Memc[i], Memc[str], SZ_LINE)
		    Memc[str+strldxs ("]", Memc[str])-1] = EOS
		    call strcat (",append]", Memc[str], SZ_LINE)

		    if (Memc[image[2]]!=EOS &&
		        strldxs ("[", Memc[image[2]]) == 0)
			call strcat (Memc[i], Memc[image[4]], SZ_FNAME)
		    do j = 1, 2 {
			if (Memc[bpmask[j]]!=EOS && Memc[bpmask[j]]!='!' &&
			    strldxs ("[", Memc[bpmask[j]]) == 0)
			    call strcat (Memc[i], Memc[bpmask[j+2]], SZ_FNAME)
			if (Memc[skyname[j]]!=EOS && Memc[skyname[j]]!='!' &&
			    strldxs ("[", Memc[skyname[j]]) == 0)
			    call strcat (Memc[str], Memc[skyname[j+2]],
				SZ_FNAME)
			if (Memc[signame[j]]!=EOS && Memc[signame[j]]!='!' &&
			    strldxs ("[", Memc[signame[j]]) == 0)
			    call strcat (Memc[str], Memc[signame[j+2]],
				SZ_FNAME)
			if (Memc[expname[j]]!=EOS && Memc[expname[j]]!='!' &&
			    strldxs ("[", Memc[expname[j]]) == 0)
			    call strcat (Memc[i], Memc[expname[j+2]], SZ_FNAME)
			if (Memc[gainname[j]]!=EOS && Memc[gainname[j]]!='!' &&
			    strldxs ("[", Memc[gainname[j]]) == 0)
			    call strcat (Memc[i], Memc[gainname[j+2]], SZ_FNAME)
		    }
		    if (Memc[incat[1]]!=EOS && Memc[incat[1]]!='!' &&
			strldxs ("[", Memc[incat[1]]) == 0)
			call strcat (Memc[i], Memc[incat[2]], SZ_FNAME)
		    if (Memc[outcat[1]]!=EOS && Memc[outcat[1]]!='!' &&
			strldxs ("[", Memc[outcat[1]]) == 0)
			call strcat (Memc[i], Memc[outcat[2]], SZ_FNAME)
		    if (Memc[outsky[1]]!=EOS && Memc[outsky[1]]!='!' &&
			strldxs ("[", Memc[outsky[1]]) == 0)
			call strcat (Memc[str], Memc[outsky[2]], SZ_FNAME)
		    if (Memc[outsig[1]]!=EOS && Memc[outsig[1]]!='!' &&
			strldxs ("[", Memc[outsig[1]]) == 0)
			call strcat (Memc[str], Memc[outsig[2]], SZ_FNAME)
		    if (Memc[objmask[1]]!=EOS && Memc[objmask[1]]!='!' &&
			strldxs ("[", Memc[objmask[1]]) == 0)
			call strcat (Memc[str], Memc[objmask[2]], SZ_FNAME)
		}

		# Append DATASEC.
		do i = 3, 4 {
		    if (Memc[image[i]] == EOS)
			next
		    iferr {
			im = NULL
			ptr = immap (Memc[image[i]], READ_ONLY, 0); im = ptr
			j = strlen (Memc[image[i]])
			call imgstr (im, "DATASEC", Memc[image[i]+j],
			    SZ_FNAME-j)
		    } then
			;
		    if (im != NULL)
			call imunmap (im)
		}

		# Process the image.
		call ace (par, image[3], bpmask[3], skyname[3], signame[3],
		    expname[3], gainname[3], scalestr, Memc[incat[2]],
		    Memc[outcat[2]], Memc[objmask[2]], Memc[outsky[2]],
		    Memc[outsig[2]], Memc[catdef], Memc[logfile])

	    }
	    call imtclose (list)
	}

	call sfree (sp)
end


# ACE -- Do all the primary steps for a single input image/catalog.

procedure ace (par, image, bpmask, skyname, signame, expname, gainname,
	scalestr, incat, outcat, objmask, outsky, outsig, catdef, logfile)

pointer	par			#I Parameters
pointer	image[2], bpmask[2], skyname[2], signame[2], expname[2]
pointer	gainname[2], scalestr[2]
char	incat[ARB], outcat[ARB], objmask[ARB], outsky[ARB], outsig[ARB]
char	catdef[ARB], logfile[ARB]

bool	dosky[2], dosig[2]
int	i, j, logfd, offset[2,2]
real	scale[2]
pointer	sp, bpname[2], str
pointer	im[2], bpm[2], skymap[2], sigmap[2], expmap[2], gainmap[2]
pointer	ptr, cat, om, omim, siglevmap, siglevels

bool	strne()
real	imgetr()
int	ctor(), strdic(), fnextn(), imstati()
int	open(), access(), imaccess()
pointer	immap(), xt_pmmap(), pm_open(), map_open()

errchk	open, immap, xt_pmmap, pm_newmask
errchk	cnvparse, sky, detect, split, grow, evaluate, map_open
errchk	catdefine, catopen, catgets

#pointer	bpm1, im_pmmapo()

begin
	call smark (sp)
	call salloc (bpname[1], SZ_FNAME, TY_CHAR) 
	call salloc (bpname[2], SZ_FNAME, TY_CHAR) 
	call salloc (str, SZ_LINE, TY_CHAR)

	# Deal with image types if needed.
	if (Memc[bpmask[1]] != EOS && Memc[bpmask[1]] != '!')
	    call xt_maskname (Memc[bpmask[1]], "pl", READ_ONLY, Memc[bpmask[1]],
		SZ_FNAME)
	if (Memc[bpmask[2]] != EOS && Memc[bpmask[2]] != '!')
	    call xt_maskname (Memc[bpmask[2]], "pl", READ_ONLY, Memc[bpmask[2]],
		SZ_FNAME)
	if (objmask[1] != EOS && objmask[1] != '!')
	    call xt_maskname (objmask, "pl", NEW_IMAGE, objmask, SZ_FNAME)
	if (incat[1] != EOS) {
	    i = fnextn (incat, Memc[str], SZ_LINE)
	    if (i > 0)
		i = strdic (Memc[str], Memc[str], SZ_LINE, CATEXTNS)
	    #if (i == 0)
	    #    call strcat (".fits", incat, SZ_FNAME)
	}
	if (outcat[1] != EOS) {
	    i = fnextn (outcat, Memc[str], SZ_LINE)
	    if (i > 0)
		i = strdic (Memc[str], Memc[str], SZ_LINE, CATEXTNS)
	    #if (i == 0)
	    #    call strcat (".fits", outcat, SZ_FNAME)
	}

	iferr {
	    # Initialize for error recovery.
	    do j = 1, 2 {
		im[j] = NULL; bpm[j] = NULL; skymap[j] = NULL
		sigmap[j] = NULL; expmap[j] = NULL; gainmap[j] = NULL
	    }
	    cat = NULL; logfd = NULL

	    # Log file.
	    if (logfile[1] != EOS) {
		ptr = open (logfile, APPEND, TEXT_FILE)
		logfd = ptr
		call fseti (logfd, F_FLUSHNL, YES)
	    }

	    # Open images.
	    if (PAR_DET(par) == NULL && PAR_EVL(par) == NULL)
		ptr = immap (Memc[image[1]], READ_ONLY, 0)
	    else {
		iferr (ptr = immap (Memc[image[1]], READ_WRITE, 0))
		    ptr = immap (Memc[image[1]], READ_ONLY, 0)
	    }
	    im[1] = ptr

	    # Open input catalog and object mask.
	    if (PAR_DET(par) == NULL && PAR_EVL(par) == NULL)
		;
	    else if (PAR_DET(par) == NULL) {
		if (incat[1] == EOS) {
		    call sprintf (Memc[str], SZ_LINE,
			"No input catalog for image (%s)")
			call pargstr (Memc[image[1]])
		    call error (1, Memc[str])
		} else {
		    if (access (incat, 0, 0) != YES) {
			call sprintf (Memc[str], SZ_LINE,
			    "Catalog does not exist (%s)")
			    call pargstr (incat)
			call error (1, Memc[str])
		    }
		}
		if (outcat[1]!=EOS && strne(incat,outcat)) {
		    if (access (outcat, 0, 0) == YES) {
			call sprintf (Memc[str], SZ_LINE,
			    "Catalog already exists (%s)")
			    call pargstr (outcat)
			call error (1, Memc[str])
		    }
		}
		call catopen (cat, incat, outcat, catdef)
		call catrobjs (cat, "")
		if (objmask[1] == EOS)
		    call catgets (cat, "mask", objmask, SZ_FNAME)
		omim = xt_pmmap (objmask, im[1], objmask, SZ_FNAME)
		om = imstati (omim, IM_PMDES)
	    } else {
		# Check for existing catalog.  Check catalog definitions.
		if (outcat[1] != EOS) {
		    if (access (outcat, 0, 0) == YES) {
			call sprintf (Memc[str], SZ_LINE,
			    "Catalog already exists (%s)")
			    call pargstr (outcat)
			call error (1, Memc[str])
		    }
		    call catdefine (NULL, NULL, catdef)
		}
		call catopen (cat, "", "", "")

		# Check for existing mask and initialize.
		if (objmask[1] != EOS) {
		    if (imaccess (objmask, 0) == YES) {
			call sprintf (Memc[str], SZ_LINE,
			    "Object mask already exists (%s)")
			    call pargstr (objmask)
			call error (1, Memc[str])
		    }
		}
	    }

	    # Open bad pixel mask.
	    ptr = xt_pmmap (Memc[bpmask[1]], im[1], Memc[bpname[1]],
		SZ_FNAME)
	    bpm[1] = ptr

	    # Do reference image.
	    if (Memc[image[2]] != EOS) {
#		if (Memc[bpmask[2]] == EOS)
#		    call imgimage (Memc[image[2]], Memc[image[2]], SZ_FNAME)

		iferr (ptr = immap (Memc[image[2]], READ_WRITE, 0))
		    ptr = immap (Memc[image[2]], READ_ONLY, 0)
		im[2] = ptr

		# Set offsets.
		call get_offsets (im, 2, "world", offset)
		offset[1,2] = offset[1,2] - offset[1,1]
		offset[2,2] = offset[2,2] - offset[2,1]

#		# Attempt to make an overlapping image section if
#		# there is no bad pixel mask.  This is a kludge.
#		if (Memc[bpmask[2]] == EOS) {
#		    c1 = max (1, 1-offset[1,2])
#		    c2 = min (IM_LEN(im[2],1), IM_LEN(im[1],1)-offset[1,2])
#		    l1 = max (1, 1-offset[2,2])
#		    l2 = min (IM_LEN(im[2],2), IM_LEN(im[1],2)-offset[2,2])
#		    if (c1!=1 || c2!=IM_LEN(im[2],1) ||
#			l1!=1 || l2!=IM_LEN(im[2],2)) {
#			call sprintf (Memc[str], SZ_LINE, "%s[%d:%d,%d:%d]")
#			    call pargstr (Memc[image[2]])
#			    call pargi (c1)
#			    call pargi (c2)
#			    call pargi (l1)
#			    call pargi (l2)
#			call strcpy (Memc[str], Memc[image[2]], SZ_FNAME)
#			call imunmap (im[2])
#			iferr (ptr = immap (Memc[image[2]], READ_WRITE, 0))
#			    ptr = immap (Memc[image[2]], READ_ONLY, 0)
#			im[2] = ptr
#
#			call get_offsets (im, 2, "world", offset)
#			offset[1,2] = offset[1,2] - offset[1,1]
#			offset[2,2] = offset[2,2] - offset[2,1]
#			PAR_OFFSET(par,1) = offset[1,2]
#			PAR_OFFSET(par,2) = offset[2,2]
#		    }
#		}

		ptr = xt_pmmap (Memc[bpmask[2]], im[2], Memc[bpname[2]],
		    SZ_FNAME)
		bpm[2] = ptr

		i = 1
		if (Memc[scalestr[1]] == EOS)
		    scale[1] = 1.
		else if (Memc[scalestr[1]] == '!') {
		    iferr (scale[1] = imgetr (im[1], Memc[scalestr[1]+1]))
			call error (1, "Bad scale for input image")
		} else if (ctor (Memc[scalestr[1]], i, scale[1]) == 0)
		    call error (1, "Bad scale for image")

		i = 1
		if (Memc[scalestr[2]] == EOS)
		    scale[2] = 1.
		else if (Memc[scalestr[2]] == '!') {
		    iferr (scale[2] = imgetr (im[2], Memc[scalestr[2]+1]))
			call error (1, "Bad scale for reference image")
		} else if (ctor (Memc[scalestr[2]], i, scale[2]) == 0)
		    call error (1, "Bad scale for reference image")
	    }

	    if (logfd != NULL) {
		call sysid (Memc[str], SZ_LINE)
		call fprintf (logfd, "ACE: %s\n")
		    call pargstr (Memc[str])
		call fprintf (logfd, "  Image: %s - %s\n")
		    call pargstr (Memc[image[1]])
		    call pargstr (IM_TITLE(im[1]))
		if (bpm[1] != NULL) {
		    call fprintf (logfd, "  Bad pixel mask: %s\n")
			call pargstr (Memc[bpname[1]])
		}
		if (im[2] != EOS) {
		    call fprintf (logfd, "  Reference image: %s - %s\n")
			call pargstr (Memc[image[2]])
			call pargstr (IM_TITLE(im[2]))
		    if (bpm[2] != NULL) {
			call fprintf (logfd,
			    "  Reference bad pixel mask: %s\n")
			    call pargstr (Memc[bpname[2]])
		    }
		}
	    }

	    # Open optional maps.
	    do j = 1, 2 {
		if (im[j] == NULL)
		    next
		if (Memc[expname[j]] != EOS)
		    expmap[j] = map_open (Memc[expname[j]], im[j])
	    }
	    do j = 1, 2 {
		if (im[j] == NULL)
		    next
		if (Memc[gainname[j]] != EOS)
		    gainmap[j] = map_open (Memc[gainname[j]], im[j])
	    }

	    # Get sky and sky sigma.
	    do j = 1, 2 {
		dosky[j] = false
		dosig[j] = false
		if (im[j] == NULL)
		    next
		if (PAR_SKY(par) == NULL) {
		    if (Memc[skyname[j]] != EOS)
			skymap[j] = map_open (Memc[skyname[j]], im[j])
		    if (Memc[signame[j]] != EOS)
			sigmap[j] = map_open (Memc[signame[j]], im[j])
		} else {
		    if (j == 1 && om != NULL)
			call sky (PAR_SKY(par), im[j], omim, expmap[j],
			    Memc[skyname[j]], Memc[signame[j]],
			    skymap[j], sigmap[j], dosky[j], dosig[j], logfd)
		    else
			call sky (PAR_SKY(par), im[j], bpm[j], expmap[j],
			    Memc[skyname[j]], Memc[signame[j]],
			    skymap[j], sigmap[j], dosky[j], dosig[j], logfd)
		}
		if (skymap[j] != NULL)
		    call map_seti (skymap[j], "sample", 5)
		if (sigmap[j] != NULL)
		    call map_seti (sigmap[j], "sample", 5)
	    }

	    # Detect objects.
	    if (PAR_DET(par) != NULL) {
		# Open object mask.
		om = pm_open (NULL)
		call pm_ssize (om, IM_NDIM(im[1]), IM_LEN(im[1],1), 27)

		# Initialize splitting map if needed.
		if (PAR_SPT(par) != NULL) {
		    siglevmap = pm_open (NULL)
		    call pm_ssize (siglevmap, IM_NDIM(im[1]),
			IM_LEN(im[1],1), 27)
		} else
		    siglevmap = NULL

		# Detect objects.
		call detect (PAR_DET(par), PAR_SPT(par), dosky, dosig,
		    Memc[skyname[1]], Memc[signame[1]], im, bpm, skymap,
		    sigmap, expmap, scale, offset[1,2], om, siglevmap,
		    siglevels, logfd, cat)

		# Split objects.
		if (PAR_SPT(par) != NULL)
		    call split (PAR_SPT(par), cat, om, siglevmap,
			Memr[siglevels], logfd)

		# Grow objects.
		if (PAR_GRW(par) != NULL)
		    call grow (PAR_GRW(par), cat, om, logfd)

		# Set boundary flags and write out the object mask.
		if (objmask[1] != EOS) {
		    if (PAR_OMTYPE(par) == OM_ALL)
			call bndry (om, NULL)
		    call omwrite (om, objmask, PAR_OMTYPE(par), im[1], cat,
		    	outcat, outcat, logfd)
		}
	    }

	    # Evaluate and write out the catalog.
	    if (PAR_EVL(par) != NULL && outcat[1] != EOS) {
		if (incat[1] == EOS)
		    call catopen (cat, "", outcat, catdef)
		call catputs (cat, "image", Memc[image[1]])
		if (objmask[1] != EOS)
		    call catputs (cat, "mask", objmask)
		call catputs (cat, "catalog", outcat)
		call catputs (cat, "objid", outcat)

		# Evaluate objects.
		call evaluate (PAR_EVL(par), cat, im[1], om, skymap[1],
		    sigmap[1], gainmap[1], expmap[1], logfd)

		if (logfd != NULL) {
		    call fprintf (logfd,
			"  Write catalog: catalog = %s\n")
			call pargstr (outcat)
		}

		call catcreate (cat)
		call catwcs (cat, im)
		call catwhdr (cat, im)
		call catwobjs (cat)

		call imastr (im[1], "CATALOG", outcat)
	    }

	    # Output sky images.
	    call skyimages (outsky, outsig, im[1], skymap[1],
		sigmap[1], gainmap[1], expmap[1], logfd)

	} then
	    call erract (EA_WARN)
	    
	if (logfd != NULL)
	    call close (logfd)
	if (cat != NULL)
	    call catclose (cat)
	if (siglevmap != NULL) {
	    call pm_close (siglevmap)
	    call mfree (siglevels, TY_REAL)
	}
	if (omim != NULL) {
	    call imunmap (omim)
	    om = NULL
	} else if (om != NULL)
	    call pm_close (om)

	do j = 1, 2 {
	    if (gainmap[j] != NULL)
		call map_close (gainmap[j])
	    if (expmap[j] != NULL)
		call map_close (expmap[j])
	    if (sigmap[j] != NULL)
		call map_close (sigmap[j])
	    if (skymap[j] != NULL)
		call map_close (skymap[j])
	    if (bpm[j] != NULL)
		call imunmap (bpm[j])
	    if (im[j] != NULL)
		call imunmap (im[j])
	}

	call sfree (sp)
end


define	OFFTYPES	"|none|wcs|world|physical|"
define	FILE		0
define	NONE		1
define	WCS		2
define	WORLD		3
define	PHYSICAL	4

# GET_OFFSETS -- Get offsets.

procedure get_offsets (in, nimages, param, offsets)

pointer	in[nimages]		#I Input image pointers
int	nimages			#I Number of images
char	param[ARB]		#I Offset parameter string
int	offsets[2,nimages]	#O Offsets

int	i, j, fd, offtype, off
real	val
bool	flip, streq(), fp_equald()
pointer	sp, str, fname
pointer	pref, lref, wref, cd, ltm, coord, section
pointer	mw, ct, mw_openim(), mw_sctran(), immap()
int	open(), fscan(), nscan(), strlen(), strdic()
errchk	mw_openim, mw_gwtermd, mw_gltermd, mw_gaxmap
errchk	mw_sctran, mw_ctrand, open, immap

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (fname, SZ_LINE, TY_CHAR)
	call salloc (lref, 2, TY_DOUBLE)
	call salloc (wref, 2, TY_DOUBLE)
	call salloc (cd, 2*2, TY_DOUBLE)
	call salloc (coord, 2, TY_DOUBLE)

	call aclri (offsets, 2*nimages)

	# Parse the user offset string.  If "none" then there are no offsets.
	# If "world" or "wcs" then set the offsets based on the world WCS.
	# If "physical" then set the offsets based on the physical WCS.
	# If a file scan the offsets.

	call sscan (param)
	call gargwrd (Memc[str], SZ_LINE)
	if (nscan() == 0)
	    offtype = NONE
	else {
	    offtype = strdic (Memc[str], Memc[fname], SZ_LINE, OFFTYPES)
	    if (offtype > 0 && !streq (Memc[str], Memc[fname]))
		offtype = 0
	}
	if (offtype == 0)
	    offtype = FILE

	switch (offtype) {
	case NONE:
	    ;
	case WORLD, WCS:
	    mw = mw_openim (in[1])
	    call mw_gwtermd (mw, Memd[lref], Memd[wref], Memd[cd], 2)
	    ct = mw_sctran (mw, "world", "logical", 0)
	    call mw_ctrand (ct, Memd[wref], Memd[lref], 2)
	    call mw_close (mw)

	    do i = 2, nimages {
		mw = mw_openim (in[i])
		ct = mw_sctran (mw, "world", "logical", 0)
		call mw_ctrand (ct, Memd[wref], Memd[coord], 2)
		do j = 1, 2
		    offsets[j,i] = nint (Memd[lref+j-1] - Memd[coord+j-1])
		call mw_close (mw)
	    }
	case PHYSICAL:
	    call salloc (pref, 2, TY_DOUBLE)
	    call salloc (ltm, 4, TY_DOUBLE)
	    call salloc (section, SZ_FNAME, TY_CHAR)

	    mw = mw_openim (in[1])
	    call mw_gltermd (mw, Memd[ltm], Memd[coord], 2)
	    call mw_close (mw)
	    do i = 2, nimages {
		mw = mw_openim (in[i])
		call mw_gltermd (mw, Memd[cd], Memd[coord], 2)
		call strcpy ("[", Memc[section], SZ_FNAME)
		flip = false
		do j = 0, 3, 3 {
		    if (Memd[ltm+j] * Memd[cd+j] >= 0.)
			call strcat ("*,", Memc[section], SZ_FNAME)
		    else {
			call strcat ("-*,", Memc[section], SZ_FNAME)
			flip = true
		    }
		}
		Memc[section+strlen(Memc[section])-1] = ']'
		if (flip) {
		    call imstats (in[i], IM_IMAGENAME, Memc[fname], SZ_LINE)
		    call strcat (Memc[section], Memc[fname], SZ_LINE)
		    call imunmap (in[i])
		    in[i] = immap (Memc[fname], READ_ONLY, TY_CHAR) 
		    call mw_close (mw)
		    mw = mw_openim (in[i])
		    call mw_gltermd (mw, Memd[cd], Memd[coord], 2)
		    do j = 0, 3
			if (!fp_equald (Memd[ltm+j], Memd[cd+j]))
			    call error (1,
				"Cannot match physical coordinates")
		}
		call mw_close (mw)
	    }

	    mw = mw_openim (in[1])
	    ct = mw_sctran (mw, "logical", "physical", 0)
	    call mw_ctrand (ct, Memd[lref], Memd[pref], 2)
	    call mw_close (mw)
	    do i = 2, nimages {
		mw = mw_openim (in[i])
		ct = mw_sctran (mw, "physical", "logical", 0)
		call mw_ctrand (ct, Memd[pref], Memd[coord], 2)
		do j = 1, 2
		    offsets[j,i] = nint (Memd[lref+j-1] - Memd[coord+j-1])
		call mw_close (mw)
	    }
	case FILE:
	    fd = open (Memc[str], READ_ONLY, TEXT_FILE)
	    i = 1
	    while (fscan (fd) != EOF) {
		do j = 1, 2 {
		    call gargr (val)
		    offsets[j,i] = nint (val)
		}
		if (nscan() == 2)
		    i = i + 1
	    }
	    call close (fd)
	    if (i <= nimages)
		call error (1, "offset file incomplete")
	}

	# Adjust offsets to be positive.
	do j = 1, 2 {
	    off =  offsets[j,1]
	    do i = 2, nimages
		off = min (off, offsets[j,i])
	    do i = 1, nimages
		offsets[j,i] = offsets[j,i] - off
	}

	call sfree (sp)
end
