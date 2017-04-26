include	<imhdr.h>
include	<imio.h>
include	<error.h>
include	"ccdtypes.h"

define	HELP1	"noao$imred/ccdred/src/ccdinst1.key"
define	HELP2	"noao$imred/ccdred/src/ccdinst2.key"
define	HELP3	"noao$imred/ccdred/src/ccdinst3.key"

define	LEVELS	"|basic|common|all|"

define	CMDS	"|quit|?|help|show|instrument|imheader|read|write|newimage\
		 |translate|imagetyp|subset|exptime|darktime|fixfile|biassec\
		 |ccdsec|datasec|trimsec|darkcor|fixpix|flatcor|fringcor\
		 |illumcor|overscan|readcor|scancor|trim|zerocor|ccdmean\
		 |fringscl|illumflt|mkfringe|mkillum|skyflat|ncombine\
		 |date-obs|dec|ra|title|next|nscanrow|"

define	QUIT		1	# Quit
define	QUESTION	2	# Help
define	HELP		3	# Help
define	SHOW		4	# Show current translations
define	INST		5	# Show instrument file
define	IMHEADER	6	# Print image header
define	READ		7	# Read instrument file
define	WRITE		8	# Write instrument file
define	NEWIMAGE	9	# Change image
define	TRANSLATE	10	# Translate image type
define	IMAGETYPE	11	# Image type
define	SUBSET		12	# Subset parameter
define	EXPTIME		13	# Exposure time
define	DARKTIME	14	# Dark time
define	FIXFILE		15	# Bad pixel file
define	BIASSEC		16	# Bias section
define	CCDSEC		17	# CCD section
define	DATASEC		18	# Data section
define	TRIMSEC		19	# Trim section
define	DARKCOR		20	# Dark count flag
define	FIXPIX		21	# Bad pixel flag
define	FLATCOR		22	# Flat field flag
define	FRINGCOR	23	# Fringe flag
define	ILLUMCOR	24	# Illumination flag
define	OVERSCAN	25	# Overscan flag
define	READCOR		26	# Readout flag
define	SCANCOR		27	# Scan mode flag
define	NSCANROW	42	# Number of scan rows
define	TRIM		28	# Trim flag
define	ZEROCOR		29	# Zero level flag
define	CCDMEAN		30	# CCD mean value
define	FRINGSCL	31	# Fringe scale value
define	ILLUMFLT	32	# Illumination flat flag
define	MKFRINGE	33	# Illumination flag
define	MKILLUM		34	# Illumination flag
define	SKYFLAT		35	# Sky flat flag
define	NCOMBINE	36	# NCOMBINE parameter
define	DATEOBS		37	# Date
define	DEC		38	# Dec
define	RA		39	# RA
define	TITLE		40	# Title
define	NEXT		41	# Next image

# T_CCDINST -- Check and modify instrument translations

procedure t_ccdinst ()

int	list, level, ncmd, imtopenp(), imtgetim(), scan(), access(), clgwrd()
pointer	sp, image, inst, ssfile, im, immap()
bool	update, clgetb()
errchk	delete, hdmwrite

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (inst, SZ_FNAME, TY_CHAR)
	call salloc (ssfile, SZ_FNAME, TY_CHAR)

	# Get the task parameters, open the translation file, set defaults.
	list = imtopenp ("images")
	call clgstr ("instrument", Memc[inst], SZ_FNAME)
	call clgstr ("ssfile", Memc[ssfile], SZ_FNAME)
	level = clgwrd ("parameters", Memc[image], SZ_FNAME, LEVELS)
	call hdmopen (Memc[inst])
	ncmd = NEXT
	update = false

	# Process each image.
	while (imtgetim (list, Memc[image], SZ_FNAME) != EOF) {
	    iferr (im = immap (Memc[image], READ_ONLY, 0)) {
		call erract (EA_WARN)
		next
	    }

	    if (clgetb ("edit"))
	        call ccdinst_edit (im, Memc[image], Memc[inst], Memc[ssfile],
		    level, ncmd, update)
	    else
		call ccdinst_hdr (im, Memc[image], Memc[inst], Memc[ssfile],
		    level)
	    call imunmap (im)
	    if (ncmd == QUIT)
		break
	}

	# Update instrument file if necessary.
	if (update) {
	    call printf ("Update instrument file %s (%b)? ")
		call pargstr (Memc[inst])
		call pargb (update)
	    call flush (STDOUT)
	    if (scan() != EOF)
		call gargb (update)
	    if (update) {
		iferr {
		    if (access (Memc[inst], 0, 0) == YES)
		        call delete (Memc[inst])
		    call hdmwrite (Memc[inst], NEW_FILE)
		} then
		    call erract (EA_WARN)
	    }
	}

	# Finish up.
	call hdmclose ()
	call imtclose (list)
	call sfree (sp)
end


# CCDINST_EDIT -- Main instrument file editor loop.
# This returns the last command (quit or next) and the update flag.
# The image name may also be changed.

procedure ccdinst_edit (im, image, inst, ssfile, level, ncmd, update)

pointer	im			# Image pointer
char	image[SZ_FNAME]		# Image name
char	inst[SZ_FNAME]		# Instrument file
char	ssfile[SZ_FNAME]	# Subset file
int	level			# Parameter level
int	ncmd			# Last command
bool	update			# Update?

bool	strne()
int	scan(), nscan(), strdic(), access()
pointer	sp, cmd, key, def, imval, im1, immap()
errchk	delete, hdmwrite

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call salloc (key, SZ_FNAME, TY_CHAR)
	call salloc (def, SZ_LINE, TY_CHAR)
	call salloc (imval, SZ_LINE, TY_CHAR)

	call sscan ("show")
	repeat {
	    call gargwrd (Memc[cmd], SZ_LINE)
	    ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, CMDS)
	    switch (ncmd) {
	    case NEXT, QUIT:
		break
	    case QUESTION, HELP:
		if (level == 1)
		    call pagefile (HELP1, "ccdinstrument")
		else if (level == 2)
		    call pagefile (HELP2, "ccdinstrument")
		else if (level == 3)
		    call pagefile (HELP3, "ccdinstrument")
	    case SHOW:
		call ccdinst_hdr (im, image, inst, ssfile, level)
	    case INST:
		call hdmwrite ("STDOUT", APPEND)
		call printf ("\n")
	    case IMHEADER:
		call ccdinst_i (im, image)
	    case READ:
		call gargwrd (Memc[imval], SZ_LINE)
		if (nscan() < 2) 
		    call ccdinst_g ("Instrument file", inst, Memc[imval])
		if (update)
		    call printf ("WARNING: Previous changes lost\n")
		call hdmclose ()
		update = false
		if (strne (inst, Memc[imval])) {
		    iferr (call hdmopen (Memc[imval])) {
			call erract (EA_WARN)
			call hdmopen (inst)
		    } else {
			call ccdinst_hdr (im, image, inst, ssfile, level)
			update = true
		    }
		}
	    case WRITE:
		call gargwrd (Memc[imval], SZ_LINE)
		if (nscan() < 2)
		    call ccdinst_g ("Instrument file", inst, Memc[imval])
		iferr {
		    if (access (Memc[imval], 0, 0) == YES)
		        call delete (Memc[imval])
		    call hdmwrite (Memc[imval], NEW_FILE)
		    update = false
		} then
		    call erract (EA_WARN)
	    case NEWIMAGE:
		call gargwrd (Memc[imval], SZ_LINE)
		if (nscan() < 2)
		    call ccdinst_g ("New image name", image, Memc[imval])
		if (strne (image, Memc[imval])) {
		    iferr (im1 = immap (Memc[imval], READ_ONLY, 0)) {
			call erract (EA_WARN)
			im1 = NULL
		    }
		    if (im1 != NULL) {
			call imunmap (im)
			im = im1
			call strcpy (Memc[imval], image, SZ_FNAME)
			call ccdinst_hdr (im, image, inst, ssfile, level)
		    }
		}
	    case TRANSLATE:
		call ccdtypes (im, Memc[cmd], SZ_LINE)
		call hdmgstr (im, "imagetyp", Memc[imval], SZ_LINE)

		call gargwrd (Memc[def], SZ_FNAME)
		if (nscan() < 2) {
		    call printf ("CCDRED image type for '%s' (%s): ")
			call pargstr (Memc[imval])
			call pargstr (Memc[cmd])
		    call flush (STDOUT)
		    if (scan() != EOF)
			call gargwrd (Memc[def], SZ_FNAME)
		    if (nscan() == 0)
			call strcpy (Memc[cmd], Memc[def], SZ_LINE)
		}
		if (strdic (Memc[def], Memc[def], SZ_LINE, CCDTYPES) == 0) {
		    call printf ("Unknown CCDRED image type\n")
		    call strcpy (Memc[cmd], Memc[def], SZ_LINE)
		}
		if (strne (Memc[def], Memc[cmd])) {
		    call hdmpname (Memc[imval], Memc[def])
		    call ccdinst_p (im, "imagetyp",
			Memc[key], Memc[def], Memc[imval])
		    update = true
		}
	    case IMAGETYPE:
		call ccdinst_e (im, "image type", "imagetyp",
		    Memc[key], Memc[def], Memc[imval], update)
	    case SUBSET:
		call ccdinst_e (im, "subset parameter", "subset",
		    Memc[key], Memc[def], Memc[imval], update)
	    case EXPTIME:
		call ccdinst_e (im, "exposure time", "exptime",
		    Memc[key], Memc[def], Memc[imval], update)
	    case DARKTIME:
		call ccdinst_e (im, "dark time", "darktime",
		    Memc[key], Memc[def], Memc[imval], update)
	    case FIXFILE:
		call ccdinst_e (im, "bad pixel file", "fixfile",
		    Memc[key], Memc[def], Memc[imval], update)
	    case BIASSEC:
		call ccdinst_e (im, "bias section", "biassec",
		    Memc[key], Memc[def], Memc[imval], update)
	    case CCDSEC:
		call ccdinst_e (im, "original CCD section", "ccdsec",
		    Memc[key], Memc[def], Memc[imval], update)
	    case DATASEC:
		call ccdinst_e (im, "data section", "datasec",
		    Memc[key], Memc[def], Memc[imval], update)
	    case TRIMSEC:
		call ccdinst_e (im, "trim section", "trimsec",
		    Memc[key], Memc[def], Memc[imval], update)
	    case DARKCOR:
		call ccdinst_e (im, "dark count flag", "darkcor",
		    Memc[key], Memc[def], Memc[imval], update)
	    case FIXPIX:
		call ccdinst_e (im, "bad pixel flag", "fixpix",
		    Memc[key], Memc[def], Memc[imval], update)
	    case FLATCOR:
		call ccdinst_e (im, "flat field flag", "flatcor",
		    Memc[key], Memc[def], Memc[imval], update)
	    case FRINGCOR:
		call ccdinst_e (im, "fringe flag", "fringcor",
		    Memc[key], Memc[def], Memc[imval], update)
	    case ILLUMCOR:
		call ccdinst_e (im, "illumination flag", "illumcor",
		    Memc[key], Memc[def], Memc[imval], update)
	    case OVERSCAN:
		call ccdinst_e (im, "overscan flag", "overscan",
		    Memc[key], Memc[def], Memc[imval], update)
	    case READCOR:
		call ccdinst_e (im, "read correction flag", "readcor",
		    Memc[key], Memc[def], Memc[imval], update)
	    case SCANCOR:
		call ccdinst_e (im, "scan mode flag", "scancor",
		    Memc[key], Memc[def], Memc[imval], update)
	    case NSCANROW:
		call ccdinst_e (im, "scan mode rows", "nscanrow",
		    Memc[key], Memc[def], Memc[imval], update)
	    case TRIM:
		call ccdinst_e (im, "trim flag", "trim",
		    Memc[key], Memc[def], Memc[imval], update)
	    case ZEROCOR:
		call ccdinst_e (im, "zero level flag", "zerocor",
		    Memc[key], Memc[def], Memc[imval], update)
	    case CCDMEAN:
		call ccdinst_e (im, "mean value", "ccdmean",
		    Memc[key], Memc[def], Memc[imval], update)
	    case FRINGSCL:
		call ccdinst_e (im, "fringe scale", "fringscl",
		    Memc[key], Memc[def], Memc[imval], update)
	    case ILLUMFLT:
		call ccdinst_e (im, "illumination flat image", "illumflt",
		    Memc[key], Memc[def], Memc[imval], update)
	    case MKFRINGE:
		call ccdinst_e (im, "fringe image", "mkfringe",
		    Memc[key], Memc[def], Memc[imval], update)
	    case MKILLUM:
		call ccdinst_e (im, "illumination image", "mkillum",
		    Memc[key], Memc[def], Memc[imval], update)
	    case SKYFLAT:
		call ccdinst_e (im, "sky flat image", "skyflat",
		    Memc[key], Memc[def], Memc[imval], update)
	    case NCOMBINE:
		call ccdinst_e (im, "number of images combined", "ncombine",
		    Memc[key], Memc[def], Memc[imval], update)
	    case DATEOBS:
		call ccdinst_e (im, "date of observation", "date-obs",
		    Memc[key], Memc[def], Memc[imval], update)
	    case DEC:
		call ccdinst_e (im, "declination", "dec",
		    Memc[key], Memc[def], Memc[imval], update)
	    case RA:
		call ccdinst_e (im, "ra", "ra",
		    Memc[key], Memc[def], Memc[imval], update)
	    case TITLE:
		call ccdinst_e (im, "title", "title",
		    Memc[key], Memc[def], Memc[imval], update)
	    default:
	        if (nscan() > 0)
		    call eprintf ("Unrecognized or ambiguous command\007\n")
	    }
	    call printf ("ccdinstrument> ")
	    call flush (STDOUT)
	} until (scan() == EOF)

	call sfree (sp)
end


# CCDINST_HDR -- Print the current instrument translations for an image.

procedure ccdinst_hdr (im, image, inst, ssfile, level)

pointer	im			# Image pointer
char	image[SZ_FNAME]		# Image name
char	inst[SZ_FNAME]		# Instrument file
char	ssfile[SZ_FNAME]	# Subset file
int	level			# Parameter level

pointer	sp, key, def, ccdval, imval

begin
	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)
	call salloc (def, SZ_LINE, TY_CHAR)
	call salloc (ccdval, SZ_LINE, TY_CHAR)
	call salloc (imval, SZ_LINE, TY_CHAR)

	# General stuff
	call printf ("Image: %s\n")
	    call pargstr (image)
	call printf ("Instrument file: %s\n")
	    call pargstr (inst)
	call printf ("Subset file: %s\n")
	    call pargstr (ssfile)

	# Table labels
	call printf ("\n%-8s  %-8s  %-8s  %-8s  %-8s\n")
	    call pargstr ("CCDRED")
	    call pargstr ("IMAGE")
	    call pargstr ("DEFAULT")
	    call pargstr ("CCDRED")
	    call pargstr ("IMAGE")
	call printf ("%-8s  %-8s  %-8s  %-8s  %-8s\n")
	    call pargstr ("PARAM")
	    call pargstr ("KEYWORD")
	    call pargstr ("VALUE")
	    call pargstr ("VALUE")
	    call pargstr ("VALUE")
	call printf ("---------------------------------------")
	call printf ("---------------------------------------\n")

	# Print translations.  Select those printed only with the all parameter.
	call ccdinst_p (im, "imagetyp", Memc[key], Memc[def], Memc[imval])
	call ccdinst_p (im, "subset", Memc[key], Memc[def], Memc[imval])
	call ccdinst_p (im, "exptime", Memc[key], Memc[def], Memc[imval])
	call ccdinst_p (im, "darktime", Memc[key], Memc[def], Memc[imval])
	if (level > 1) {
	    call printf ("\n")
	    call ccdinst_p (im, "biassec", Memc[key], Memc[def], Memc[imval])
	    call ccdinst_p (im, "trimsec", Memc[key], Memc[def], Memc[imval])
	    call printf ("\n")
	    call ccdinst_p (im, "fixpix", Memc[key], Memc[def], Memc[imval])
	    call ccdinst_p (im, "overscan", Memc[key], Memc[def], Memc[imval])
	    call ccdinst_p (im, "trim", Memc[key], Memc[def], Memc[imval])
	    call ccdinst_p (im, "zerocor", Memc[key], Memc[def], Memc[imval])
	    call ccdinst_p (im, "darkcor", Memc[key], Memc[def], Memc[imval])
	    call ccdinst_p (im, "flatcor", Memc[key], Memc[def], Memc[imval])
	}
	if (level > 2) {
	    call ccdinst_p (im, "datasec", Memc[key], Memc[def], Memc[imval])
	    call ccdinst_p (im, "ccdsec", Memc[key], Memc[def], Memc[imval])
	    call ccdinst_p (im, "fixfile", Memc[key], Memc[def], Memc[imval])
	    call printf ("\n")
	    call ccdinst_p (im, "illumcor", Memc[key], Memc[def], Memc[imval])
	    call ccdinst_p (im, "fringcor", Memc[key], Memc[def], Memc[imval])
	    call ccdinst_p (im, "readcor", Memc[key], Memc[def], Memc[imval])
	    call ccdinst_p (im, "scancor", Memc[key], Memc[def], Memc[imval])
	    call ccdinst_p (im, "nscanrow", Memc[key], Memc[def], Memc[imval])
	    call printf ("\n")
	    call ccdinst_p (im, "illumflt", Memc[key], Memc[def], Memc[imval])
	    call ccdinst_p (im, "mkfringe", Memc[key], Memc[def], Memc[imval])
	    call ccdinst_p (im, "mkillum", Memc[key], Memc[def], Memc[imval])
	    call ccdinst_p (im, "skyflat", Memc[key], Memc[def], Memc[imval])
	    call printf ("\n")
	    call ccdinst_p (im, "ccdmean", Memc[key], Memc[def], Memc[imval])
	    call ccdinst_p (im, "fringscl", Memc[key], Memc[def], Memc[imval])
	    call ccdinst_p (im, "ncombine", Memc[key], Memc[def], Memc[imval])
	    call ccdinst_p (im, "date-obs", Memc[key], Memc[def], Memc[imval])
	    call ccdinst_p (im, "dec", Memc[key], Memc[def], Memc[imval])
	    call ccdinst_p (im, "ra", Memc[key], Memc[def], Memc[imval])
	    call ccdinst_p (im, "title", Memc[key], Memc[def], Memc[imval])
	}

	call printf ("\n")
	call flush (STDOUT)
	call sfree (sp)
end


# CCDINST_P -- Print the translation for the specified translation name.

procedure ccdinst_p (im, name, key, def, value)

pointer	im			# Image pointer
char	name[SZ_FNAME]		# CCDRED name
char	key[SZ_FNAME]		# Image header keyword
char	def[SZ_LINE]		# Default value
char	value[SZ_LINE]		# Value

int	i, strdic(), hdmaccf()
bool	bval, ccdflag()

begin
	i = strdic (name, key, SZ_FNAME, CMDS)
	if (i == 0)
	    return

	# Get translaltion image keyword, default, and image value.
	call hdmname (name, key, SZ_FNAME)
	call hdmgdef (name, def, SZ_LINE)
	call hdmgstr (im, name, value, SZ_LINE)
	if (value[1] == EOS)
	    call strcpy ("?", value, SZ_LINE)

	switch (i) {
	case IMAGETYPE: 
	    call printf ("%-8s  %-8s  %-8s")  
		call pargstr (name)
		call pargstr (key)
		call pargstr (def)
	    call ccdtypes (im, def, SZ_LINE)
	    call printf ("  %-8s  %-.39s\n")  
		call pargstr (def)
		call pargstr (value)
	case SUBSET: 
	    call printf ("%-8s  %-8s  %-8s")  
		call pargstr (name)
		call pargstr (key)
		call pargstr (def)
	    call ccdsubset (im, def, SZ_LINE)
	    call printf ("  %-8s  %-.39s\n")  
		call pargstr (def)
		call pargstr (value)
	case FIXPIX, OVERSCAN, TRIM, ZEROCOR, DARKCOR, FLATCOR, ILLUMCOR,
	     FRINGCOR, READCOR, SCANCOR, ILLUMFLT, MKFRINGE, MKILLUM,
	     SKYFLAT:
	    bval = ccdflag (im, name)
	    if (hdmaccf (im, name) == NO)
		call strcpy ("?", value, SZ_LINE)
	    call printf ("%-8s  %-8s  %-8s  %-8b  %-.39s\n")  
		call pargstr (name)
		call pargstr (key)
		call pargstr (def)
		call pargb (bval)
		call pargstr (value)
	default:
	    call printf ("%-8s  %-8s  %-8s  %-8s")  
		call pargstr (name)
		call pargstr (key)
		call pargstr (def)
		call pargstr (value)
	    if (hdmaccf (im, name) == NO)
		call strcpy ("?", value, SZ_LINE)
	    call printf ("  %-.39s\n")  
		call pargstr (value)
	}
end


# CCDINST_E -- Edit a single translation entry.
# This checks for parameters on the command line and if missing queries.
# The default value may only be changed on the command line.

procedure ccdinst_e (im, prompt, name, key, def, imval, update)

pointer	im			# Image pointer
char	prompt[ARB]		# Parameter prompt name
char	name[SZ_FNAME]		# CCDRED name
char	key[SZ_FNAME]		# Image header keyword
char	def[SZ_LINE]		# Default value
char	imval[SZ_LINE]		# Value
bool	update			# Update translation file?

bool	strne()
int	i, scan(), nscan()
pointer	sp, oldkey, olddef

begin
	    call smark (sp)
	    call salloc (oldkey, SZ_FNAME, TY_CHAR)
	    call salloc (olddef, SZ_LINE, TY_CHAR)

	    # Get command line values
	    call gargwrd (key, SZ_FNAME)
	    call gargwrd (def, SZ_LINE)

	    # Get current values
	    call hdmname (name, Memc[oldkey], SZ_FNAME)
	    call hdmgdef (name, Memc[olddef], SZ_LINE)

	    # Query for keyword if needed.
	    i = nscan()
	    if (i < 2) {
		call printf ("Image keyword for %s (%s): ")
		    call pargstr (prompt)
		    call pargstr (Memc[oldkey])
		call flush (STDOUT)
		if (scan() != EOF)
		    call gargwrd (key, SZ_FNAME)
		if (nscan() == 0)
		    call strcpy (Memc[oldkey], key, SZ_FNAME)
	    }
	    if (i < 3) {
		#call printf ("Default %s (%s): ")
		#    call pargstr (prompt)
		#    call pargstr (Memc[olddef])
		#call flush (STDOUT)
		#if (scan() != EOF)
		#    call gargwrd (def, SZ_LINE)
		#if (nscan() == 0)
		    call strcpy (Memc[olddef], def, SZ_LINE)
	    }

	    # Update only if the new value is different from the old value.
	    if (strne (key, Memc[oldkey])) { 
	        call hdmpname (name, key)
		update = true
	    }
	    if (strne (def, Memc[olddef])) {
		call hdmpdef (name, def)
		update = true
	    }

	    # Print the revised translation.
	    call ccdinst_p (im, name, key, def, imval)
	    call sfree (sp)
end


# CCDINST_G -- General procedure to prompt for value.

procedure ccdinst_g (prompt, def, val)

char	prompt[ARB]		# Prompt
char	def[ARB]		# Default value
char	val[SZ_LINE]		# Value

int	scan(), nscan()

begin
	call printf ("%s (%s): ")
	    call pargstr (prompt)
	    call pargstr (def)
	call flush (STDOUT)
	if (scan() != EOF)
	    call gargwrd (val, SZ_FNAME)
	if (nscan() == 0)
	    call strcpy (def, val, SZ_LINE)
end


define	USER_AREA	Memc[($1+IMU-1)*SZ_STRUCT + 1]

# CCDINST_IMH -- Print the user area of the image, if nonzero length
# and it contains only ascii values.  This copied from the code for
# IMHEADER.  It differs in including the OBJECT keyword, using a temporary
# file to page the header, and no leading blanks.

procedure ccdinst_i (im, image)

pointer	im			# image descriptor
char	image[ARB]		# image name

pointer	sp, tmp, lbuf, ip
int	in, out, ncols, min_lenuserarea 
int	open(), stropen(), getline(), envgeti()

begin
	call smark (sp)
	call salloc (tmp, SZ_FNAME, TY_CHAR)
	call salloc (lbuf, SZ_LINE, TY_CHAR)

	# Open user area in header.
	min_lenuserarea = (LEN_IMDES + IM_LENHDRMEM(im) - IMU) * SZ_STRUCT - 1
	in = stropen (USER_AREA(im), min_lenuserarea, READ_ONLY)
	ncols = envgeti ("ttyncols")

	# Open temporary output file.
	call mktemp ("tmp$", Memc[tmp], SZ_FNAME)
	iferr (out = open (Memc[tmp], NEW_FILE, TEXT_FILE)) {
	    call erract (EA_WARN)
	    call sfree (sp)
	    return
	}

	# Copy standard header records.
	call fprintf (out, "OBJECT  = '%s'\n")
	    call pargstr (IM_TITLE(im))

	# Copy header records to the output, stripping any trailing
	# whitespace and clipping at the right margin.

	while (getline (in, Memc[lbuf]) != EOF) {
	    for (ip=lbuf;  Memc[ip] != EOS && Memc[ip] != '\n';  ip=ip+1)
		;
	    while (ip > lbuf && Memc[ip-1] == ' ')
		ip = ip - 1
	    if (ip - lbuf > ncols)
		ip = lbuf + ncols 
	    Memc[ip] = '\n'
	    Memc[ip+1] = EOS
	    
	    call putline (out, Memc[lbuf])
	}
	call putline (out, "\n")

	call close (in)
	call close (out)

	call pagefile (Memc[tmp], image)
	call delete (Memc[tmp])

	call sfree (sp)
end
